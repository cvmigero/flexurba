#' Preprocess the data for the DEGURBA spatial units classification
#'
#' @description
#' The spatial units classification of the Degree of Urbanisation requires three different inputs (all input sources should be in the Mollweide coordinate system):
#' - a vector layer with the small spatial units
#' - a raster layer with the grid cell classification of the Degree of Urbanisation
#' - a raster layer with the population grid
#'
#' The three input layers are pre-processed as follows. The classification grid and population grid are resampled to the `resample_resolution` with the nearest neighbor algorithm. In doing this, the values of the population grid are divided by the oversampling ratio (for example: going from a resolution of 100 m to a resolution of 50 m, the values of the grid are divided by 4).
#'
#' In addition, the function makes sure the extents of the three input layers match. If the bounding box of the units layer is smaller than the extent of the grids, then the grids are cropped to the bounding box of the units layer. Alternatively, if the units layer covers a larger area than the grids, then the units that do not intersect with the grids are discarded (and a warning message is printed). This ensures that the classification algorithm runs efficiently and does not generate any incorrect classifications due to missing data.
#'
#' More information about the pre-processing workflow, see [GHSL Data Package 2023 (Section 2.7.2.3)](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf).
#' @param units character / object of class `sf`. Path to the vector layer with small spatial units, or an object of class `sf` with the small spatial units
#' @param classification character / SpatRaster. Path to the grid cell classification of the Degree of Urbanisation, or SpatRaster with the grid cell classification
#' @param pop character / SpatRaster. Path to the population grid, or SpatRaster with the population grid
#' @param resample_resolution numeric. Resolution to which the grids are resampled during pre-processing. If `NULL`, the grids are resampled to the smallest resolution among the population and classification grid.
#' @param dissolve_units_by character. If not `NULL`, the units are dissolved by this column's value, can for example be used to dissolve spatial units to a certain GADM level (see examples).
#' @return named list with the required data to execute the spatial units classification procedure, and their metadata. The list contains the following elements:
#' - `classification`: the (resampled and cropped) grid cell classification layer
#' - `pop`: the (resampled and cropped) population grid
#' - `units`: the (dissolved and filtered) spatial units (object of class `sf`)
#' - `metadata`: named list with the metadata of the input files. It contains the elements `units`, `classification` and `pop` (with paths to the respective data sources), `resample_resolution` and `dissolve_units_by` if not `NULL`. (Note that when the input sources are passed by object , the metadata might be empty).
#' @examples
#' \dontrun{
#' # load the grid data
#' grid_data <- flexurba::DoU_load_grid_data_belgium()
#' # load the units and filter for West-Flanders
#' units_data <- flexurba::units_belgium %>%
#'   dplyr::filter(GID_2 == "BEL.2.5_1")
#' # classify the grid
#' classification <- classify_grid(data = grid_data)
#'
#' # preprocess the data for units classification
#' data1 <- DoU_preprocess_units(
#'   units = units_data,
#'   classification = classification,
#'   pop = grid_data$pop,
#'   resample_resolution = 50
#' )
#'
#' # preprocess the data for units classification at GADM level 3 (Belgian districts)
#' data2 <- DoU_preprocess_units(
#'   units = units_data,
#'   classification = classification,
#'   pop = grid_data$pop,
#'   resample_resolution = 50,
#'   dissolve_units_by = "GID_3"
#' )
#' }
#' @export
DoU_preprocess_units <- function(units, classification, pop, resample_resolution = NULL, dissolve_units_by = NULL) {
  # create the output object
  data <- list()
  data$metadata <- list()
  
  # read the data of the units
  if (!inherits(units, "sf")) {
    if (!is.character(units)) {
      stop('Invalid argument: units should be the path to a vector layer with small spatial units, or an object of class "sf"')
    }
    data$metadata$units <- units
    units <- sf::st_read(units, quiet = TRUE)
  } else {
    data$metadata$units <- ""
  }
  
  # read the data of the classification
  if (!inherits(classification, "SpatRaster")) {
    if (!is.character(classification)) {
      stop("Invalid argument: units should be the path to a raster layer with the grid cell classification, or a SpatRaster")
    }
    classification <- terra::rast(classification)
  }
  data$metadata$classification <- terra::sources(classification)
  
  # read the data of the population
  if (!inherits(pop, "SpatRaster")) {
    if (!is.character(pop)) {
      stop("Invalid argument: units should be the path to a raster layer with the population grid, or a SpatRaster")
    }
    pop <- terra::rast(pop)
  }
  data$metadata$pop <- terra::sources(pop)
  data$metadata$dissolve_units_by <- dissolve_units_by
  
  # check resolution and coordinate system
  grids <- list(classification, pop)
  gridnames <- list("classification grid", "population grid")
  if (is.null(resample_resolution)) {
    resample_resolution <- min(terra::res(pop), terra::res(classification))
    data$metadata$resample_resolution <- resample_resolution
  }
  for (i in 1:length(grids)) {
    if ((terra::res(grids[[i]])[1] %% resample_resolution) != 0) {
      stop(paste("The resolution of the", gridnames[[i]], "is not a multiple of the resample_resolution"))
    }
    
    if (terra::crs(grids[[i]], proj = TRUE) != terra::crs("ESRI:54009", proj = TRUE)) {
      stop(paste("The coordinate system of the", gridnames[[i]], "is not Mollweide"))
    }
  }
  if (sf::st_crs(units) != sf::st_crs("ESRI:54009")) {
    stop("The coordinate system of the spatial units layer is not Mollweide")
  }
  
  # oversample classification grid
  oversample_factor <- terra::res(classification)[1] / resample_resolution
  if (oversample_factor != 1) {
    classification <- terra::disagg(classification, oversample_factor, method = "near")
  }
  
  # oversample population grid (and divide value by oversample-factor)
  oversample_factor <- terra::res(pop)[1] / resample_resolution
  if (oversample_factor != 1) {
    pop <- terra::disagg(pop, oversample_factor, method = "near") / (oversample_factor * oversample_factor)
  }
  
  # crop classification and pop to smallest extent
  l <- crop_to_smallest_extent(classification, pop)
  data$classification <- l[[1]]
  data$pop <- l[[2]]
  
  # crop grids on the bounding box of the units layer
  data$classification <- terra::crop(data$classification, terra::ext(sf::st_bbox(units)))
  data$pop <- terra::crop(data$pop, terra::ext(sf::st_bbox(units)))
  
  sf::st_geometry(units) <- "geometry"
  
  # global variable
  .data <- NULL
  
  # dissolve units by group
  if (!is.null(dissolve_units_by)) {
    if (dissolve_units_by %in% names(units)) {
      units <- units %>%
        dplyr::group_by(.data[[dissolve_units_by]]) %>%
        dplyr::summarize(geometry = sf::st_union(.data[["geometry"]]))
    } else {
      stop(paste("Invalid parameter:", dissolve_units_by, "is not a column in the units data."))
    }
  }
  
  # only remain units that intersect with the extent of the grids
  data$units <- filter_units_on_extent(units, data$pop)
  if (nrow(data$units) != nrow(units)) {
    warning("Units which not intersect with the population and classification grid are discarded. \n")
  }
  
  return(data)
}


#' Preprocess the data for the DEGURBA spatial units classification
#' 
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' `preprocess_grid()` has been renamed to `DoU_preprocess_grid()` to create a more consistent API and to better indicate that this function is specifically designed for preprocessing the units data to reconstruct the DEGURBA classification with `classify_units()`. 
#' @param units character / object of class `sf`. Path to the vector layer with small spatial units, or an object of class `sf` with the small spatial units
#' @param classification character / SpatRaster. Path to the grid cell classification of the Degree of Urbanisation, or SpatRaster with the grid cell classification
#' @param pop character / SpatRaster. Path to the population grid, or SpatRaster with the population grid
#' @param resample_resolution numeric. Resolution to which the grids are resampled during pre-processing. If `NULL`, the grids are resampled to the smallest resolution among the population and classification grid.
#' @param dissolve_units_by character. If not `NULL`, the units are dissolved by this column's value, can for example be used to dissolve spatial units to a certain GADM level (see examples).
#' @return named list with the required data to execute the spatial units classification procedure, and their metadata. The list contains the following elements:
#' - `classification`: the (resampled and cropped) grid cell classification layer
#' - `pop`: the (resampled and cropped) population grid
#' - `units`: the (dissolved and filtered) spatial units (object of class `sf`)
#' - `metadata`: named list with the metadata of the input files. It contains the elements `units`, `classification` and `pop` (with paths to the respective data sources), `resample_resolution` and `dissolve_units_by` if not `NULL`. (Note that when the input sources are passed by object , the metadata might be empty).
#' @keywords internal
#' @export
preprocess_units <- function(units, classification, pop, resample_resolution = NULL, dissolve_units_by = NULL){
  return(DoU_preprocess_units(units, classification, pop, resample_resolution, dissolve_units_by))
}