#' Replace non-valid mollweide cells with NA
#'
#' The function returns the input classification, but replaces the value of 
#' non-valid mollweide cells with NA
#' @param classification SpatRaster. Grid cell classification
#' @return SpatRaster
#' @noRd
mask_mollweide <- function(classification){
  # load valid mollweide data and crop to classification
  valid_mollweide <- terra::rast(system.file("extdata", "valid-mollweide.tif", package = "flexurba")) %>%
    terra::crop(classification)
  # return maks
  return(terra::mask(classification, valid_mollweide))
}




#' Convert vector layer to Spatextent
#'
#' The function returns a SpatExtent covering the same area as the bounding box of the vector layer
#' @param layer object of class `sf`, or any object that has a `sf::st_bbox()`
#' @return SpatExtent
#' @noRd
convert_layer_to_spatextent <- function(layer) {
  extent <- sf::st_bbox(layer)
  
  return(terra::ext(c(extent$xmin, extent$xmax, extent$ymin, extent$ymax)))
}

# utility function to write metadata-files
write_metadata <- function(metadata_file, metadata) {
  json_file <- jsonlite::toJSON(metadata)
  write(json_file, metadata_file)
}



#' Convert a SpatExtent to a vector layer
#'
#' The function returns an object of class `sf` with 1 polygon covering the same area as the SpatExtent
#' @param extent SpatExtent, or any object that has a SpatExtent
#' @param crs character. Coordinate reference system of `extent`. By default, it is assumed that the coordinates are in the Mollweide projection.
#' @return object of class `sf`
#' @noRd
convert_spatextent_to_layer <- function(extent, crs = "ESRI:54009") {
  # get the coordinates
  extent <- terra::ext(extent) %>%
    as.list()
  
  return(sf::st_as_sf(
    x = data.frame(
      x = c(extent$xmin, extent$xmax),
      y = c(extent$ymin, extent$ymax)
    ),
    coords = c("x", "y"),
    crs = crs
  ) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf())
}




#' Crop multiple SpatRasters to the one with the smallest extent
#'
#' The function identifies the SpatRaster with the smallest extent, and crops all SpatRasters on this extent.
#' @param ... one or more SpatRasters
#' @return a list of the SpatRasters cropped to the smallest extent
#' @noRd
crop_to_smallest_extent <- function(...) {
  grids <- list(...)
  xmin <- Reduce(max, lapply(grids, terra::xmin))
  xmax <- Reduce(min, lapply(grids, terra::xmax))
  ymin <- Reduce(max, lapply(grids, terra::ymin))
  ymax <- Reduce(min, lapply(grids, terra::ymax))
  extent <- terra::ext(xmin, xmax, ymin, ymax)
  return(lapply(grids, terra::crop, extent))
}




#' Filter spatial units based on a SpatExtent
#'
#' The function filters spatial units based on the provided extent. Only polygons that intersect with the extent are retained.
#' @param units object of class `sf`
#' @param extent SpatExtent, or any object that has a SpatExtent
#' @param crs character. Coordinate reference system of the `extent`. The filtered data will be returned in this reference system.
#' @param filename character. Output filename
#' @return object of class `sf` with filtered data
#' @noRd
filter_units_on_extent <- function(units, extent, crs = "ESRI:54009", filename = NULL) {
  # convert to bbox
  bbox <- convert_spatextent_to_layer(extent, crs)
  
  # global variable
  .data <- NULL
  
  # filter the units
  sf::st_geometry(units) <- "geometry"
  units <- units %>%
    sf::st_transform(crs) %>%
    dplyr::filter(as.vector(sf::st_intersects(.data[["geometry"]], bbox, sparse = FALSE)))
  
  # write the data
  if (!is.null(filename)) {
    sf::st_write(units, filename, append = FALSE)
  }
  
  return(units)
}



#' Get the color palette of the Degree of Urbanisation
#'
#' The function returns a named vector with the color palette that is used by the Global Human Settlement Layer to visualise the Degree of Urbanisation classification (see [GHSL Data Package 2023](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf)).
#' @param level1 logical. Whether to return the color palette of level 1 of the Degree of Urbanisation (`TRUE`) or level 2 of the Degree of Urbanisation (`FALSE`)
#' @return named vector with the color palette
#' @noRd
GHSL_palette <- function(level1 = TRUE) {
  if (level1) {
    return(c(
      "3" = "#FF0000",
      "2" = "#FFAA00",
      "1" = "#73B273",
      "0" = "#7AB6F5"
    ))
  } else {
    return(c(
      "30" = "#FF0000",
      "23" = "#732600",
      "22" = "#A87000",
      "21" = "#FFFF00",
      "13" = "#375623",
      "12" = "#ABCD66",
      "11" = "#CDF57A",
      "10" = "#7AB6F5"
    ))
  }
}


#' Get the labels of the classes of the Degree of Urbanisation
#'
#' The function returns a vector with the class labels that are used by the Global Human Settlement Layer for the Degree of Urbanisation classification (see [GHSL Data Package 2023](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf)).
#' @param level1 logical. Whether to return the labels of level 1 of the Degree of Urbanisation (`TRUE`) or level 2 of the Degree of Urbanisation (`FALSE`)
#' @param grids logical. Whether to return the labels of grid cell classification (`TRUE`) or spatial units classification (`FALSE`)
#' @return vector with the class labels
#' @noRd
GHSL_labels <- function(level1 = TRUE, grids = TRUE) {
  if (grids) {
    if (level1) {
      return(c(
        "urban centre",
        "urban cluster",
        "rural cell",
        "water cell"
      ))
    } else {
      return(c(
        "urban centre",
        "dense urban cluster",
        "semi-dense urban cluster",
        "suburban or peri-urban cell",
        "rural cluster",
        "low density rural cell",
        "very low density rural cell",
        "water cell"
      ))
    }
  } else {
    if (level1) {
      return(c(
        "city",
        "town and semi-dense area",
        "rural area"
      ))
    } else {
      return(c(
        "city",
        "dense town",
        "semi-dense town",
        "suburbs or peri-urban area",
        "village",
        "dispersed rural area",
        "mostly uninhabitated area"
      ))
    }
  }
}

