#' Preprocess the data for the DEGURBA grid cell classification
#'
#' @description
#' The grid cell classification of the Degree of Urbanisation requires three different inputs:
#'
#' - a built-up area grid
#' - a population grid
#' - a land grid
#'
#' The function rescales the built-up area and land grid if necessary (see details) and computes the population and built-up area per permanent land by dividing the respective grids by the land layer.
#'
#' @param directory character. Path to the directory where the three input grids are saved (for example generated by the function [download_GHSLdata()])
#' @param filenames vector of length 3 with the filenames of the built-up area, population and land grid
#' @param rescale_land logical. Whether to rescale the values of the land grid (see details)
#' @param rescale_built logical. Whether to rescale the values of the built-up area grid (see details)
#' @return named listed with the required data to execute the grid cell classification procedure. The list contains following elements:
#' - `built`: built-up area grid
#' - `pop`: population grid
#' - `land`: land grid
#' - `pop_per_land`: population per area of permanent land
#' - `built_per_land`: built-up area per permanent land
#' - `metadata_BUILT_S`: the metadata of the built-up area grid
#' - `metadata_POP`: the metadata of the population grid
#' - `metadata_LAND`: the metadata of the land grid.
#' @details
#' The values of the land grid and built-up area grid should range from `0` to `1`, representing the proportion of permanent land and built-up area respectively. However, the grid values of the GHSL are standard in m². With a cell size of 1 km², the values thus range from `0` to `1 000 000`. The two grids can be rescaled by setting `rescale_land=TRUE` and/or `rescale_built=TRUE` respectively.
#' @export
DoU_preprocess_grid <- function(
  directory,
  filenames = c("BUILT_S.tif", "POP.tif", "LAND.tif"),
  rescale_land = TRUE,
  rescale_built = TRUE
) {
  # check if the directory exists
  if (!dir.exists(directory)) {
    stop(paste("Invalid argument:", directory, "does not exist"))
  }

  # check if files exist in directory
  if (!all(filenames %in% list.files(directory))) {
    stop(paste(
      "Invalid argument:",
      paste0(filenames, collapse = ", "),
      " are no files in ",
      directory
    ))
  }

  # read the grid files
  built <- terra::rast(file.path(directory, filenames[[1]]))
  pop <- terra::rast(file.path(directory, filenames[[2]]))
  land <- terra::rast(file.path(directory, filenames[[3]]))

  # check if resolution of grids match
  if (
    length(unique(c(terra::res(built), terra::res(pop), terra::res(land)))) != 1
  ) {
    stop("Invalid argument: the resolution of the grids do not match")
  }

  # get resolution of the grids
  resolution <- terra::res(land)[[1]]
  if (resolution != 1000) {
    warning(paste0(
      "You are using a grid with a resolution of ",
      resolution,
      "m . The official algorithm of the Degree of Urbanisation is designed to be applied on a 1000 m grid.\n"
    ))
  }

  # rescale the grids if necessary
  if (rescale_land) {
    land <- land / resolution / resolution
  }
  if (rescale_built) {
    built <- built / resolution / resolution
  }

  # crop the grids to the one with the smallest extent
  grids <- crop_to_smallest_extent(built, pop, land)

  # check if the grids are in the Mollweide projection
  for (i in seq_along(grids)) {
    if (
      terra::crs(grids[[i]], proj = TRUE) !=
        terra::crs("ESRI:54009", proj = TRUE)
    ) {
      stop(paste(
        "Invalid argument: the grids should be in the Mollweide projection."
      ))
    }
  }

  # save the grids in a list
  data <- list()
  data$built <- grids[[1]]
  data$pop <- grids[[2]]
  data$land <- grids[[3]]
  names(data$pop) <- "POP"
  names(data$built) <- "BUILT_S"
  names(data$land) <- "LAND"

  # calculate population and built-up per permanent land
  data$pop_per_land <- data$pop / data$land
  names(data$pop_per_land) <- "POP_PER_LAND"
  data$built_per_land <- round(data$built / data$land, digits = 3)
  names(data$built_per_land) <- "BUILT_S_PER_LAND"

  metadata_list <- list("metadata_BUILT_s", "metadata_POP", "metadata_LAND")

  # write metadata files
  for (i in seq_along(metadata_list)) {
    if (
      file.exists(file.path(directory, gsub(".tif", ".json", filenames[[i]])))
    ) {
      data[[metadata_list[[i]]]] <- jsonlite::fromJSON(file.path(
        directory,
        gsub(".tif", ".json", filenames[[i]])
      ))
    } else {
      data[[metadata_list[[i]]]] <- NULL
    }
  }

  return(data)
}


#'  Preprocess the data for the DEGURBA grid cell classification
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `preprocess_grid()` has been renamed to `DoU_preprocess_grid()` to create a more consistent API and to better indicate that this function is specifically designed for preprocessing the grid data to reconstruct the DEGURBA classification with `DoU_classify_grid()`.
#' @param directory character. Path to the directory where the three input grids are saved (for example generated by the function [download_GHSLdata()])
#' @param filenames vector of length 3 with the filenames of the built-up area, population and land grid
#' @param rescale_land logical. Whether to rescale the values of the land grid (see details)
#' @param rescale_built logical. Whether to rescale the values of the built-up area grid (see details)
#' @return named listed with the required data to execute the grid cell classification procedure. The list contains following elements:
#' - `built`: built-up area grid
#' - `pop`: population grid
#' - `land`: land grid
#' - `pop_per_land`: population per area of permanent land
#' - `built_per_land`: built-up area per permanent land
#' - `metadata_BUILT_S`: the metadata of the built-up area grid
#' - `metadata_POP`: the metadata of the population grid
#' - `metadata_LAND`: the metadata of the land grid.
#' @keywords internal
#' @export
preprocess_grid <- function(
  directory,
  filenames = c("BUILT_S.tif", "POP.tif", "LAND.tif"),
  rescale_land = TRUE,
  rescale_built = TRUE
) {
  return(DoU_preprocess_grid(directory, filenames, rescale_land, rescale_built))
}
