#' Fill gaps in clusters of cells
#'
#' The function fills gaps with an area smaller than `max_gap`. A gap is considered a patch of `NA` cells that lies within a cluster of cells with the same value. The implementation of the function relies on the function `nngeo::st_remove_holes()`.
#' @param x SpatRaster
#' @param max_gap numeric. Gaps with an area smaller than `max_gap` are filled (see details for more information about the unit of this value).
#' @return SpatRaster
#' @details
#' `max_gap` has the same unit as the resolution of `x`. For example, with a SpatRaster in Mollweide (`EPSG:54009`) and a resolution of 1 km², `max_gap` is interpreted in km².
#' @examples
#' nr <- nc <- 8
#' r <- terra::rast(nrows = nr, ncols = nc, ext = c(0, nc, 0, nr), crs = "epsg:25831")
#' terra::values(r) <- c(
#'   NA, NA, NA, NA, 1, 1, 1, NA,
#'   NA, 2, 2, 2, NA, NA, 1, NA,
#'   NA, 2, NA, 2, 2, NA, 1, 1,
#'   NA, 2, NA, NA, 2, 2, NA, NA,
#'   NA, 2, 2, 2, 2, NA, NA, NA,
#'   NA, NA, NA, NA, NA, NA, NA, NA,
#'   NA, NA, NA, NA, NA, NA, NA, NA,
#'   NA, NA, NA, NA, NA, NA, NA, NA
#' )
#' terra::plot(r)
#' gaps_filled <- fill_gaps(r)
#' terra::plot(gaps_filled)
#' @export
fill_gaps <- function(x, max_gap = 15) {
  if (!is.numeric(max_gap)) {
    stop(paste(
      "Invalid argument:",
      max_gap,
      "is not a valid parameter for max_gap"
    ))
  }

  # convert to polygons
  polygons <- terra::as.polygons(x, dissolve = TRUE) %>%
    sf::st_as_sf()

  if (nrow(polygons) > 0) {
    # fill gaps
    polygons <- nngeo::st_remove_holes(
      polygons,
      max_area = (max_gap - 1) * prod(terra::res(x))
    )

    # convert to raster
    rast_lyr <- polygons %>%
      terra::vect() %>%
      terra::rasterize(
        y = x,
        field = names(polygons)[1]
      )
    names(rast_lyr) <- names(x)
    terra::set.values(rast_lyr, which(is.na(rast_lyr[])), NA)

    return(rast_lyr)
  } else {
    return(x)
  }
}
