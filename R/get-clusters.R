#' Identify clusters of cells that meet the criteria
#'
#' @description
#' Identify clusters of cells that meet the minimum density criterium/citeria, the mimimum size criterium and the contiguity criterium.
#'
#' The function can be executed with one density criterium, or with two density criteria:
#'
#' - **With one density criterium:** Cells are selected if their value is above or equal to the density threshold (`xden >= minden`). Selected cells are afterwards grouped together based on the contiguity rule. Groups of cells are valid clusters if they also meet the total size criterium (sum of `xsiz` per group above or equal to`minsiz`).
#' - **With two density criteria:** Cells are selected if their value is above or equal to one of two density thresholds (`xden >= minden`, or `xden2 >= minden2`). Selected cells are afterwards grouped together based on the contiguity rule. Groups of cells are valid clusters if they also meet the total size criterium (sum of `xsiz` per group above or equal to`minsiz`).
#' @param xden SpatRaster. Grid for density criterium 1
#' @param minden numeric. Minimum density threshold 1
#' @param xden2 SpatRaster. Grid for density criterium 2
#' @param minden2 numeric. Minimum density threshold 2
#' @param xsiz SpatRaster. Grid for the size criterium. If `NULL`, then `xden` is employed for the size criterium.
#' @param minsiz numeric. Minimum size threshold
#' @param directions integer. Which cells are considered adjacent:  `4` for rooks case (horizontal and vertical neighbors) or `8` for queens case (horizontal, vertical and diagonal neighbors)
#' @return SpatRaster with cluster of cells. The value of the cells represent the id of the clusters.
#' @examples
#' # load data
#' grid_data_belgium <- flexurba::load_grid_data_belgium()
#'
#' # get clusters of cells (4-cell connectivity) with at least 1500 inhabitants
#' # per km² of permanent land and a minimum total population of 50 000
#' # inhabitants:
#' terra::plot(get_clusters(
#'   xden = grid_data_belgium$pop_per_land,
#'   minden = 1500,
#'   xsiz = grid_data_belgium$pop,
#'   minsiz = 50000,
#'   directions = 4
#' ))
#'
#' # get clusters of cells (4-cell connectivity) with at least 1500 inhabitants
#' # per km² of permanent land or at least 20% built-up area per permanent
#' # land, and a minimum total population of 50 000 inhabitants:
#' terra::plot(get_clusters(
#'   xden = grid_data_belgium$pop_per_land,
#'   minden = 1500,
#'   xden2 = grid_data_belgium$built_per_land,
#'   minden2 = 0.2,
#'   xsiz = grid_data_belgium$pop,
#'   minsiz = 50000,
#'   directions = 4
#' ))
#'
#' # get clusters of cells (8-cell connectivity) with at least 300 inhabitants
#' # per km² of permanent land, and a minimum total population of 5000
#' # inhabitants:
#' terra::plot(get_clusters(
#'   xden = grid_data_belgium$pop_per_land,
#'   minden = 300,
#'   xsiz = grid_data_belgium$pop,
#'   minsiz = 5000,
#'   directions = 8
#' ))
#' @export
get_clusters <- function(xden, minden, xden2 = NULL, minden2 = NULL, xsiz = NULL, minsiz, directions) {
  # if no grid is provided, use the density grid
  if (is.null(xsiz)) {
    xsiz <- xden
  }
  
  if (!is.numeric(minden)) {
    stop(paste("Invalid argument:", minden, "is not a valid parameter for the minimum density threshold"))
  }
  if (!is.null(minden2) & !is.numeric(minden2)) {
    stop(paste("Invalid argument:", minden2, "is not a valid parameter for the minimum density threshold"))
  }
  if (!is.null(minsiz) & !is.numeric(minsiz)) {
    stop(paste("Invalid argument:", minsiz, "is not a valid parameter for the minimum size threshold"))
  }
  
  # none or both should be NULL
  if (sum(c(is.null(minden2)), is.null(xden2)) == 1) {
    stop(paste("Invalig arguments: minden2 and xden2 should be both provided or both NULL"))
  }
  
  if (!is.null(xden2)) {
    if ((terra::ext(xden) != terra::ext(xden2)) | (terra::ext(xden) != terra::ext(xsiz))) {
      stop("Invalid argument: extents of the provided grids do not match.")
    }
  } else {
    if (terra::ext(xden) != terra::ext(xsiz)) {
      stop("Invalid argument: extents of the provided grids do not match.")
    }
  }
  
  # apply density threshold 1
  densecells <- terra::which.lyr(xden >= minden)
  
  # apply density threshold 2
  if (!is.null(minden2) & !is.null(xden2)) {
    densecells2 <- terra::which.lyr(xden2 >= minden2)
    terra::set.values(densecells, which(densecells2[] == 1), 1)
  }
  
  # get clusters based on contiguity criteria
  densepatches <- densecells %>% flexurba::get_patches(directions = directions)
  
  # get size of the clusters and filter
  zonal_statistics <- xsiz %>%
    terra::zonal(
      z = densepatches,
      fun = sum, na.rm = TRUE
    ) %>%
    as.data.frame()
  colnames(zonal_statistics) <- c("cluster", "sum")
  largeclusters <- zonal_statistics %>% dplyr::filter(sum >= minsiz)
  
  # only keep clusters sufficiently large
  terra::set.values(densepatches, which(!(densepatches[] %in% largeclusters$cluster)), NA)
  names(densepatches) <- names(xden)
  
  return(densepatches)
}
