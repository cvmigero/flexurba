#' Detect patches of cells
#'
#' @description
#' The function detects patches of cells based on rook contiguity (horizontal and vertical neighbours) or queen contiguity (horizontal, vertical and diagonal neighbours).
#'
#' Patches of cells are groups of cells that are surrounded by `NA` of `NaN` values. The function identifies patches by converting the SpatRaster into polygons with the functions `geos::as_geos_geometry()` and `geos::geos_unnest()`. During this conversion polygons are automatically created as cells with rook contiguity. Touching polygons are afterwards dissolved to create patches with queen contiguity.
#'
#'  The function is similar as `terra::patches()`, but is faster for large SpatRasters (see details).
#' @param x SpatRaster
#' @param directions integer. Which cells are considered adjacent: `4` for rooks case (horizontal and vertical neighbours) or `8` for queens case (horizontal, vertical and diagonal neighbours)
#' @param cells  character / vector. Either `"all"` or a vector with specific cell values. If `cells="all"`, patches are identified based on all non-`NA` cells in `x`. Otherwise, patches are only identified for cells in `x` with the specific cell values.
#' @return SpatRaster with patches of cells. The value of the cells represent the id of the patches.
#' @examples
#' r <- terra::rast(nrows = 8, ncols = 8, vals = c(
#'   2, 2, NA, NA, NA, NA, NA, NA,
#'   NA, NA, NA, NA, NA, NA, NA, NA,
#'   NA, NA, 1, NA, NA, NA, NA, NA,
#'   NA, NA, NA, 1, NA, NA, NA, NA,
#'   NA, NA, NA, NA, 1, 1, 1, 1,
#'   NA, NA, NA, NA, NA, 1, 1, 1,
#'   2, NA, NA, NA, NA, NA, NA, NA,
#'   NA, 2, NA, NA, NA, NA, NA, NA
#' ))
#' terra::plot(r)
#' patches_rook1 <- get_patches(r, directions = 4)
#' terra::plot(patches_rook1)
#' patches_rook2 <- get_patches(r, directions = 4, cells = 1)
#' terra::plot(patches_rook2)
#' patches_queen1 <- get_patches(r, directions = 8)
#' terra::plot(patches_queen1)
#' patches_queen2 <- get_patches(r, directions = 8, cells = 1)
#' terra::plot(patches_queen2)
#' @details
#' The function is similar as `terra::patches()`, but is faster for large SpatRasters.
#'
#' **Rook contiguity**
#' | Size SpatRaster | `terra::patches(directions=4)` | `flexurba::get_patches(directions=4)` |
#' | ----------- | ----------- | ----------- |
#' | 4000 x 4000 (106 642 not-`NA` cells) | 75.157 s | 5.351 s |
#' | 4000 x 4000 (423 888 not-`NA` cells) | 324.846 s |  8.336 s |
#'
#' **Queen contiguity**
#' | Size SpatRaster | `terra::patches(directions=8)` | `flexurba::get_patches(directions=8)` |
#' | ----------- | ----------- | ----------- |
#' | 4000 x 4000 (106 642 not-`NA` cells) | 37.322 s | 7.737 s |
#' | 4000 x 4000 (423 888 not-`NA` cells) | 183.428 s | 24.094 s |
#' @export
get_patches <- function(x, directions, cells = "all") {
  if (!is.na(terra::minmax(x)[[2]])) {
    if (directions == 4) {
      return(get_patches_rook(x, cells))
    } else if (directions == 8) {
      return(get_patches_queen(x, cells))
    } else {
      stop("Directions should be 8 (queens case) or 4 (rooks case)")
    }
  } else {
    return(x)
  }
}


# HELPER FUNCTION FOR ROOK CONTIGUITY
get_patches_rook <- function(x, cells = "all") {
  if (!(cells == "all")) {
    terra::values(x)[which(!(x[] %in% cells))] <- NA
  }

  # assign same value to all non-NA cells
  terra::values(x)[which(!is.na(x[]))] <- 1

  polygons <- terra::as.polygons(x) %>%
    sf::st_as_sf() %>%
    geos::as_geos_geometry() %>%
    geos::geos_unnest(
      keep_multi = FALSE,
      keep_empty = FALSE
    ) %>%
    geos::geos_write_wkt() %>%
    terra::vect()

  # convert back to raster layer
  rast_lyr <- polygons %>%
    terra::rasterize(
      y = x,
      field = seq_len(nrow(polygons))
    )

  names(rast_lyr) <- names(x)
  terra::set.values(rast_lyr, which(is.nan(rast_lyr[])), NA)

  return(rast_lyr)
}


# HELPER FUNCTION FOR QUEEN CONTIGUITY
get_patches_queen <- function(x, cells = "all") {
  if (!(cells == "all")) {
    terra::values(x)[which(!(x[] %in% cells))] <- NA
  }

  # assign same value to all non-NA cells
  terra::values(x)[which(!is.na(x[]))] <- 1

  geo <- geometry <- group <- neighbors <- length_of_neighbors <- NULL
  # convert to polygons
  polygons <- terra::as.polygons(x) %>%
    sf::st_as_sf() %>%
    geos::as_geos_geometry() %>%
    geos::geos_unnest(keep_multi = FALSE, keep_empty = FALSE)

  # get a list of neighbors (= touching polygons)
  neighbor_list <- sf::st_intersects(sf::st_as_sf(polygons), prepared = TRUE, remove_self = TRUE)
  attributes(neighbor_list) <- NULL

  # create data table
  dt <- data.table::data.table(
    id = seq_along(neighbor_list),
    neighbors = neighbor_list,
    geo = polygons
  )
  dt[, length_of_neighbors := lengths(neighbors), ]

  # assign a group id to neighbors
  dt[, group := group_polygons(.SD), ]

  # merge geometries whitin the same group
  poly_queen <- dt[, list(geometry = geos::geos_write_wkt(geos::geos_make_collection(geo) %>% geos::geos_unary_union())), by = "group"][, geometry, ]

  # convert the polygons back to a raster layer
  rast_lyr <- poly_queen %>%
    terra::vect() %>%
    terra::rasterize(y = x, field = seq_along(poly_queen))
  names(rast_lyr) <- names(x)
  terra::set.values(rast_lyr, which(is.nan(rast_lyr[])), NA)

  return(rast_lyr)
}


# HELPER FUNCTION FOR GROUPING POLYGONS WITH QUEEN CONTIGUITY
group_polygons <- function(dt) {
  length_of_neighbors <- NULL

  # create empty vector
  group <- rep(NA, nrow(dt))
  no_neigh <- dt[, length_of_neighbors, ] < 1

  # assign unique id when no neighbors
  group[no_neigh] <- 1:sum(no_neigh)

  # get current group value
  group_val <- sum(no_neigh)
  neighbor_list <- dt[, neighbors]

  # as long as not all polygons are assigned to a group
  index <- 0
  while (!is.na(index)) {
    # get the first polygon not assigned to a group
    index <- fastmatch::fmatch(NA, group)

    # get neighbors of the polygon
    neighbors <- neighbor_list[index]

    currentlist <- c(index)
    previouslist <- c()

    # iteratively get all polygons that are touching with the current list
    while (length(previouslist) != length(currentlist)) {
      previouslist <- currentlist
      currentlist <- unique(c(previouslist, unlist(neighbor_list[previouslist])))
    }

    # assign id to the group of polygons
    group_val <- group_val + 1
    group[currentlist] <- group_val
  }
  return(group)
}
