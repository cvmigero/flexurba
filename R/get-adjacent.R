#' Identify adjacent cells
#'
#' The function identifies all cells that are adjacent to non-`NA` cells in `x`. The implementation of the function relies on the function `terra::adjacent()`.
#' @param x SpatRaster
#' @param cells character / integer. Either `"all"` or a specific cell value. If `cells="all"`, adjacent cells are identified for all non-`NA` cells in `x`. Otherwise, adjacent cells are only identified for cells in `x` with the specific cell value.
#' @param adjacent_value integer. Value assigned to adjacent cells in the resulting grid
#' @param include logical. Whether to include the focal cells in the resulting grid
#' @param directions integer. Which cells are considered adjacent:  `4` for rooks case (horizontal and vertical neighbours) or `8` for queens case (horizontal, vertical and diagonal neighbours)
#' @return SpatRaster with adjacent cells
#' @examples
#' set.seed(10)
#' nr <- nc <- 10
#' r <- terra::rast(
#'   ncols = nc, nrows = nr,
#'   ext = c(0, nc, 0, nr),
#'   vals = sample(c(NA, 1, 2), nr * nc, replace = TRUE, prob = c(0.8, 0.1, 0.1))
#' )
#' terra::plot(r)
#' adj1 <- get_adjacent(r)
#' terra::plot(adj1)
#' adj2 <- get_adjacent(r, cells = 1, include = FALSE)
#' terra::plot(adj2)
#' @export
get_adjacent <- function(x, cells = "all", adjacent_value = 1, include = TRUE, directions = 8) {
  # check if directions argument is valid
  if (!(directions %in% c(4, 8))) {
    stop("Invalid argument: directions should be either 4 (rooks case) or 8 (queens case)")
  }

  adjacent_grid <- x %>% terra::setValues(NA)

  # select cells of interest
  if (cells != "all") {
    x <- terra::which.lyr(x == cells)
  }

  adjacent_index <- terra::adjacent(
    x = x,
    cells = which(!is.na(x[])),
    directions = directions,
    pairs = FALSE
  )
  terra::set.values(adjacent_grid, adjacent_index, adjacent_value)
  if (include) {
    terra::set.values(adjacent_grid, which(!is.na(x[])), adjacent_value)
  } else {
    adjacent_grid <- terra::mask(adjacent_grid, x, inverse = TRUE)
  }

  return(adjacent_grid)
}
