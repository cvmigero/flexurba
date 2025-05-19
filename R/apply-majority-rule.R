#' Apply the majority rule algorithm
#' @description
#' The functions applies the majority rule to smooth edges of clusters of cells. The function supports two different version of the majority rule algorithm: the version of [GHSL Data Package 2022](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2022.pdf) and [GHSL Data Package 2023](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf):
#'
#' - `version="R2022A"`: If a cell has at least five of the eight surrounding cells belonging to a unique cluster of cells, then the cell is added to that cluster. The process is iteratively repeated until no more cells are added.
#' - `version="R2023A"`:  A cell is added to a cluster if the majority of the surrounding cells belongs to one unique cluster, with majority only computed among populated (`pop > 0`) or land cells (`land > 0.5`). Cells with permanent water (`permanent_water` not `NA`) can never be added to a cluster of cells. The process is iteratively repeated until no more cells are added.
#'
#' @param x SpatRaster. Grid with clusters of cells
#' @param version character. Version of the majority rule algorithm. Supported versions are `"R2022A"` and `"R2023A"`.
#' @param permanent_water SpatRaster. Grid with permanent water cells (only required when `version="R2023A"`)
#' @param land SpatRaster. Grid with proportion of permanent land (only required when `version="R2023A"`)
#' @param pop SpatRaster. Grid with population (only required when `version="R2023A"`)
#' @return SpatRaster with clusters of cells with smoothed edges
#' @examples
#' nr <- nc <- 8
#' r <- terra::rast(nrows = nr, ncols = nc, ext = c(0, nc, 0, nr), vals = c(
#'   NA, NA, 1, 1, 1, NA, NA, NA,
#'   NA, NA, NA, 1, 1, NA, NA, NA,
#'   NA, NA, 2, NA, NA, NA, NA, NA,
#'   NA, NA, 2, NA, NA, 2, NA, NA,
#'   NA, NA, 2, NA, 2, 2, NA, NA,
#'   2, 2, 2, 2, 2, 2, NA, NA,
#'   NA, NA, 2, 2, NA, NA, NA, NA,
#'   NA, NA, NA, 2, NA, NA, NA, NA
#' ))
#' terra::plot(r)
#' smoothed <- apply_majority_rule(r)
#' terra::plot(smoothed)
#' @export
apply_majority_rule <- function(x, version = "R2022A", permanent_water = NULL, land = NULL, pop = NULL) {
  if (!(version %in% c("R2022A", "R2023A"))) {
    stop("Invalid argument: version should be R2022A or R2023A")
  }

  if (version == "R2023A" & any(is.null(c(permanent_water, land, pop)))) {
    stop("Invalid argument: permanent_water, land and pop are required when version is R2023A")
  }

  if (version == "R2022A") {
    return(apply_majority_rule_R2022A(x))
  } else {
    return(apply_majority_rule_R2023A(x, permanent_water, land, pop))
  }
}



#' Helper function for majority rule 2022
#' @description
#' The functions applies the majority rule to smooth edges of clusters of cells. If a cell has at least five of the eight surrounding cells belonging to a unique cluster of cells, then the cell is added to that cluster. The process is iteratively repeated until no more cells are added.
#'
#' @param x SpatRaster. Grid with clusters of cells
#' @return SpatRaster with clusters of cells with smoothed edges
#' @noRd
apply_majority_rule_R2022A <- function(x) {
  # get an distinct id for adjacent cells (= largest cluster id + 1)
  adjacent_value <- x %>%
    terra::unique() %>%
    max(na.rm = TRUE) %>%
    magrittr::add(1)

  # counter for steps
  step <- 1

  # initialise variables
  no_new_cells <- FALSE # TRUE when no new cells are added by the majority rule (at the start: FALSE)
  current_patches <- x # current set of urban centres
  newly_added_cells <- current_patches # newly added cells (at the start: all current urban centre cells)

  # as long as new cells are added to patches
  while (!no_new_cells) {
    # get cells adjacent cells to the newly added cells
    adjacent_cells <- flexurba::get_adjacent(newly_added_cells, adjacent_value = adjacent_value, include = FALSE)

    # combine current patches with the adjacent cells
    patches_with_ajacent <- terra::cover(current_patches, adjacent_cells)

    # identify new cells based on the majority rule algorithm
    newly_added_cells <- terra::focalCpp(
      x = patches_with_ajacent,
      w = 3,
      fun = majority_rule_R2022A,
      fillvalue = NA,
      value_of_interest = adjacent_value
    )

    # add the new cells to the patches
    current_patches <- terra::cover(current_patches, newly_added_cells)

    # check if there were new cells added
    no_new_cells <- is.na(terra::minmax(newly_added_cells))[1]
    step <- step + 1
  }

  return(current_patches)
}




#' Helper function for majority rule 2023
#' @description
#' The functions applies the majority rule to smooth edges of clusters of cells.  A cell is added to a cluster if the majority of the surrounding cells belongs to one unique cluster, with majority only computed among populated (`pop > 0`) or land cells (`land > 0.5`). Cells with permanent water (`permanent_water` not `NA`) can never be added to a cluster of cells. The process is iteratively repeated until no more cells are added.
#'
#' @param x SpatRaster. Grid with clusters of cells
#' @param permanent_water SpatRaster. Grid with permanent water cells
#' @param land SpatRaster. Grid with proportion of permanent land
#' @param pop SpatRaster. Grid with population
#' @return SpatRaster with clusters of cells with smoothed edges
#' @noRd
apply_majority_rule_R2023A <- function(x, permanent_water, land, pop) {
  # get an distinct ids for special values

  # value to identify permanent water cells
  if (is.null(terra::unique(permanent_water))) {
    water_value <- 0
  } else {
    water_value <- as.numeric(terra::unique(permanent_water))
  }

  # value to identify cells that are adjacent to urban centres
  adjacent_value <- -1

  # value to identify cells to ignore in computing majority (non-populated and non-land cells)
  ignore_value <- -2

  # value to identify cells to are both adjacent to urban centres and to ignore
  # in computing majority (non-populated and non-land cells)
  adj_ignore_value <- -3

  # values to ignore: ignore values that are not populated (pop == 0) and are no land cells (land < 0.5)
  water_grid <- terra::which.lyr(land < 0.5)
  nopop_grid <- terra::which.lyr(pop == 0)
  ignore_grid <- terra::mask(water_grid, nopop_grid)
  terra::set.values(ignore_grid, which(!is.na(ignore_grid[])), ignore_value)

  # counter for steps
  step <- 1

  # initialise variables
  no_new_cells <- FALSE # TRUE when no new cells are added by the majority rule (at the start: FALSE)
  current_patches <- x # current set of urban centres
  newly_added_cells <- current_patches # newly added cells (at the start: all current urban centre cells)

  # as long as new cells are added to urban centres
  while (!no_new_cells) {
    # get cells adjacent to the newly added cells
    adjacent_cells <- flexurba::get_adjacent(newly_added_cells,
      adjacent_value = adjacent_value, include = FALSE
    )

    # mask with adjacent cells with ignore grid to get cells that are adjacent AND should be ignored
    adj_ignore <- terra::mask(ignore_grid, adjacent_cells)
    terra::set.values(adj_ignore, which(!is.na(adj_ignore[])), adj_ignore_value)

    # cover all grids to create one grid with all different special values
    patches_with_ajacent <- terra::cover(
      current_patches,
      terra::cover(
        permanent_water,
        terra::cover(
          adj_ignore,
          terra::cover(adjacent_cells, ignore_grid)
        )
      )
    )

    # identify new cells based on the majority rule algorithm
    newly_added_cells <- terra::focalCpp(
      x = patches_with_ajacent,
      w = 3,
      fun = majority_rule_R2023A,
      fillvalue = NA,
      adj_value = adjacent_value,
      ignore_value = ignore_value,
      water_value = water_value,
      adj_ignore = adj_ignore_value
    )

    # add the new cells to the urban centres
    current_patches <- terra::cover(current_patches, newly_added_cells)

    # check if there are new cells added
    no_new_cells <- is.na(terra::minmax(newly_added_cells))[1]
    step <- step + 1
  }

  return(current_patches)
}
