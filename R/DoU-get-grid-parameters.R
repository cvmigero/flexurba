#' Get the parameters for the grid cell classification
#'
#' The argument `parameter` of the function [`classify_grid()`] allows to adapt the standard specifications in the Degree of Urbanisation in order to construct an alternative version of the grid classification. This function returns a named list with the standard parameters.
#' @param level1 logical. Whether to return the standard parameters of level 1 of the Degree of Urbanisation (`TRUE`), or level 2 of the Degree of Urbanisation (`FALSE`).
#' @return named list with the standard parameters
#' @examples
#' # example on how to employ the function to construct
#' # an alternative version of the grid classification:
#' # get the standard parameters
#' parameters <- get_grid_parameters()
#'
#' # adapt the standard parameters
#' parameters$UCL_density_threshold <- 500
#' parameters$UCL_size_threshold <- 6000
#' parameters$UCL_smooth_pop <- TRUE
#' parameters$UCL_smooth_pop_window <- 7
#'
#' # load the data
#' grid_data <- DoU_load_grid_data_belgium()
#'
#' # use the adapted parameters to construct a grid cell classification
#' classification <- classify_grid(
#'   data = grid_data,
#'   parameters = parameters
#' )
#' @export
get_grid_parameters <- function(level1 = TRUE) {
  if (level1) {
    return(list(
      UC_density_threshold = 1500,
      UC_size_threshold = 50000,
      UC_contiguity_rule = 4,
      UC_built_criterium = TRUE,
      UC_built_threshold = 0.2,
      built_optimal_data = NULL,
      UC_smooth_pop = FALSE,
      UC_smooth_pop_window = 5,
      UC_gap_fill = TRUE,
      UC_max_gap = 15,
      UC_smooth_edge = TRUE,
      UC_smooth_edge_fun = "majority_rule_R2023A",
      UCL_density_threshold = 300,
      UCL_size_threshold = 5000,
      UCL_contiguity_rule = 8,
      UCL_smooth_pop = FALSE,
      UCL_smooth_pop_window = 5,
      water_land_threshold = 0.5,
      water_pop_threshold = 0,
      water_built_threshold = 0
    ))
  } else {
    return(list(
      UC_density_threshold = 1500,
      UC_size_threshold = 50000,
      UC_contiguity_rule = 4,
      UC_built_criterium = TRUE,
      UC_built_threshold = 0.2,
      built_optimal_data = NULL,
      UC_gap_fill = TRUE,
      UC_max_gap = 15,
      UC_smooth_edge = TRUE,
      UC_smooth_edge_fun = "majority_rule_R2023A",
      DUC_density_threshold = 1500,
      DUC_size_threshold = 5000,
      DUC_built_criterium = TRUE,
      DUC_built_threshold = 0.2,
      DUC_contiguity_rule = 4,
      SDUC_density_threshold = 300,
      SDUC_size_threshold = 5000,
      SDUC_contiguity_rule = 8,
      SDUC_buffer_size = 3,
      SUrb_density_threshold = 300,
      SUrb_size_threshold = 5000,
      SUrb_contiguity_rule = 8,
      RC_density_threshold = 300,
      RC_size_threshold = 500,
      RC_contiguity_rule = 8,
      LDR_density_threshold = 50,
      water_land_threshold = 0.5,
      water_pop_threshold = 0,
      water_built_threshold = 0
    ))
  }
}
