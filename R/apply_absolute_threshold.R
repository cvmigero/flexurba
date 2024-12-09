#' Identify urban boundaries by applying an absolute threshold 
#'
#' @description
#' TODO
#' @param grid 
#' @param threshold 
#' @param operator 
#' @param smoothing 
#' @return SpatRaster with the urban areas
#' @examples
#' 
#' @export
apply_absolute_threshold <- function(grid, threshold, operator='greater_than', smoothing=TRUE){
  
  # check if spatraster only has 1 layer
  if (terra::nlyr(grid) > 1){
    grid <- grid[[1]]
    warning('The provided grid has multiple layers, be aware that the threshold is applied to the first layer.')
  }  
  
  
  if (operator == "greater_than") {
    boundaries <- terra::which.lyr(grid > threshold)
  } else if (operator == "greater_or_equal") {
    boundaries <- terra::which.lyr(grid >= threshold)
  } else if (operator == "smaller_than") {
    boundaries <- terra::which.lyr(grid < threshold)
  } else if (operator == "smaller_or_equal") {
    boundaries <- terra::which.lyr(grid <= threshold)
  } else if (operator == "equals") {
    boundaries <- terra::which.lyr(grid == threshold)
  } else {
    stop("Invalid argument: operator should be one of: 'greater_than', 'greater_or_equal', 'smaller_than', 'smaller_or_equal', 'equals'.")
  }
  
  if (smoothing){
    # TODO: change this?
    boundaries <- apply_majority_rule_R2022A(boundaries)
  }
  
  return(boundaries)
}