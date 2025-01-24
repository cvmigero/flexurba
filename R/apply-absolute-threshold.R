#' Identify urban boundaries by applying an absolute threshold 
#'
#' @description
#' TODO
#' @param grid 
#' @param threshold 
#' @param operator 
#' @param smoothing 
#' @return named list with the following elements:
#' - `rboundaries`: SpatRaster
#' - `vboundaries`: sf object
#' - `threshold`: the threshold value used to construct the boundaries
#' @examples
#' proxies <- load_proxies_belgium()
#' 
#' pop_above_1500 <- apply_absolute_threshold(proxies$pop, 1500)
#' terra::plot(pop_above_1500$boundaries)
#' 
#' built_above_20 <- apply_absolute_threshold(proxies$built, 0.20)
#' terra::plot(built_above_20$boundaries)
#' 
#' light_above_30 <- apply_absolute_threshold(proxies$light, 30)
#' terra::plot(light_above_30$boundaries)
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
    boundaries <- apply_majority_rule_R2022A(boundaries)
  }
  
  return(list(
    rboundaries=boundaries,
    vboundaries=sf::st_as_sf(as.polygons(boundaries)),
    threshold=threshold)
    )
}