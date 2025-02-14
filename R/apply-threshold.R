#' Identify urban boundaries by applying an relative threshold 
#'
#' @description
#' TODO
#' @param grid SpatRaster
#' @param type character. Either 'predefined' or 'data-driven'.
#' @param threshold_value numeric or vector. If `type="predefined"`, this threshold value is applied to derive urban boundaries. Be aware the threshold value should be in the same unit as the grid values. If `zones` is not `NULL`, you can also pass a vector of threshold values which should be applied in the respective zones.
#' @param fun character / function. If `type="data-driven"`, this function is used to derive the threshold value. Either as character: 'min', 'max', 'mean', 'median', or 'pX' where X is a percentile value (e.g., p25 for the 25% percentile value"), or for relatively small grids a custom function.
#' @param ... additional arguments passed to fun
#' @param zones character / SpatRaster / sf object. If  with the zones for which a separate threshold value will be derived.
#' @param operator  character. Operator used to enforce the threshold. Either 'greater_than', 'greater_or_equal', 'smaller_than', 'smaller_or_equal' or 'equals'.
#' @param smoothing logical. Whether to smooth the edges of the boundaries. If `TRUE`, the the function `apply_majority_rule` will be employed to smooth the boundaries.
#' @return named list with the following elements:
#' - `rboundaries`: SpatRaster in which cells that are part of an urban unit have a value of '1'
#' - `vboundaries`: sf object with the urban units as polygons
#' - `threshold`: dataframe with per zone the threshold value that is applied
#' - `zones`: SpatRaster with the zones
#' @examples
#' proxies <- load_proxies_belgium()
#' 
#' # option 1: predefined - absolute threshold
#' predefined_absolute <- apply_threshold(proxies$pop, type='predefined',
#'                                        threshold_value=1500)
#' terra::plot(predefined_absolute$rboundaries)
#' 
#' # option 2: data-driven - absolute threshold
#' datadriven_absolute <- apply_threshold(proxies$pop, type='data-driven', 
#'                                        threshold_value='p95')
#' terra::plot(datadriven_absolute$rboundaries)
#' 
#' # in the examples below we will use Brussels, Flanders and the Walloon 
#' # Region as separate zones
#' zones <- convert_zones_to_grid(flexurba::units_belgium, proxies$pop, 'GID_1')
#' terra::plot(zones)
#' 
#' # option 3: predefined - relative threshold
#' predefined_relative <- apply_threshold(proxies$pop, type='predefined'
#'                                        threshold_value=c(1500, 1200, 1000),
#'                                        zones=zones)
#' terra::plot(predefined_relative$rboundaries)
#' 
#' # option 4: data-driven - relative threshold
#' datadriven_relative <- apply_threshold(proxies$pop, type='data-driven'
#'                                        fun='p95,
#'                                        zones=zones)
#' terra::plot(datadriven_relative$rboundaries)
#' @export
apply_threshold <- function(grid, type="predetermined", threshold_value=NULL, fun=NULL, ..., zones=NULL, operator='greater_than', smoothing=TRUE){

  # check if spatraster only has 1 layer
  if (terra::nlyr(grid) > 1){
    grid <- grid[[1]]
    warning('The provided grid has multiple layers, be aware that the threshold
            is derived from and applied to the first layer.')
  }

  # PROCESS ZONES
  if (is.null(zones)){
    zones <- grid %>% terra::init(1) 
  } else if (inherits(zones, "sf")) {
    zones <- convert_zones_to_grid(zones, grid) 
  } else if (is.character(zones) && endsWith(zones, '.tif')){
    zones <- terra::rast(zones)
  } else if (is.character(zones)){
    zones <- convert_zones_to_grid(zones, grid)
  }
  
  numzones <- as.numeric(zones)
  zone_value <- unique(terra::values(zones, na.rm=TRUE)) %>% sort()
  nr_of_zones <- length(zone_value)
  zone_levels <- terra::levels(zones)[[1]]
  
  # CHECK ARGUMENTS
  check_arguments(type, threshold_value, nr_of_zones, fun)
  
  
  # PREDEFINED THRESHOLD
  if (type=="predefined"){
    threshold_per_zone <- cbind(zone_value, threshold_value)
   
  
  
  # DATA-DRIVEN THRESHOLD
  } else if (type=="data-driven"){
    threshold_per_zone <- derive_data_driven_threshold(grid, numzones, fun, ...)
  } 
  
  names(threshold_per_zone) <- c("zone_value", "threshold_value")
  
  threshold_matrix <- as.matrix(threshold_per_zone)
  threshold_raster <- terra::classify(zones, threshold_matrix)
  
  applied_threshold <- list()
  applied_threshold$rboundaries <- compare_grid_to_threshold(grid, threshold_raster, 
                                                             operator, smoothing)
  applied_threshold$vboundaries <- sf::st_as_sf(terra::as.polygons(applied_threshold$rboundaries))
  
  if (all(zone_levels == "")){
    applied_threshold$threshold = threshold_per_zone
    
  # add zone name to the dataframe
  } else {
    applied_threshold$threshold <- merge(threshold_per_zone, zone_levels, 
                                         by.x='zone_value', by.y='ID')
  }
  
  return(applied_threshold)
}


check_arguments <- function(type, threshold_value, nr_of_zones, fun){
  
  if (!(type %in% c("predefined", "data-driven"))){
    stop("Invalid argument: type should be 'predefined' or 'data-driven'")
  }
  
  if (type == "predefined"){
    
    # a threshold_value should be provided
    if (is.null(threshold_value)){
      stop("Invalid arguments: if type is 'predefined', then a threshold_value should be provided.")
    }
    
    # check if provided threshold_value is correct format
    if (!is.numeric(threshold_value) | !(length(threshold_value) %in% c(1, nr_of_zones))){
      stop(paste("Invalid argument: the threshold_value should be a numeric value or a 
                 vector of values with the same length as the number of zones."))
    }
    
    # warning if function is provided
    if (!is.null(fun)){
      warning('A function is provided, but be aware it will not be used as the threshold type is "predefined".')
    }
  }
  
  if (type == "data-driven"){
    
    # a function should be provided
    if(is.null(fun)){
      stop("Invalid arguments: if type is 'data-driven', then a function should be provided.")
    }
    
    # warning if threshold is provided
    if (!is.null(threshold_value)){
      warning('A threshold value is provided, but be aware it will not be used as the threshold type is "data-driven".')
    }
  }
}


derive_data_driven_threshold <- function(grid, zones, fun, ...){
  
  # if fun is a custom function
  if (inherits(fun, 'function')){
    threshold_per_zone <- tryCatch({
      terra::zonal(grid, zones, fun, ..., na.rm=TRUE)
      
      # if an error occurs, add custom message
    }, error = function(e) {
      stop(paste("Invalid function:", e$message))
    })
    
  # if fun is max, min or mean
  } else if (fun %in% c('max', 'min', 'mean')){
    threshold_per_zone <- terra::zonal(grid, zones, fun, na.rm=TRUE)
    
  # if function is median
  } else if (fun == "median"){
    threshold_per_zone <- terra::zonal(grid, zones, function(x) {
      terra::quantile(x, probs=0.5, na.rm = TRUE)
    })
    
  # if function is a percentile value
  } else if (grepl("^[p][0-9]+$", fun)){
    percentile <- as.numeric(sub("p", "", fun))
    if (percentile < 0 || percentile > 100){
      stop("Invalid argument: percentile value should be between 0 and 100")
    }
    threshold_per_zone <- terra::zonal(grid, zones, function(x) {
      terra::quantile(x, probs=percentile/100, na.rm = TRUE)
    })
    
  # else error
  } else {
    stop("Invalid argument: fun should be 'min', 'max', 'mean', 'median', or 
           'pX' where X is a percentile value (e.g., p25 for the 
           25% percentile value")
  }
  
  return(threshold_per_zone)
}


compare_grid_to_threshold <- function(grid, threshold, operator='greater_than', smoothing=TRUE){
  
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
  
  return(boundaries)
}
