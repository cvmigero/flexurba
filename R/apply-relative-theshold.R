#' Identify urban boundaries by applying an relative threshold 
#'
#' @description
#' TODO
#' @param grid SpatRaster
#' @param fun character / function. Function applied to derive a threshold value for each zone. Either as character: 'min', 'max', 'mean', 'median', or 'pX' where X is a percentile value (e.g., p25 for the 25% percentile value"), or for relatively small grids a custom function.
#' @param ... additional arguments passed to fun
#' @param zones character / SpatRaster / sf object with the zones for which a separate threshold value will be derived.
#' @param operator  character. Operator used to enforce the threshold. Either 'greater_than', 'greater_or_equal', 'smaller_than', 'smaller_or_equal' or 'equals'.
#' @param smoothing logical. Whether to smooth the edges of the boundaries. If `TRUE`, the the function `apply_majority_rule` will be employed.
#' @return named list with the following elements:
#' - `rboundaries`: SpatRaster in which cells that are part of an urban unit have a value of '1'
#' - `vboundaries`: sf object with the urban units as polygons
#' - `threshold`: dataframe with per zone the threshold value that is applied
#' @examples
#' proxies <- load_proxies_belgium()
#' 
#' # use the 95 percentile value of the population as threshold
#' pop_above_p95 <- apply_relative_threshold(proxies$pop, 'p95')
#' terra::plot(pop_above_p95$rboundaries)
#' 
#' # use the 95 percentile value per province as threshold
#' province_zones <- convert_zones_to_grid(flexurba::units_belgium, proxies$pop, 'GID_2')
#' pop_above_p95_per_prov <- apply_relative_threshold(proxies$pop, 'p95', zones=province_zones)
#' terra::plot(pop_above_p95_per_prov$rboundaries)
#' @export
apply_relative_threshold <- function(grid, fun, ..., zones=NULL, operator='greater_than', smoothing=TRUE){
  
  # check if spatraster only has 1 layer
  if (terra::nlyr(grid) > 1){
    grid <- grid[[1]]
    warning('The provided grid has multiple layers, be aware that the threshold
            is derived from and applied to the first layer.')
  }  
  
  if (is.null(zones)){
    zones <- grid %>% terra::init(1) 
  } else if (inherits(zones, "sf")) {
    zones <- convert_zones_to_grid(zones, grid) 
  } else if (is.character(zones) && endsWith(zones, '.tif')){
    zones <- terra::rast(zones)
  } else if (is.character(zones)){
    zones <- convert_zones_to_grid(zones, grid)
  }
  zones <- as.numeric(zones)
  
  
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
    
    
    # function is a percentile value
  } else if (grepl("^[p][0-9]+$", fun)){
    percentile <- as.numeric(sub("p", "", fun))
    if (percentile < 0 || percentile > 100){
      stop("Invalid argument: percentile value should be between 0 and 100")
    }
    threshold_per_zone <- terra::zonal(grid, zones, function(x) {
      terra::quantile(x, probs=percentile/100, na.rm = TRUE)
    })
    
    # error
  } else {
    stop("Invalid argument: fun should be 'min', 'max', 'mean', 'median', or 
         'pX' where X is a percentile value (e.g., p25 for the 
         25% percentile value")
  }
  
  threshold_matrix <- as.matrix(threshold_per_zone)
  
  threshold_raster <- terra::classify(zones, threshold_matrix)
  
  applied_threshold <- apply_absolute_threshold(grid, threshold_raster, operator, smoothing)
  applied_threshold$threshold <- threshold_per_zone
  
  return(applied_threshold)
}