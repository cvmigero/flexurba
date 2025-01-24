#' Identify urban boundaries by applying an relative threshold 
#'
#' @description
#' TODO
#' @param grid 
#' @param fun 
#' @param ... additional arguments passed to fun, such as `na.rm=TRUE`
#' @param zones 
#' @param operator
#' @param smoothing 
#' @return SpatRaster with the urban areas
#' @examples
#' 
#' @export
apply_relative_threshold <- function(grid, fun, ..., zones=NULL, operator='greater_than', smoothing=TRUE){
  
  # check if spatraster only has 1 layer
  if (nlyr(grid) > 1){
    grid <- grid[[1]]
    warning('The provided grid has multiple layers, be aware that the threshold
            is derived from and applied to the first layer.')
  }  
  
  if (is.null(zones)){
    zones <- grid %>% init(1) 
  } else if (inherits(zones, "sf")) {
    zones <- convert_zones_to_grid(zones, grid) 
  } else if (is.character(zones) & endsWith(zones, '.tif')){
    zones <- terra::rast(zones)
  } else if (is.character(zones)){
    zones <- convert_zones_to_grid(zones, grid)
  }
  zones <- as.numeric(zones)
  
  
  # function is maximum, minimum or mean
  if (fun %in% c('max', 'min', 'mean')){
    threshold_per_zone <- terra::zonal(grid, zones, fun, na.rm=TRUE)
    
    # function is median
  } else if (fun == "median"){
    threshold_per_zone <- terra::zonal(grid, zones, function(x) {
      quantile(x, probs=0.5, na.rm = TRUE)
    })
    
    
    # function is a percentile value
  } else if (grepl("^[p][0-9]+$", fun)){
    percentile <- as.numeric(sub("p", "", fun))
    if (percentile < 0 || percentile > 100){
      stop("Invalid argument: percentile value should be between 0 and 100")
    }
    threshold_per_zone <- terra::zonal(grid, zones, function(x) {
      quantile(x, probs=percentile/100, na.rm = TRUE)
    })
    
    # error
  } else if (is.character(fun)){
    stop("Invalid argument: fun should be 'min', 'max', 'mean', 'median', or 
         'pX' where X is a percentile value (e.g., p25 for the 
         25% percentile value")
  } else {
    threshold_per_zone <- terra::zonal(grid, zones, fun, ..., na.rm=TRUE)
  }
  
  threshold_per_zone <- as.matrix(threshold_per_zone)
  
  threshold_raster <- terra::classify(zones, threshold_per_zone)
  
  return(apply_absolute_threshold(grid, threshold_raster, operator, smoothing))
}