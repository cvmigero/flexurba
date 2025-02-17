#' Identify urban areas by applying a threshold on grid cells
#'
#' @description
#' The function identifies urban areas by applying a threshold to individual grid cells. Two key decisions must be made regarding the thresholding approach:
#'
#' 1. **How is the threshold value determined?**  
#'    - The threshold can be *predefined by the user* (`type="predefined"`) or *derived from the data* (`type="data-driven"`).
#'
#' 2. **Where is the threshold enforced?**  
#'    - The threshold can be enforced *consistently across the study area* (= absolute approach, `regions=NULL`) or *tailored within specific regions* (= relative approach, `regions` not `NULL`).
#'
#' For more details on these thresholding approaches, including their advantages and limitations, see the section "Thresholding Approaches" below. The table below outlines the appropriate combination of function arguments for each approach:
#'
#' |           | **Absolute Approach**                                            | **Relative Approach**                                            |
#' |-------------------------|-----------------------------------------------------------------|-----------------------------------------------------------------|
#' | **Predefined Value**    | `type="predefined"` with `threshold_value` not `NULL`, and `regions=NULL` | `type="predefined"` with `threshold_value` not `NULL`, and `regions` not `NULL` |
#' | **Data-Driven Value**   | `type="data-driven"` with `fun` not `NULL`, and `regions=NULL` | `type="data-driven"` with `fun` not `NULL`, and `regions` not `NULL` |
#'
#' @param grid SpatRaster with the data
#' @param type character. Either `"predefined"` or `"data-driven"`.
#' @param threshold_value numeric or vector. The threshold value used to identify urban areas when `type="predefined"`. If `regions` is not `NULL`, a vector of threshold values can be provided, where each value corresponds to a specific region (the respective values are linked to regions in alphabetical order based on their IDs; see examples). In addition, ensure that the threshold values are in the same unit as the grid values.
#' @param fun character or function. This function is used to derive the threshold value from the data when `type="data-driven"`. Either as character: `"min"`, `"max"`, `"mean"`, `"median"`, or `"pX"` where X denotes a percentile value (e.g., p95 for the 95% percentile value"). It is also possible to provide a custom function for relatively small grids.
#' @param ... additional arguments passed to fun
#' @param regions character, SpatRaster or sf object. If not `NULL`, a different threshold value is applied in the separate regions (i.e. a relative thresholding approach). The argument can either be:
#' - a SpatRaster with a separate value for each region
#' - the path to the region data (as character)
#' - an sf polygon layer
#' In the latter two cases, the function [covert_regions_to_grid()] will be used to convert the regions to a gridded format.
#' @param operator  character. Operator used to enforce the threshold. Either `"greater_than"`, `"greater_or_equal"`, `"smaller_than"`, `"smaller_or_equal"` or `"equals"`.
#' @param smoothing logical. Whether to smooth the edges of the boundaries. If `TRUE`, boundaries will be smoothed with the function [apply_majority_rule()].
#' @return named list with the following elements:
#' - `rboundaries`: SpatRaster in which cells that are part of an urban area have a value of '1'
#' - `vboundaries`: sf object with the urban areas as separate polygons
#' - `threshold`: dataframe with per region the threshold value that is applied
#' - `regions`: SpatRaster with the gridded version of the regions that is employed
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
#'                                        fun='p90')
#' terra::plot(datadriven_absolute$rboundaries)
#' 
#' # in the examples below we will use 'Bruxelles', 'Vlaanderen' and 'Wallonie' as separate regions
#' regions <- convert_regions_to_grid(flexurba::units_belgium, proxies$pop, 'NAME_1')
#' terra::plot(regions)
#' 
#' # option 3: predefined - relative threshold
#' # note that the threshold values are linked to the regions in alphabetical 
#' # order based on their IDs. So, the threshold of 1500 is applied to 
#' # 'Bruxelles', # 1200 to 'Vlaanderen', and 1000 to 'Wallonie'.
#' predefined_relative <- apply_threshold(proxies$pop, type='predefined',
#'                                        threshold_value=c(1500, 1200, 1000),
#'                                        regions=regions)
#' terra::plot(predefined_relative$rboundaries)
#' 
#' # option 4: data-driven - relative threshold
#' datadriven_relative <- apply_threshold(proxies$pop, type='data-driven',
#'                                        fun='p95',
#'                                        regions=regions)
#' terra::plot(datadriven_relative$rboundaries)
#' 
#' @section Thresholding approaches:
#' 
#' Thresholding approaches for urban delineation can vary across two dimensions: (1) how the threshold value is determined and (2) where the threshold is enforced. 
#' 
#' There are two main ways to determine a threshold's value. The value can either be set by the researcher or analyst, typically based on expert knowledge, or it can be derived from the underlying data. There is considerable debate regarding whether predefined or data-driven thresholds should be preferred for urban delineation. Predefined thresholds are easier to understand and implement and thus contribute to a higher degree of transparency. However, their exact value is often difficult to justify conceptually, and their value is relatively easy to modify. Data-driven thresholds, on the other hand, are more complex to manually adjust and are therefore perceived as less arbitrary. Nevertheless, it is important to note that concerns related to arbitrariness are not eliminated as the user still needs to decide on a particular function to derive the data-driven value. In that sense, arbitrariness is moved from the user into the delineation method.
#' 
#' The second dimension concerns where the threshold is enforced. Thresholds can be applied consistently across the study area (i.e. an absolute approach) or separately in individual regions (i.e. a relative approach). An example of the latter approach is when different thresholds are applied for different countries. This approach allows a threshold to be tailored to the specific urbanisation pattern of a country. Nevertheless, customised thresholds hamper comparability across space. In constrast, absolute thresholds allow for more meaningful comparisons as they ensure that urban areas are identified consistently across space. 
#' 
#' Combing these two dimensions results in four possible approaches: 
#' 
#' 1.	**A predefined, absolute approach** 
#' 
#'      The Degree of Urbanisation definition developed by [Dijkstra et al. (2021)](https://www.sciencedirect.com/science/article/pii/S0094119020300838) employs this approach and consistently applies a predefined population density threshold of 1500 inhabitants per km² across the globe. 
#'      
#' 2. **A predefined, relative approach** 
#' 
#'      The [OECD (2013)](https://www.oecd.org/content/dam/oecd/en/publications/reports/2013/12/oecd-regions-at-a-glance-2013_g1g356f6/reg_glance-2013-en.pdf) specifies two predefined thresholds: a minimum of 1000 inhabitants per km² for the United States and Canada, and a threshold of 1500 inhabitants per km² for other OECD countries.
#'      
#' 3. **A data-driven, absolute approach** 
#' 
#'      [Jiang et al. (2015)](https://www.tandfonline.com/doi/full/10.1080/13658816.2014.988715) derive a minimum night-time light emission threshold from the data, and enforce this threshold consistently across the globe. 
#'      
#' 4. **A data-driven, relative approach** 
#' 
#'      [Combes et al. (2024)](https://documents1.worldbank.org/curated/en/099415311272320571/pdf/IDU0faef6c000aaba0485209f0e08928760d9a57.pdf) determine a separate data-driven population density threshold for each country in Sub-Saharan Africa.
#'
#' There is no definitive answer to what should be preferred: a predefined or data-driven threshold, enforced in an absolute or relative manner. Each approach possess unique advantages yet also come with limitations. Ultimately, the applicability should depend on the purpose of the delineation and the context of the application. 
#' 
#' @export
apply_threshold <- function(grid, type="predefined", threshold_value=NULL, fun=NULL, ..., regions=NULL, operator='greater_than', smoothing=TRUE){

  # check if spatraster only has 1 layer
  if (terra::nlyr(grid) > 1){
    grid <- grid[[1]]
    warning('The provided grid has multiple layers, be aware that the threshold
            is derived from and applied to the first layer.')
  }

  # PROCESS regions
  if (is.null(regions)){
    regions <- grid %>% terra::init(1) 
  } else if (inherits(regions, "sf")) {
    regions <- convert_regions_to_grid(regions, grid) 
  } else if (is.character(regions) && endsWith(regions, '.tif')){
    regions <- terra::rast(regions)
  } else if (is.character(regions)){
    regions <- convert_regions_to_grid(regions, grid)
  }
  
  numregions <- as.numeric(regions)
  region_value <- unique(terra::values(regions, na.rm=TRUE)) %>% sort()
  nr_of_regions <- length(region_value)
  region_levels <- terra::levels(regions)[[1]]
  
  # CHECK ARGUMENTS
  check_arguments(type, threshold_value, nr_of_regions, fun)
  
  
  # PREDEFINED THRESHOLD
  if (type=="predefined"){
    threshold_per_region <- cbind(region_value, threshold_value)
   
  
  
  # DATA-DRIVEN THRESHOLD
  } else if (type=="data-driven"){
    threshold_per_region <- derive_data_driven_threshold(grid, numregions, fun, ...)
  } 
  
  names(threshold_per_region) <- c("region_value", "threshold_value")
  
  threshold_matrix <- as.matrix(threshold_per_region)
  threshold_raster <- terra::classify(regions, threshold_matrix)
  
  applied_threshold <- list()
  applied_threshold$rboundaries <- compare_grid_to_threshold(grid, threshold_raster, 
                                                             operator, smoothing)
  applied_threshold$vboundaries <- sf::st_as_sf(terra::as.polygons(applied_threshold$rboundaries))
  
  if (all(region_levels == "")){
    applied_threshold$threshold = threshold_per_region
    
  # add region name to the dataframe
  } else {
    applied_threshold$threshold <- merge(threshold_per_region, region_levels, 
                                         by.x='region_value', by.y='ID')
  }
  
  return(applied_threshold)
}


check_arguments <- function(type, threshold_value, nr_of_regions, fun){
  
  if (!(type %in% c("predefined", "data-driven"))){
    stop("Invalid argument: type should be 'predefined' or 'data-driven'")
  }
  
  if (type == "predefined"){
    
    # a threshold_value should be provided
    if (is.null(threshold_value)){
      stop("Invalid arguments: if type is 'predefined', then a threshold_value should be provided.")
    }
    
    # check if provided threshold_value is correct format
    if (!is.numeric(threshold_value) | !(length(threshold_value) %in% c(1, nr_of_regions))){
      stop(paste("Invalid argument: the threshold_value should be a numeric value or a 
                 vector of values with the same length as the number of regions."))
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


derive_data_driven_threshold <- function(grid, regions, fun, ...){
  
  # if fun is a custom function
  if (inherits(fun, 'function')){
    threshold_per_region <- tryCatch({
      terra::zonal(grid, regions, fun, ..., na.rm=TRUE)
      
      # if an error occurs, add custom message
    }, error = function(e) {
      stop(paste("Invalid function:", e$message))
    })
    
  # if fun is max, min or mean
  } else if (fun %in% c('max', 'min', 'mean')){
    threshold_per_region <- terra::zonal(grid, regions, fun, na.rm=TRUE)
    
  # if function is median
  } else if (fun == "median"){
    threshold_per_region <- terra::zonal(grid, regions, function(x) {
      terra::quantile(x, probs=0.5, na.rm = TRUE)
    })
    
  # if function is a percentile value
  } else if (grepl("^[p][0-9]+$", fun)){
    percentile <- as.numeric(sub("p", "", fun))
    if (percentile < 0 || percentile > 100){
      stop("Invalid argument: percentile value should be between 0 and 100")
    }
    threshold_per_region <- terra::zonal(grid, regions, function(x) {
      terra::quantile(x, probs=percentile/100, na.rm = TRUE)
    })
    
  # else error
  } else {
    stop("Invalid argument: fun should be 'min', 'max', 'mean', 'median', or 
           'pX' where X is a percentile value (e.g., p25 for the 
           25% percentile value")
  }
  
  return(threshold_per_region)
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
