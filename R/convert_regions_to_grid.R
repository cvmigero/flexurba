#' Convert regions to a grid
#'
#' @description
#' Convert the value of regions associated with a vector layer to a grid layer.
#' @param regions character / object of class sf. Path to the vector layer with spatial regions, or an object of class sf with spatial regions
#' @param referencegrid Spatraster. Reference grid used to rasterize the values of the regions.
#' @param id character. The column that contains the values of the regions. If `NULL` the first column is used.
#' @return SpatRaster with the regions
#' @examples
#' # load data for urban proxies and regions 
#' proxies <- load_proxies_belgium()
#' regions <- flexurba::units_belgium
#' 
#' # convert Belgian provinces ('GID_2') to a zonal grid
#' gridded_regions <- convert_regions_to_grid(regions, proxies$pop, 'GID_2')
#' terra::plot(gridded_regions)
#' @export
convert_regions_to_grid <- function(regions, referencegrid, id=NULL){
  if (!inherits(regions, "sf")) {
    if (!is.character(regions)) {
      stop('Invalid argument: regions should be the path to a vector layer with 
           spatial units, or an object of class "sf"')
    }
    regions <- sf::st_read(regions, quiet = TRUE)
  }
  if (is.null(id)){
    id <- names(regions)[[1]]
  }
  return(terra::rasterize(terra::vect(regions), referencegrid, id))
}