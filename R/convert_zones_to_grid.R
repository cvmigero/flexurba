#' Convert zones to a grid
#'
#' @description
#' Convert the value of zones associated with a vector layer to a grid layer.
#' @param zones character / object of class sf. Path to the vector layer with spatial zones, or an object of class sf with spatial zones
#' @param referencegrid Spatraster. Reference grid used to rasterize the values of the zones.
#' @param id character. The column that contains the values of the zones. If `NULL` the first column is used.
#' @return SpatRaster with the zones
#' @examples
#' # load grid and units data for belgium
#' data_belgium <- load_grid_data_belgium()
#' units <- flexurba::units_belgium
#' 
#' # convert Belgian provinces ('GID_2') to a zonal grid
#' gridded_zones <- convert_zones_to_grid(units, data_belgium$pop, 'GID_2')
#' terra::plot(gridded_zones)
#' @export
convert_zones_to_grid <- function(zones, referencegrid, id=NULL){
  if (!inherits(zones, "sf")) {
    if (!is.character(zones)) {
      stop('Invalid argument: zones should be the path to a vector layer with 
           small spatial units, or an object of class "sf"')
    }
    zones <- sf::st_read(zones, quiet = TRUE)
  }
  if (is.null(id)){
    id <- names(zones)[[1]]
  }
  return(terra::rasterize(terra::vect(zones), referencegrid, id))
}