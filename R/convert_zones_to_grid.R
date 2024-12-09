#' Convert zones to a grid
#'
#' @description
#' TODO
#' @param zones 
#' @param id 
#' @param referencegrid 
#' @return SpatRaster with the zones
#' @examples
#' 
#' @export
convert_zones_to_grid <- function(zones, id, referencegrid){
  return(terra::rasterize(terra::vect(zones, referencegrid, id)))
}