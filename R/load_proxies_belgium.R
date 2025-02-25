#' Load the data for three urban proxies for Belgium
#' @description
#' The function loads a population and built-up grid from the [GLobal Human Settlement Layer](https://human-settlement.emergency.copernicus.eu/download.php) and a night-time light grid from the [Earth Observation Group](https://eogdata.mines.edu/products/vnl/#annual_v2).
#' 
#' The specification of the source data is as follows:
#' 
#' The night-time light grid was processed with the following code to ensure that it aligns with the population and built-up layer. 
#' ```{r, eval=FALSE}
#' # POPULATION DATA
#' terra::rast(system.file("proxies/processed-ghs-pop.tif', package = "flexurbaData")) %>% 
#'    terra::crop(terra::ext(187000, 490000, 5816000, 6035000)) %>%
#'    terra::writeRaster('inst/extdata/belgium/processed-ghs-pop-belgium.tif')  
#'    
#' # BUILT-UP AREA DATA
#' terra::rast(system.file("proxies/processed-ghs-built-s.tif', package = "flexurbaData")) %>% 
#'    terra::crop(terra::ext(187000, 490000, 5816000, 6035000)) %>%
#'    terra::writeRaster('inst/extdata/belgium/processed-ghs-built-s-belgium.tif')  
#' 
#' # LIGHT DATA
#' terra::rast(system.file("proxies/processed-viirs-light.tif', package = "flexurbaData")) %>% 
#'    terra::crop(terra::ext(187000, 490000, 5816000, 6035000)) %>%
#'    terra::writeRaster('inst/extdata/belgium/processed-viirs-light-belgium.tif')  
#' ```
#' @return named list with gridded population, built-up area and night-time light data for Belgium.
#' @examples
#' load_proxies_belgium()
#' @export
load_proxies_belgium <- function() {
  return(list(
    pop = terra::rast(system.file("extdata", "belgium", 'processed-ghs-pop-belgium.tif', package = "flexurba")),
    built = terra::rast(system.file("extdata", "belgium", 'processed-ghs-built-s-belgium.tif', package = "flexurba")),
    light = terra::rast(system.file("extdata", "belgium", 'processed-viirs-light-belgium.tif', package = "flexurba"))
  ))
}
