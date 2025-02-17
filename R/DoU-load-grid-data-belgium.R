#' Load the grid data for Belgium to reconstruct DEGURBA classification
#' @description
#' The function loads the data required to execute the grid classification of the Degree of Urbanisation for Belgium with the function [`preprocess_grid()`].
#'
#' The data was constructed with the code below:
#' ```{r, eval=FALSE}
#' # download the GHSL data on a global scale
#' download_GHSLdata(output_directory = "inst/extdata/global")
#'
#' # crop the global grid to Belgium
#' crop_GHSLdata(extent = terra::ext(192000, 485000, 5821000, 6030000),
#'               global_directory = "inst/extdata/global",
#'               output_directory = "inst/extdata/belgium")
#' ```
#' @return named list with the data of Belgium required for the grid classification of the Degree of Urbanisation.
#' @examples
#' DoU_load_grid_data_belgium()
#' @export
DoU_load_grid_data_belgium <- function() {
  return(preprocess_grid(system.file("extdata", "belgium", package = "flexurba")))
}

#' Load the grid data for Belgium to reconstruct DEGURBA classification
#' 
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' `load_grid_data_belgium()` has been renamed to `DoU_load_grid_data_belgium()` to create a more consistent API and to better indicate that this function is specifically designed for reconstructing the DEGURBA classification with `classify_grid()`. 
#' @return named list with the data of Belgium required for the grid classification of the Degree of Urbanisation.
#' @keywords internal
#' @export
load_grid_data_belgium <- function() {
  return(DoU_load_grid_data_belgium())
}