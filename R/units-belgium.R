#' GADM spatial units of Belgium
#'
#' @description
#' An object of class `sf` with the small spatial units of Belgium (downloaded from the [GADM website](https://gadm.org/index.html)). The GADM data is filtered and converted to the Mollweide projection with the following code:
#' ```{r, eval=FALSE}
#' # download the GADM data
#' download.file(
#'   "https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip",
#'   "data/GADM/gadm_410-gpkg.zip"
#' )
#'
#' # unzip the file
#' unzip("data/GADM/gadm_410-gpkg.zip",
#'       files = "gadm_410.gpkg",
#'       exdir = "data/GADM")
#'
#' # filter for Belgium, convert to Mollweide, and select only relevant attributes
#' units_belgium <-
#'   sf::st_read('data/GADM/gadm_410.gpkg', quiet = TRUE) %>%
#'   dplyr::filter(COUNTRY == "Belgium") %>%
#'   sf::st_transform('ESRI:54009') %>%
#'   dplyr::select(
#'     c(
#'       "UID",
#'       "GID_0",
#'       "NAME_0",
#'       "GID_1",
#'       "NAME_1",
#'       "GID_2",
#'       "NAME_2",
#'       "GID_3",
#'       "NAME_3",
#'       "GID_4",
#'       "NAME_4"
#'     )
#'   )
#' ```
#' @format ## `units_belgium`
#' A object of class `sf` with the GADM spatial units for Belgium
#'
#' @source <https://gadm.org/download_world.html>
"units_belgium"