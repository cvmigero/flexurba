#' Spatial units of Belgium
#'
#' @description
#' An object of class `sf` with the small spatial units of Belgium (based on data from the [Algemene Directie Statistiek - Statistics Belgium](https://statbel.fgov.be/nl/open-data/statistische-sectoren-2024)). The data is aggregated to municipal level and converted to the Mollweide projection with the following code:
#' ```{r, eval=FALSE}
#' # read the data
#' statsec <- st_read('sh_statbel_statistical_sectors_31370_20240101.geojson')
#' 
#' # aggregate units to munipal level and rename variables
#' units_belgium <- statsec %>%
#'   group_by(cd_munty_refnis) %>%
#'   summarise(UID = first(as.integer(cd_munty_refnis)),
#'             GID_0 = first(cd_country),
#'             NAME_0 = 'Belgium',
#'             GID_1 = first(cd_rgn_refnis),
#'             NAME_1 = first(tx_rgn_descr_nl),
#'             GID_2 = first(cd_prov_refnis),
#'             NAME_2 = first(tx_prov_descr_nl),
#'             GID_3 = first(cd_dstr_refnis),
#'             NAME_3 = first(tx_adm_dstr_descr_nl),
#'             GID_4 = first(cd_munty_refnis),
#'             NAME_4 = first(tx_munty_descr_nl)
#'   ) 
#' 
#' # simplify geometries and convert to Mollweide
#' units_belgium <- units_belgium %>% select(-cd_munty_refnis) %>%
#'   rmapshaper::ms_simplify() %>%
#'   st_transform('ESRI:54009') %>%
#'   rename(geom = geometry)
#' ```
#' @format ## `units_belgium`
#' A object of class `sf` with spatial units for Belgium
#'
#' @source <https://statbel.fgov.be/nl/open-data/statistische-sectoren-2024>
"units_belgium"