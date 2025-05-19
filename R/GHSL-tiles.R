#' A dataframe with existing GHSL tiles
#'
#' @description
#' The Global Human Settlement Layer (GHSL) divides the world into different rectangular areas called "tiles". These tiles can be used to download the GHSL products from the website (see [GHSL Download page](https://ghsl.jrc.ec.europa.eu/download.php)).
#'
#' The dataset `GHSL_tiles` contains the geometry and metadata of all valid tiles.
#'
#' @format ## `GHSL_tiles`
#' A dataframe with 375 rows and 6 columns
#' \describe{
#'   \item{tile_id}{id of the tile}
#'   \item{left, top, right, bottom}{The coordinates of the bounding box of the tile (in Mollweide, EPSG: 54009)}
#'   \item{geometry}{`sfc_POLYGON` geometry of the tile}
#' }
#' @source <https://ghsl.jrc.ec.europa.eu/download/GHSL_data_54009_shapefile.zip>
#' @details
#' Spatial distribution of the GHSL tiles:
#' \figure{figure_GHSL_tiles.png}{GHSL tiles}
"GHSL_tiles"




#' Check if a tile is a valid GHSL tile
#' @description
#' The Global Human Settlement Layer (GHSL) divides the world into different rectangular areas called "tiles". The function checks if the `tile_id` represents a valid GHSL tile.
#'
#' Because GHSL data is only available in land areas, not all combinations of rows and columns are valid. For more information about the GHSL tiles and their extent see [GHSL Download page](https://ghsl.jrc.ec.europa.eu/download.php) and [GHSL_tiles].

#' @param tile_id character
#' @return logical. Whether the tile is a valid GHSL tile
#' @noRd
is_GHSLtile <- function(tile_id) {
  return(tile_id %in% flexurba::GHSL_tiles$tile_id)
}
