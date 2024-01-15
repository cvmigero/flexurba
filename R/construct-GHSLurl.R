#' Construct an URL to download data from the GHSL website
#'
#' @description
#' The function constructs the URL to download a data product with certain specifications from the Global Human Settlement Layer (GHSL) website. The following data products are supported:
#' - **BUILT-S**: the built-up area grid
#' - **POP**: the population grid
#' - **LAND**: the land grid (with the proportion of permanent land)
#'
#' The GHSL divides the world into different rectangular areas called "tiles". The argument `tile_id` can be used to specify the spatial extent based on these tiles. If `tile_id` is `NULL`, the URL will be constructed to download the data for the whole world. For more information about the data products and their available specifications (epoch, coordinate system, resolution, ...), see [GHSL Download page](https://ghsl.jrc.ec.europa.eu/download.php).
#' @param type character. Type of the data product: `"BUILT_S"`, `"POP"` or `"LAND"` for the built-up area grid, the population grid and the land grid respectively
#' @param epoch integer. Epoch
#' @param release character. Release code (only release `"R2022A"` and `"R2023A"` are supported)
#' @param crs integer. EPSG code of the coordinate system: for example, `4326` for WGS84 and `54009` for Mollweide.
#' @param resolution integer. Resolution (in meters)
#' @param version vector with the version code and number
#' @param tile_id character. The GHSL tile id representing the spatial extent. If `tile_id` is `NULL`, the URL is constructed for the whole world.
#' @return URL
#' @examples
#' construct_GHSLurl("POP", epoch = 2000, resolution = 100)
#' construct_GHSLurl("LAND", epoch = 2018, resolution = 1000, tile_id = "R3_C19")
#' @noRd
construct_GHSLurl <- function(type, epoch = 2020, release = "R2023A", crs = 54009, resolution = 1000, version = c("V1", "0"), tile_id = NULL) {
  # check if type is valid
  if (!(type %in% c("BUILT_S", "POP", "LAND"))) {
    stop("Invalid argument: type should be BUILT_S, POP or LAND")
  }
  
  # check if tile_id is valid
  if (!is.null(tile_id)) {
    if (!is_GHSLtile(tile_id)) {
      stop("Invalid argument: tile_id is not valid")
    }
  }
  
  baseurl <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/"
  part1 <- paste0("GHS_", type, "_GLOBE_", release, "/")
  if (type == "BUILT_S" & release == "R2022A") {
    part2 <- part1
  } else {
    part2 <- ""
  }
  part3 <- paste0("GHS_", type, "_E", epoch, "_GLOBE_", release, "_", crs, "_", resolution, "/")
  part4 <- paste0(paste(c("V1", "0"), collapse = "-"), "/")
  if (is.null(tile_id)) {
    # global grid
    part5 <- paste0("GHS_", type, "_E", epoch, "_GLOBE_", release, "_", crs, "_", resolution, "_", paste(c("V1", "0"), collapse = "_"))
  } else {
    part5 <- paste0("tiles/GHS_", type, "_E", epoch, "_GLOBE_", release, "_", crs, "_", resolution, "_", paste(c("V1", "0"), collapse = "_"), "_", tile_id)
  }
  part6 <- ".zip"
  return(paste0(baseurl, part1, part2, part3, part4, part5, part6))
}