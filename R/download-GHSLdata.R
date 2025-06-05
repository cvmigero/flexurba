#' Download data products from the GHSL website
#'
#' @description
#' The function will download data products with certain specifications from the Global Human Settlement Layer (GHSL) website. The following data products are supported:
#'
#' - **BUILT_S**: the built-up area grid
#' - **POP**: the population grid
#' - **LAND**: the land grid (with the proportion of permanent land)
#'
#' These are also the three data products that are required for the grid cell classification of the Degree of Urbanisation. For more information about the data products and their available specifications, see [GHSL Download page](https://ghsl.jrc.ec.europa.eu/download.php). The downloaded data will be saved in the `output_directory` together with a JSON metadata-file. This function downloads large volumes of data, make sure the timeout parameter is sufficiently high (for example: `options(timeout=500)`).
#'
#' *Note that the land grid is only available for epoch 2018 and release R2022A on the GHSL website. The land grid will consequently always be downloaded with these specifications, regardless of the epoch and release specified in the arguments (a warning message is printed).*
#' @param output_directory character. Path to the output directory
#' @param products vector with the types of the data products: `"BUILT_S"`, `"POP"` and/or `"LAND"` for the built-up area grid, the population grid and the land grid respectively
#' @param epoch integer. Epoch
#' @param release character. Release code (only release `"R2022A"` and `"R2023A"` are supported)
#' @param crs integer. EPSG code of the coordinate system: for example, `54009` for Mollweide.
#' @param resolution integer. Resolution (in meters for Mollweide projection).
#' @param version vector with the version code and number
#' @param extent character or vector representing the spatial extent. There are three possibilities:
#'
#' - `extent = "global"`: The data is downloaded on a global scale.
#' - `extent` is a vector of GHSL tile ids: The data is downloaded for each tile separately and afterwards merged together. For more information about the GHSL tiles and their extent see [`GHSL_tiles`] or [GHSL Download page](https://ghsl.jrc.ec.europa.eu/download.php).
#' - `extent = "regions"`: The data will be downloaded in 9 pre-defined regions. The pre-defined regions are the smallest grouping of GHSL tiles possible while ensuring that no continuous land mass is split over two regions. The regions are constructed to execute the Degree of Urbanisation classification algorithms in a memory-efficient manner. For each of the regions, the data products will be downloaded and saved in a sub-directory of `output_directory` (e.g., for region `W_AME`, the directory `output_directory/W_AME` is created). For more information, see the documentation of [`GHSL_tiles_per_region`].
#' @param filenames character. Filenames for the output files
#' @return path to the created files.
#' @examples
#' \donttest{
#' # Download the population grid for epoch 2000 for specific tiles
#' download_GHSLdata(
#'   output_directory = tempdir(),
#'   filename = "POP_2000.tif",
#'   products = "POP",
#'   extent = c("R3_C19", "R4_C19"),
#'   epoch = 2000,
#' )
#' \dontshow{
#'  unlink(file.path(tempdir(), 'POP_2000.tif'))
#'  unlink(file.path(tempdir(), 'POP_2000.json'))
#' }
#' }
#' @export
download_GHSLdata <- function(
  output_directory,
  products = c("POP", "BUILT_S", "LAND"),
  epoch = 2020,
  release = "R2023A",
  crs = 54009,
  resolution = 1000,
  version = c("V1", "0"),
  extent = "global",
  filenames = c("POP.tif", "BUILT_S.tif", "LAND.tif")
) {
  # create metadata list
  metadata <- as.list(environment())
  metadata <- within(
    metadata,
    rm(output_directory, products, extent, filenames)
  )
  metadata$version <- paste(metadata$version, collapse = "_")

  # if directory does not exist: create it
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  # DOWNLOAD IN REGIONS
  if (all(extent == "regions")) {
    # download the data for each region separately
    for (region in names(flexurba::GHSL_tiles_per_region)) {
      download_GHSLdata(
        file.path(output_directory, region),
        products = products,
        epoch = epoch,
        release = release,
        crs = crs,
        resolution = resolution,
        version = version,
        extent = flexurba::GHSL_tiles_per_region[[region]],
        filenames = filenames
      )
    }
  } else {
    # check the arguments
    if (length(products) != length(filenames)) {
      stop(
        "Invalid argument: the length of the products and filenames should be equal."
      )
    }

    # for each product, download the data
    nr <- 0
    for (type in products) {
      nr <- nr + 1

      # check if type is valid
      if (!(type %in% c("BUILT_S", "POP", "LAND"))) {
        stop(
          "Invalid argument: the supported data products are BUILT_S, POP or LAND"
        )
      }

      # LAND grid only exists for 2018 - R2022A
      if (type == "LAND") {
        if (epoch != 2018) {
          epoch <- 2018
          metadata$epoch <- 2018
          warning(
            "Land grid only exists for epoch 2018. The land grid of 2018 will be downloaded. \n"
          )
        }

        if (release != "R2022A") {
          release <- "R2022A"
          metadata$release <- "R2022A"
          warning(
            "Land grid only exists for release R2022A. The land grid of R2022A will be downloaded. \n"
          )
        }

        # check if the filename is valid
        if (!endsWith(filenames[[nr]], ".tif")) {
          stop("Invalid agrument: filenames should have extention .tif")
        }
      }

      # construct target file
      metadata$file <- file.path(output_directory, filenames[[nr]])
      if (file.exists(metadata$file)) {
        stop(paste("Invalid argument:", metadata$file, "already exists"))
      }

      # construct filename based on specifications
      GHSfilename <- paste0(
        "GHS_",
        type,
        "_E",
        epoch,
        "_GLOBE_",
        release,
        "_",
        crs,
        "_",
        resolution,
        "_",
        paste(c("V1", "0"), collapse = "_")
      )

      # DOWNLOAD GLOBAL DATA
      if (all(extent == "global")) {
        metadata$extent <- c("global")
        tiffile <- file.path(output_directory, paste0(GHSfilename, ".tif"))
        zipfile <- file.path(output_directory, paste0(GHSfilename, ".zip"))

        # construct url
        metadata$url <- construct_GHSLurl(
          type = type,
          epoch = epoch,
          release = release,
          crs = crs,
          resolution = resolution,
          version = version
        )
        # download the data
        suppressWarnings(
          err <- try(
            utils::download.file(metadata$url, zipfile, quiet = TRUE),
            silent = TRUE
          )
        )
        if (inherits(err, "try-error")) {
          if (grepl("failed", err[1])) {
            # timeout error
            stop(
              "Download timeout was reached. Please increase the timeout parameter with 'options(timeout=500)' or higher. \n"
            )
          } else {
            # URL does not exist
            stop(paste0(
              "An error occurred when reaching the GHSL server with the following URL: ",
              metadata$url,
              ". Please check if the product with the given specifications exists on the GHSL website or alternatively download the data from https://ghsl.jrc.ec.europa.eu/download.php. \n"
            ))
          }
        }
        metadata$download_time <- format(Sys.time(), "%a %b %d %X %Y")
        utils::unzip(
          zipfile,
          files = paste0(GHSfilename, ".tif"),
          exdir = output_directory
        )
        file.remove(zipfile)
        file.rename(tiffile, metadata$file)

        # DOWNLOAD FROM TILE IDS
      } else {
        metadata$extent <- c()
        grids <- list()
        i <- 0
        to_remove <- c()

        # download data for each tile
        for (tile in extent) {
          if (is_GHSLtile(tile)) {
            metadata$extent <- append(metadata$extent, tile)
            tiffile <- file.path(
              output_directory,
              paste0(GHSfilename, "_", tile, ".tif")
            )
            zipfile <- file.path(
              output_directory,
              paste0(GHSfilename, "_", tile, ".zip")
            )
            metadata$url <- construct_GHSLurl(
              type,
              epoch = epoch,
              release = release,
              crs = crs,
              resolution = resolution,
              version = version,
              tile_id = tile
            )
            # download the data
            suppressWarnings(
              err <- try(
                utils::download.file(metadata$url, zipfile, quiet = TRUE),
                silent = TRUE
              )
            )
            if (inherits(err, "try-error")) {
              if (grepl("failed", err[1])) {
                # timeout error
                stop(
                  "Download timeout was reached. Please increase the timeout parameter with 'options(timeout=500)' \n"
                )
              } else {
                # URL does not exist
                stop(paste0(
                  "URL: ",
                  metadata$url,
                  " does not exist, no GHSL product exists with the given specifications. Please check https://ghsl.jrc.ec.europa.eu/download.php to see which files exist. \n"
                ))
              }
            }
            metadata$download_time <- format(Sys.time(), "%a %b %d %X %Y")
            utils::unzip(
              zipfile,
              files = paste0(GHSfilename, "_", tile, ".tif"),
              exdir = output_directory
            )
            to_remove <- append(to_remove, zipfile)

            i <- i + 1
            grids[[i]] <- terra::rast(tiffile)
            to_remove <- append(to_remove, tiffile)
          } else {
            warning(paste(tile, "is not a valid GHSL tile and is ignored. \n"))
          }
        }

        # check the length of the grids, if > 1, merge the grids
        if (length(grids) == 0) {
          stop("No valid GHSL tiles provided")
        } else if (length(grids) == 1) {
          merged <- grids[[1]]
        } else {
          merged <- do.call(terra::merge, grids)
        }
        names(merged) <- c(type)
        terra::writeRaster(merged, metadata$file)
        file.remove(to_remove)
      }

      # write the metadata
      write(
        jsonlite::toJSON(metadata),
        file.path(output_directory, gsub(".tif", ".json", filenames[[nr]]))
      )
    }
  }
  return(file.path(output_directory, filenames))
}
