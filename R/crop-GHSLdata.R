#' Crop GHSL data to the provided extent
#'
#' @description
#' The grid cell classification of the Degree of Urbanisation requires three different inputs: a built-up area grid, a population grid, and a land grid. The grids can be downloaded from the GHSL website with [download_GHSLdata()].
#'
#' This function reads the downloaded grids from the `global_directory` and crops them to the provided extent. The newly cropped grids will be saved in the `output_directory`, together with their respective metadata (in JSON format).
#' @param extent SpatRaster, or any other object that has a SpatExtent
#' @param output_directory character. Path to the directory to save the cropped grids
#' @param global_directory character. Path to the directory where the global grids are saved (created with [download_GHSLdata()])
#' @param output_filenames vector of length 3 with the filenames used to save the built-up area, population and land grid in `output_directory`
#' @param global_filenames vector of length 3 with the filenames of the built-up area, population and land grid in `global_directory`
#' @param buffer integer. If larger than 0, a buffer of `buffer` cells will be added around the borders of the extent to allow cities or towns at the edges to be correctly classified.
#' @return path to the created files.
#' @export
crop_GHSLdata <- function(
  extent,
  output_directory,
  global_directory,
  output_filenames,
  global_filenames,
  buffer = 5
) {
  
  if (missing(output_filenames) | missing(global_filenames)){
    stop("Invalid argument: 'output_filenames' and 'global_filenames' are now required (as per CRAN policies)")
  }
  
  # check if input and output names are valid
  if (!(length(output_filenames) == length(global_filenames))) {
    stop(
      "output_filenames and global_filenames should have length three: respectively the name of the built-up, population and land grid"
    )
  }
  if (!all(endsWith(c(output_filenames, global_filenames), ".tif"))) {
    stop("output_filenames and global_filenames should have extension .tif")
  }
  
  # check if other arguments are valid
  if (!all(file.exists(file.path(global_directory, global_filenames)))) {
    stop("global files do not exist")
  }
  if (!inherits(extent, 'SpatExtent')) {
    stop("extent is not a valid SpatExtent")
  }
  if (!is.numeric(buffer) & buffer >= 0) {
    stop("buffer should be numeric and >= 0")
  }

  if (!dir.exists(output_directory)) {
    dir.create(output_directory)
  }

  # add buffer to the extent
  if (buffer > 0) {
    # get the resolution of the input grid
    resolution <- terra::res(terra::rast(file.path(
      global_directory,
      global_filenames[[1]]
    )))[[1]]
    buffer <- buffer * resolution
    extent <- extent + buffer
  }

  for (i in seq_along(output_filenames)) {
    targetfile <- file.path(output_directory, output_filenames[[i]])
    if (file.exists(targetfile)) {
      stop(paste(
        targetfile,
        "already exists. Choose another directory or delete the file."
      ))
    }

    # crop the tif
    tiffile <- terra::rast(file.path(global_directory, global_filenames[[i]]))
    terra::crop(tiffile, extent, filename = targetfile)

    # write metadata
    if (
      file.exists(file.path(
        global_directory,
        gsub(".tif", ".json", global_filenames[[i]])
      ))
    ) {
      metadata <- jsonlite::fromJSON(file.path(
        global_directory,
        gsub(".tif", ".json", global_filenames[[i]])
      ))
    } else {
      metadata <- list()
    }

    metadata$file <- file.path(
      output_directory,
      gsub(".tif", ".json", output_filenames[[i]])
    )
    metadata$cropped_to_bbox <- terra::ext(extent) %>% as.vector()
    metadata$buffer <- buffer
    write(
      jsonlite::toJSON(metadata),
      file.path(output_directory, gsub(".tif", ".json", output_filenames[[i]]))
    )
  }

  # check if the area of interest is located on the edge of the Mollweide projection, if so, give
  # a warning for potential distortions
  valid_mollweide_cropped <- terra::vect(valid_mollweide) %>%
    terra::crop(extent)

  if (!all(terra::relate(valid_mollweide_cropped, 
                         terra::as.polygons(extent), 'covers'))) {
    warning(
      "The area of interest is located on the edges of the Mollweide projection. Be aware that this can cause distortions in the grid cell classification and subsequent visualisations and zonal statistics.\n"
    )
  }
  return(file.path(output_directory, output_filenames))
}
