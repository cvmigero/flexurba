#' Plot the grid cell classification 
#'
#' @description
#' The function can be used to plot the results of the grid cell classification of the Degree of Urbanisation. The implementation relies upon the function `tidyterra::geom_spatraster()`. By default, the standard color scheme of the Global Human Settlement Layer (GHSL) is used (see [GHSL Data Package 2023](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf)), but this can be altered by the `palette` argument.
#'
#' Note that the function is computational quite heavy for large spatial extents (regional or global scale). It is advised to use the `extent` argument to plot only a selection of the grid classification.
#' @param classification SpatRaster / character. A SpatRaster with the grid cell classification or the path to the grid cell classification file
#' @param extent SpatExtent. If not `NULL`, the grid classification will be cropped to the provided extent before plotting
#' @param level1 logical. Whether the grid is classified according to level 1 of the Degree of Urbanisation (`TRUE`), or level 2 of the Degree of Urbanisation (`FALSE`).
#' @param palette named vector with the color palette used to plot the grid cell classification. If `NULL`, the standard color palette of the GHSL is used (see `GHSL_palette()`).
#' @param labels vector with the labels used in the legend. If `NULL`, the standard labels of the GHSL are used (see `GHSL_labels()`).
#' @param title character. Title of the plot.t
#' @param scalebar logical. Whether to add a scale bar to the plot.
#' @param filename character. Path to the location to save the plot
#' @return ggplot object
#' @examples
#' classification <-DoU_classify_grid(DoU_load_grid_data_belgium())
#'
#' # plot with standard color scheme
#' DoU_plot_grid(classification)
#'
#' # use custom palette and labels
#' DoU_plot_grid(classification,
#'   palette = c("3" = "#e16c72", "2" = "#fac66c", "1" = "#97c197", "0" = "#acd3df"),
#'   labels = c("UC", "UCL", "RUR", "WAT")
#' )
#' @export
DoU_plot_grid <- function(classification, extent = NULL, level1 = TRUE, palette = NULL, labels = NULL, title = NULL, scalebar = FALSE, filename = NULL) {
  # check if classification is SpatRaster, otherwise read the classification from file
  if (!inherits(classification, "SpatRaster")) {
    classification <- terra::rast(classification)
  }
  
  # if extent is provided, clip on the extent
  if (!is.null(extent)) {
    classification <- terra::crop(classification, extent)
  }
  
  # save the name of the layer
  layer <- names(classification)[1]
  
  # convert to a categorical raster
  plot_rast <- classification %>% terra::init(NA)
  terra::values(plot_rast) <- as.factor(terra::values(classification))
  
  # use standard palette if no palette is provided
  if (is.null(palette)) {
    palette <- GHSL_palette(level1)
  }
  
  # use standard labels if no labels are provided
  if (is.null(labels)) {
    labels <- GHSL_labels(level1)
  }
  
  # check if palette and labels are of the same length
  if (length(palette) != length(labels)) {
    stop("Invalid argument: palette and labels must have the same length")
  }
  
  # check if the palette and the values match
  if (length(setdiff(unlist(terra::unique(classification)), names(palette))) != 0) {
    warning("Some values in the grid classification are not included in the pallette (displayed in white) \n")
  }
  
  # global variable
  .data <- NULL
  
  # return plot
  plotobj <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = plot_rast, ggplot2::aes(fill = .data[[layer]])) +
    ggplot2::scale_fill_manual(
      breaks = as.numeric(names(palette)),
      values = palette,
      labels = labels,
      na.value = "white"
    ) +
    ggplot2::labs(fill = NULL) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom", plot.margin = grid::unit(c(0, 0, 0, 0), "mm"), plot.title = ggplot2::element_text(hjust = 0.5))
  
  if (scalebar) {
    plotobj <- plotobj +
      ggspatial::annotation_scale(pad_x = grid::unit(0.06, "npc"), pad_y = grid::unit(0.08, "npc"), width_hint = 0.10, height = grid::unit(0.08, "cm"), bar_cols = c("black"))
  }
  
  if (!is.null(filename)) {
    ggplot2::ggsave(filename, plotobj)
  }
  return(plotobj)
}

#' Plot the grid cell classification 
#' 
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' `plot_grid()`  has been renamed to `DoU_plot_grid()` to create a more consistent API and to better indicate that this function is specifically designed for plotting the DEGURBA classification generated with `DoU_classify_grid()`. 
#' @param classification SpatRaster / character. A SpatRaster with the grid cell classification or the path to the grid cell classification file
#' @param extent SpatExtent. If not `NULL`, the grid classification will be cropped to the provided extent before plotting
#' @param level1 logical. Whether the grid is classified according to level 1 of the Degree of Urbanisation (`TRUE`), or level 2 of the Degree of Urbanisation (`FALSE`).
#' @param palette named vector with the color palette used to plot the grid cell classification. If `NULL`, the standard color palette of the GHSL is used (see `GHSL_palette()`).
#' @param labels vector with the labels used in the legend. If `NULL`, the standard labels of the GHSL are used (see `GHSL_labels()`).
#' @param title character. Title of the plot.t
#' @param scalebar logical. Whether to add a scale bar to the plot.
#' @param filename character. Path to the location to save the plot
#' @return ggplot object
#' @keywords internal
#' @export
plot_grid <- function(classification, extent = NULL, level1 = TRUE, palette = NULL, labels = NULL, title = NULL, scalebar = FALSE, filename = NULL) {
  lifecycle::deprecate_soft("1.0.0.0", "plot_grid()", "DoU_plot_grid()")
  DoU_plot_grid(classification, extent, level1, palette, labels, title, scalebar, filename)
}
