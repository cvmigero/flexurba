#' Plot the spatial units classification
#'
#' @description
#' The function can be used to plot the results of the spatial units classification of the Degree of Urbanisation. The implementation relies upon the function `ggplot2::geom_sf()`. By default, the standard color scheme of the Global Human Settlement Layer (GHSL) is used (see [GHSL Data Package 2023](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf)), but this can be altered by the `palette` argument.
#'
#' Note that the function is computational quite heavy for large spatial extents (regional or global scale). It is advised to use the `extent` argument to plot only a selection of the spatial units classification.
#' @param units object of class `sf`. The spatial units to be displayed on the plot
#' @param classification dataframe with the classification of the spatial units, as returned by `classify_units()`. If `NULL`, it is assumed that the classification results are merged in the `units` object.
#' @param level1 logical. Whether the spatial units are classified according to level 1 of the Degree of Urbanisation (`TRUE`), or level 2 of the Degree of Urbanisation (`FALSE`).
#' @param extent SpatExtent or an object of class "bbox" (`sf`). If not `NULL`, the spatial units will be filtered based on the provided extent before plotting.
#' @param column character. Column name of the spatial units classification. By default, `"flexurba_L1"` when `level1=TRUE` and `"flexurba_L2"` when `level1=FALSE`.
#' @param palette named vector with the color palette used to plot the spatial units classification. If `NULL`, the standard color palette of the GHSL is used (see `GHSL_palette()`).
#' @param labels vector with the labels used in the legend. If `NULL`, the standard labels of the GHSL are used (see `GHSL_labels()`).
#' @param title character. Title of the plot.
#' @param scalebar logical. Whether to add a scale bar to the plot.
#' @param filename character. Path to the location to save the plot
#' @return ggplot object
#' @examples
#' # get spatial units classification
#' data_belgium <- load_grid_data_belgium()
#' grid_classification <- classify_grid(data_belgium)
#' data1 <- preprocess_units(
#'   units = flexurba::units_belgium,
#'   classification = grid_classification,
#'   pop = data_belgium$pop
#' )
#' units_classification <- classify_units(data1)
#'
#' # plot using the standard color palette
#' plot_units(data1$units, units_classification)
#'
#' # plot using custom palette and labels
#' plot_units(data1$units, units_classification,
#'   palette = c("3" = "#e16c72", "2" = "#fac66c", "1" = "#97c197"),
#'   labels = c("C", "T", "R")
#' )
#' @export
plot_units <- function(units, classification = NULL, level1 = TRUE, extent = NULL, column = NULL, palette = NULL, labels = NULL, title = NULL, scalebar = FALSE, filename = NULL) {
  
  # global variable
  .data <- NULL
  
  # use standard palette if no palette is provided
  if (is.null(palette)) {
    palette <- utils::head(GHSL_palette(level1), -1)
  }
  
  # use standard labels if no labels are provided
  if (is.null(labels)) {
    labels <- GHSL_labels(level1, grids = FALSE)
  }
  
  # check if palette and labels are of the same length
  if (length(palette) != length(labels)) {
    stop("Invalid argument: palette and labels must have the same length")
  }
  
  # get the column name of the units classification
  if (is.null(column)) {
    if (level1) {
      column <- "flexurba_L1"
    } else {
      column <- "flexurba_L2"
    }
  }
  
  # crop units to extent
  if (!is.null(extent)) {
    if (inherits(extent, "SpatExtent")) {
      extent <- convert_spatextent_to_layer(extent)
    } else if (inherits(extent, "bbox")) {
      extent <- sf::st_as_sfc(extent)
    } else {
      stop("Invalid argument: extent should be SpatExtent or sf bbox object")
    }
    sf::st_geometry(units) <- "geometry"
    units <- units %>%
      dplyr::filter(as.vector(sf::st_intersects(.data[["geometry"]], extent, sparse = FALSE)))
  }
  
  # join units and classification
  if (!is.null(classification)) {
    if (length(intersect(names(classification), names(units))) == 0) {
      stop("Invalid argument: there is no common column in 'classification' and 'units'")
    }
    suppressMessages(units <- units %>% dplyr::left_join(classification))
  }
  
  # check if the column name is present in the data.frame
  if (!(column %in% names(units))) {
    stop(paste("Invalid argument:", column, "is not a column in the provided data"))
  }
  
  # check if the palette and the values match
  units[[column]] <- as.factor(units[[column]])
  if (length(setdiff(unique(units[[column]]), names(palette))) != 0) {
    warning("Some values in the spatial units classification are not included in the pallette (displayed in white) \n")
  }
  
  # return plot
  plotobj <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = units, ggplot2::aes(fill = .data[[column]])) +
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