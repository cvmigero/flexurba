#' Create the DEGURBA grid cell classification of urban centres
#'
#' @description
#' The Degree of Urbanisation identifies urban centres as clusters of continuous grid cells (based on rook contiguity) with a minimum density of 1500 inhabitants per km² (or with a minimum built-up area; see details), and a minimum total population of 50 000 inhabitants. Gaps smaller than 15 km² in the urban centres are filled and edges are smoothed by a 3x3-majority rule (see details).
#'
#' For more information about the Degree of Urbanisation methodology, see the [methodological manual](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Applying_the_degree_of_urbanisation_manual), [GHSL Data Package 2022](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2022.pdf) and [GHSL Data Package 2023](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf).
#'
#' The arguments of the function allow to adapt the standard specifications in the Degree of Urbanisation in order to construct an alternative version of the grid classification.
#' @param data path to the directory with the data, or named list with the data as returned by function [DoU_preprocess_grid()]
#' @param density_threshold numeric. Minimum population density per permanent land of a cell required to belong to an urban centre
#' @param size_threshold numeric. Minimum total population size required for an urban centre
#' @param contiguity_rule integer. Which cells are considered adjacent: `4` for rooks case (horizontal and vertical neighbours) or `8` for queens case (horizontal, vertical and diagonal neighbours)
#' @param built_criterium logical. Whether to use the additional built-up area criterium (see details). If `TRUE`, not only cells that meet the population density requirement will be considered when delineating urban centres, but also cells with a built-up area per permanent land above the `built_threshold`
#' @param built_threshold numeric. Additional built-up area threshold. A value between `0` and `1`, representing the minimum built-up area per permanent land. Ignored when `built_criterium` is `FALSE`.
#' @param smooth_pop logical. Whether to smooth the population grid before delineating urban centres. If `TRUE`, the population grid will be smoothed with a moving average of window size `smooth_pop_window`.
#' @param smooth_pop_window integer. Size of the moving window used to smooth the population grid before delineating urban centres. Ignored when `smooth_pop` is `FALSE`.
#' @param gap_fill logical. Whether to perform gap filling. If `TRUE`, gaps in urban centres smaller than `max_gap` are filled.
#' @param max_gap integer. Gaps with an area smaller than this threshold in urban centres will be filled (unit is km²). Ignored when `gap_fill` is `FALSE`.
#' @param smooth_edge logical. Whether to perform edge smoothing. If `TRUE`, edges of urban centres are smoothed with the function `smooth_edge_fun`.
#' @param smooth_edge_fun character / function. Function used to smooth the edges of urban centres. Ignored when `smooth_edge` is `FALSE`. Possible values are:
#'   - `"majority_rule_R2022A"` to use the edge smoothing algorithm in GHSL Data Package 2022 (see details)
#'   - `"majority_rule_R2023A"` to use the edge smoothing algorithm in GHSL Data Package 2023 (see details)
#'   - a custom function with a signature similar as [`apply_majority_rule()`].
#' @param value integer. Value assigned to urban centres in the resulting grid
#' @return SpatRaster with the grid cell classification of urban centres
#' @details
#' - **Optional built-up area criterium**
#'
#' In Data Package 2022, the Degree of Urbanisation includes an optional built-up area criterium to account for the presence of office parks, shopping malls, factories and transport infrastructure. When the setting is enabled, urban centres are created using both cells with a population density of at least 1500 inhabitants per km² *and* cells that have at least 50% built-up area on permanent land. For more information: see [GHSL Data Package 2022, footnote 25](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2022.pdf). The parameter setting `built_criterium=TRUE` and `built_threshold=0.5` reproduces this built-up area criterium.
#'
#' In Data Package 2023, the built-up area criterium is slightly adapted and renamed to the "Reduce Fragmentation Option". Instead of using a fixed threshold of built-up area per permanent land of 50%, an "optimal" threshold is employed. The optimal threshold is dynamically identified as the global average built-up area proportion in clusters with a density of at least 1500 inhabitants per permanent land with a minimum population of 5000 people. For more information: see [GHSL Data Package 2023, footnote 30](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf). The optimal built-up threshold can be computed with the function [DoU_get_optimal_builtup()]. We determined empirically that this optimal threshold is 20% for the data of 2020.
#'
#' - **Edge smoothing: majority rule algorithm**
#'
#' In Data Package 2022, edges of urban centres are smoothed by an iterative majority rule. The majority rule works as follows: if a cell has at least five of the eight surrounding cells belonging to an unique urban centre, then the cell is added to that urban centre. The process is iteratively repeated until no more cells are added. The parameter setting `smooth_edge=TRUE` and `smooth_edge_fun="majority_rule_R2022A"` reproduces this edge smoothing rule.
#'
#' In Data Package 2023, the majority rule is slightly adapted. A cell is added to an urban centre if the majority of the surrounding cells belongs to an unique urban centre, with majority only computed among populated or land cells (proportion of permanent land > 0.5). In addition, cells with permanent water are never added to urban centres. The process is iteratively repeated until no more cells are added. For more information: see [GHSL Data Package 2023, footnote 29](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf). The parameter setting `smooth_edge=TRUE` and `smooth_edge_fun="majority_rule_R2023A"` reproduces this edge smoothing rule.
#'
#' @examples
#' data_belgium <- DoU_load_grid_data_belgium()
#'
#' # standard parameters of the Degree of Urbanisation:
#' classification1 <- DoU_classify_grid_urban_centres(data_belgium)
#' DoU_plot_grid(classification1)
#'
#' # with custom parameters:
#' classification2 <- DoU_classify_grid_urban_centres(data_belgium,
#'   density_threshold = 1000,
#'   gap_fill = FALSE,
#'   smooth_edge = FALSE
#' )
#' DoU_plot_grid(classification2)
#'
#' @export
DoU_classify_grid_urban_centres <- function(
  data,
  density_threshold = 1500,
  size_threshold = 50000,
  contiguity_rule = 4,
  built_criterium = TRUE,
  built_threshold = 0.2,
  smooth_pop = FALSE,
  smooth_pop_window = 5,
  gap_fill = TRUE,
  max_gap = 15,
  smooth_edge = TRUE,
  smooth_edge_fun = "majority_rule_R2023A",
  value = 3
) {
  ##### CHECK IF PAREMETERS ARE VALID

  # read data
  if (is.character(data)) {
    data <- DoU_preprocess_grid(data)
  }

  # check if data is valid
  if (
    !all(
      c("pop", "land", "pop_per_land", "built_per_land", "built") %in%
        names(data)
    )
  ) {
    stop(
      "Invalid argument: The data should contain a named list of pop, land, built, pop_per_land and built_per_land as generated by the function DoU_preprocess_grid."
    )
  }

  # check the other arguments:
  if (!built_criterium %in% c(TRUE, FALSE)) {
    stop(paste(
      "Invalid argument:",
      built_criterium,
      "is not a valid parameter for built_criterium"
    ))
  }

  if (is.na(as.numeric(built_threshold))) {
    stop(paste(
      "Invalid argument:",
      built_threshold,
      "is not a valid parameter for built_threshold"
    ))
  } else {
    built_threshold <- as.numeric(built_threshold)
  }

  if (!smooth_pop %in% c(TRUE, FALSE)) {
    stop(paste(
      "Invalid argument:",
      smooth_pop,
      "is not a valid parameter for smooth_pop"
    ))
  }

  if (!is.numeric(smooth_pop_window)) {
    stop(paste(
      "Invalid argument:",
      smooth_pop_window,
      "is not a valid parameter for smooth_pop_window"
    ))
  }

  if (!(gap_fill %in% c(TRUE, FALSE))) {
    stop(paste(
      "Invalid argument:",
      gap_fill,
      "is not a valid parameter for gap_fill"
    ))
  }

  if (!smooth_edge %in% c(TRUE, FALSE)) {
    stop(paste(
      "Invalid argument:",
      smooth_edge,
      "is not a valid parameter for smooth_edge"
    ))
  }

  if (
    !((is.function(smooth_edge_fun)) ||
      (smooth_edge_fun %in% c("majority_rule_R2022A", "majority_rule_R2023A")))
  ) {
    stop(paste(
      "Invalid argument:",
      smooth_edge_fun,
      "is not a valid parameter for smooth_edge_fun"
    ))
  }

  ##### CONSTRUCT URBAN CENTRES

  # smooth the population grid if smooth_pop = TRUE
  if (smooth_pop) {
    pop <- terra::focal(
      data$pop,
      w = smooth_pop_window,
      fun = "mean",
      na.rm = TRUE
    )
    pop_per_land <- terra::focal(
      data$pop_per_land,
      w = smooth_pop_window,
      fun = "mean",
      na.rm = TRUE
    )
  } else {
    pop <- data$pop
    pop_per_land <- data$pop_per_land
  }

  # get dense clusters (with or without built-up criterium)
  if (built_criterium) {
    urbancentres <- flexurba::get_clusters(
      xden = pop_per_land,
      minden = density_threshold,
      xden2 = data$built_per_land,
      minden2 = built_threshold,
      xsiz = pop,
      minsiz = size_threshold,
      directions = contiguity_rule
    )
  } else {
    urbancentres <- flexurba::get_clusters(
      xden = pop_per_land,
      minden = density_threshold,
      xsiz = pop,
      minsiz = size_threshold,
      directions = contiguity_rule
    )
  }

  # smooth edges if smooth_edge = TRUE
  if (smooth_edge) {
    if (smooth_edge_fun == "majority_rule_R2022A") {
      urbancentres <- apply_majority_rule(urbancentres, version = "R2022A")
    } else if (smooth_edge_fun == "majority_rule_R2023A") {
      urbancentres <- apply_majority_rule(
        urbancentres,
        version = "R2023A",
        permanent_water = flexurba::DoU_classify_grid_water(data),
        land = data$land,
        pop = pop
      )
    } else {
      urbancentres <- smooth_edge_fun(urbancentres)
    }
  }

  # fill gaps if gap_fill = TRUE
  if (gap_fill) {
    urbancentres <- flexurba::fill_gaps(urbancentres, max_gap)
  }

  # set the value of urban centres and return result
  terra::set.values(urbancentres, which(!is.na(urbancentres[])), value)
  names(urbancentres) <- c("layer")

  return(urbancentres)
}

#' Create the DEGURBA grid cell classification of urban centres
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `classify_grid_urban_centres()` has been renamed to `DoU_classify_grid_urban_centres()` to create a more consistent API and to better indicate that this function is specifically designed to classify urban centres in the context of the DEGURBA classification.
#' @param data path to the directory with the data, or named list with the data as returned by function [DoU_preprocess_grid()]
#' @param density_threshold numeric. Minimum population density per permanent land of a cell required to belong to an urban centre
#' @param size_threshold numeric. Minimum total population size required for an urban centre
#' @param contiguity_rule integer. Which cells are considered adjacent: `4` for rooks case (horizontal and vertical neighbours) or `8` for queens case (horizontal, vertical and diagonal neighbours)
#' @param built_criterium logical. Whether to use the additional built-up area criterium (see details). If `TRUE`, not only cells that meet the population density requirement will be considered when delineating urban centres, but also cells with a built-up area per permanent land above the `built_threshold`
#' @param built_threshold numeric. Additional built-up area threshold. A value between `0` and `1`, representing the minimum built-up area per permanent land. Ignored when `built_criterium` is `FALSE`.
#' @param smooth_pop logical. Whether to smooth the population grid before delineating urban centres. If `TRUE`, the population grid will be smoothed with a moving average of window size `smooth_pop_window`.
#' @param smooth_pop_window integer. Size of the moving window used to smooth the population grid before delineating urban centres. Ignored when `smooth_pop` is `FALSE`.
#' @param gap_fill logical. Whether to perform gap filling. If `TRUE`, gaps in urban centres smaller than `max_gap` are filled.
#' @param max_gap integer. Gaps with an area smaller than this threshold in urban centres will be filled (unit is km²). Ignored when `gap_fill` is `FALSE`.
#' @param smooth_edge logical. Whether to perform edge smoothing. If `TRUE`, edges of urban centres are smoothed with the function `smooth_edge_fun`.
#' @param smooth_edge_fun character / function. Function used to smooth the edges of urban centres. Ignored when `smooth_edge` is `FALSE`. Possible values are:
#'   - `"majority_rule_R2022A"` to use the edge smoothing algorithm in GHSL Data Package 2022 (see details)
#'   - `"majority_rule_R2023A"` to use the edge smoothing algorithm in GHSL Data Package 2023 (see details)
#'   - a custom function with a signature similar as [`apply_majority_rule()`].
#' @param value integer. Value assigned to urban centres in the resulting grid
#' @return SpatRaster with the grid cell classification of urban centres
#' @keywords internal
#' @export
classify_grid_urban_centres <- function(
  data,
  density_threshold = 1500,
  size_threshold = 50000,
  contiguity_rule = 4,
  built_criterium = TRUE,
  built_threshold = 0.2,
  smooth_pop = FALSE,
  smooth_pop_window = 5,
  gap_fill = TRUE,
  max_gap = 15,
  smooth_edge = TRUE,
  smooth_edge_fun = "majority_rule_R2023A",
  value = 3
) {
  return(DoU_classify_grid_urban_centres(
    data,
    density_threshold,
    size_threshold,
    contiguity_rule,
    built_criterium,
    built_threshold,
    smooth_pop,
    smooth_pop_window,
    gap_fill,
    max_gap,
    smooth_edge,
    smooth_edge_fun,
    value
  ))
}
