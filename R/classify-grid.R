#' Create the grid cell classification
#'
#' @description
#' The function reconstructs the grid cell classification of the Degree of Urbanisation. The arguments of the function allow to adapt the standard specifications in the Degree of Urbanisation in order to construct an alternative version  (see section "Custom specifications" below).
#'
#' For more information about the Degree of Urbanisation methodology, see the [methodological manual](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Applying_the_degree_of_urbanisation_manual), [GHSL Data Package 2022](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2022.pdf) and [GHSL Data Package 2023](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf).
#'
#' @param data path to the directory with the data, or named list with the data as returned by function [DoU_preprocess_grid()]
#' @param level1 logical. Whether to classify the grid according to first hierarchical level (`TRUE`) or the second hierarchical level (`FALSE`). For more details, see section "Classification rules" below.
#' @param parameters named list with the parameters to adapt the standard specifications in the Degree of Urbanisation classification. For more details, see section "Custom specifications" below.
#' @param values vector with the values assigned to the different classes in the resulting classification:
#'    - If `level1=TRUE`: the vector should contain the values for (1) urban centres, (2) urban clusters, (3) rural grid cells and (4) water cells.
#'    - If `level1=FALSE`: the vector should contain the values for (1) urban centres, (2) dense urban clusters, (3) semi-dense urban clusters, (4) suburban or peri-urban cells, (5) rural clusters, (6) low density rural cells, (7) very low density rural cells and (8) water cells.
#' @param regions logical. Whether to execute the classification in the memory-efficient pre-defined regions. For more details, see section "Regions" below (Note that this requires a large amount of memory).
#' @param filename character. Output filename (with extension `.tif`). The grid classification together with a metadata file (in JSON format) will be saved if `filename` is not `NULL`.
#' @return SpatRaster with the grid cell classification
#'
#'
#' @section Classification rules:
#'
#' The Degree of Urbanisation consists of two hierarchical levels. In level 1, the cells of a 1 km² grid are classified in urban centres, urban clusters and rural cells (and water cells). In level 2, urban cluster are further divided in dense urban clusters, semi-dense urban clusters and suburbs or peri-urban cells. Rural cells are further divided in rural clusters, low density rural cells and very low density rural cells.
#'
#' The detailed classification rules are as follows:
#'
#' **LEVEL 1:**
#'
#' - **Urban centres** are identified as clusters of continuous grid cells (based on rook contiguity) with a minimum density of 1500 inhabitants per km² (or with a minimum built-up area; see section "Built-up area criterium" below), and a minimum total population of 50 000 inhabitants. Gaps smaller than 15 km² in the urban centres are filled and edges are smoothed by a 3x3-majority rule (see section "Edge smoothing" below).
#' - **Urban clusters** are identified as clusters of continuous grid cells (based on queen contiguity) with a minimum density of 300 inhabitants per km², and a minimum total population of 5000 inhabitants.
#' - **Water cells** contain no built-up area, no population, and less than 50% permanent land. All other cells not belonging to an urban centre or urban cluster are considered **rural cells**.
#'
#'
#' **LEVEL 2:**
#'
#' - **Urban centres** are identified as clusters of continuous grid cells (based on rook contiguity) with a minimum density of 1500 inhabitants per km² (or with a minimum built-up area; see section "Built-up area criterium" below), and a minimum total population of 50 000 inhabitants. Gaps smaller than 15 km² in the urban centres are filled and edges are smoothed by a 3x3-majority rule (see section "Edge smoothing" below).
#' - **Dense urban clusters** are identified as clusters of continuous grid cells (based on rook contiguity) with a minimum density of 1500 inhabitants per km² (or with a minimum built-up area; see section "Built-up area criterium" below), and a minimum total population of 5000 inhabitants.
#' - **Semi-dense urban clusters** are identified as clusters of continuous grid cells (based on queen contiguity) with a minimum density of 300 inhabitants per km², and a minimum total population of 5000 inhabitants, that are at least 3 km away from urban centres and dense urban clusters. Clusters that are less than 3 km away are classified as **suburban and peri-urban cells**.
#' - **Rural clusters** are clusters of continuous grid cells (based on queen contiguity) with a minimum density of 300 inhabitants per km², and a minimum total population of 500 inhabitants.
#' - **Low density rural cells** are remaining cells with a population density less than 50 inhabitants per km².
#' - **Water cells** contain no built-up area, no population, and less than 50% permanent land. All cells not belonging to an other class are considered **very low density rural cells**.
#'
#' For more information about the Degree of Urbanisation methodology, see the [methodological manual](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Applying_the_degree_of_urbanisation_manual), [GHSL Data Package 2022](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2022.pdf) and [GHSL Data Package 2023](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf).
#'
#' @section Custom specifications:
#'
#' The function allows to change the standard specifications of the Degree of Urbanisation in order to construct an alternative version of the grid classification. Custom specifications can be passed in a named list by the argument `parameters`. The supported parameters with their default values are returned by the function [`DoU_get_grid_parameters()`] and are as follows:
#'
#' **LEVEL 1**
#'
#' - `UC_density_threshold` numeric (default: `1500`).
#'
#'      Minimum population density per permanent land of a cell required to belong to an urban centre
#' - `UC_size_threshold` numeric (default: `50000`).
#'
#'      Minimum total population size required for an urban centre
#' - `UC_contiguity_rule` integer (default: `4`).
#'
#'      Which cells are considered adjacent in urban centres: `4` for rooks case (horizontal and vertical neighbors) or `8` for queens case (horizontal, vertical and diagonal neighbors)
#' - `UC_built_criterium` logical (default: `TRUE`).
#'
#'      Whether to use the additional built-up area criterium (see section "Built-up area criterium" below). If `TRUE`, not only cells that meet the population density requirement will be considered when delineating urban centres, but also cells with a built-up area per permanent land above the `UC_built_threshold`
#' - `UC_built_threshold` numeric or character (default: `0.2`).
#'
#'      Additional built-up area threshold. Can be a value between `0` and `1`, representing the minimum built-up area per permanent land, or `"optimal"` (see section "Built-up area criterium" below). Ignored when `UC_built_criterium` is `FALSE`.
#' - `built_optimal_data` character / list (default: `NULL`).
#'
#'     Path to the directory with the data, or named list with the data as returned by function [DoU_preprocess_grid()] used to determine the optimal built threshold (see section "Built-up area criterium" below). Ignored when `UC_built_criterium` is `FALSE` or when `UC_built_threshold` is not `"optimal"`.
#' - `UC_smooth_pop` logical (default: `FALSE`).
#'
#'      Whether to smooth the population grid before delineating urban centres. If `TRUE`, the population grid will be smoothed with a moving average of window size `UC_smooth_pop_window`.
#' - `UC_smooth_pop_window` integer (default: `5`).
#'
#'      Size of the moving window used to smooth the population grid before delineating urban centres. Ignored when `UC_smooth_pop` is `FALSE`.
#' - `UC_gap_fill` logical (default: `TRUE`).
#'
#'      Whether to perform gap filling. If `TRUE`, gaps in urban centres smaller than `UC_max_gap` are filled.
#' - `UC_max_gap` integer (default: `15`).
#'
#'      Gaps with an area smaller than this threshold in urban centres will be filled (unit is km²). Ignored when `UC_gap_fill` is `FALSE`.
#' - `UC_smooth_edge` logical (default: `TRUE`).
#'
#'      Whether to perform edge smoothing. If `TRUE`, edges of urban centres are smoothed with the function `UC_smooth_edge_fun`.
#' - `UC_smooth_edge_fun` character / function (default: `"majority_rule_R2023A"`).
#'
#'      Function used to smooth the edges of urban centres. Ignored when `UC_smooth_edge` is `FALSE`. Possible values are:
#'      - `"majority_rule_R2022A"` to use the edge smoothing algorithm in GHSL Data Package 2022 (see section "Edge smoothing" below)
#'      - `"majority_rule_R2023A"` to use the edge smoothing algorithm in GHSL Data Package 2023 (see section "Edge smoothing" below)
#'      - a custom function with a signature similar as [apply_majority_rule()].
#' - `UCL_density_threshold` numeric (default: `300`).
#'
#'      Minimum population density per permanent land of a cell required to belong to an urban cluster
#' - `UCL_size_threshold` numeric (default: `5000`).
#'
#'      Minimum total population size required for an urban cluster
#' - `UCL_contiguity_rule` integer (default: `8`).
#'
#'      Which cells are considered adjacent in urban clusters: `4` for rooks case (horizontal and vertical neighbors) or `8` for queens case (horizontal, vertical and diagonal neighbors)
#' - `UCL_smooth_pop` logical (default: `FALSE`).
#'
#'      Whether to smooth the population grid before delineating urban clusters. If `TRUE`, the population grid will be smoothed with a moving average of window size `UCL_smooth_pop_window`.
#' - `UCL_smooth_pop_window` integer (default: `5`).
#'
#'      Size of the moving window used to smooth the population grid before delineating urban clusters. Ignored when `UCL_smooth_pop` is `FALSE`.
#' - `water_land_threshold` numeric (default: `0.5`).
#'
#'      Maximum proportion of permanent land allowed in a water cell
#' - `water_pop_threshold` numeric (default: `0`).
#'
#'      Maximum population size allowed in a water cell
#' - `water_built_threshold` numeric (default: `0`).
#'
#'      Maximum built-up area allowed in a water cell
#'
#' **LEVEL 2**
#' - `UC_density_threshold` numeric (default: `1500`).
#'
#'      Minimum population density per permanent land of a cell required to belong to an urban centre
#' - `UC_size_threshold` numeric (default: `50000`).
#'
#'      Minimum total population size required for an urban centre
#' - `UC_contiguity_rule` integer (default: `4`).
#'
#'      Which cells are considered adjacent in urban centres: `4` for rooks case (horizontal and vertical neighbors) or `8` for queens case (horizontal, vertical and diagonal neighbors)
#' - `UC_built_criterium` logical (default: `TRUE`).
#'
#'      Whether to use the additional built-up area criterium (see section "Built-up area criterium" below). If `TRUE`, not only cells that meet the population density requirement will be considered when delineating urban centres, but also cells with a built-up area per permanent land above the `UC_built_threshold`
#' - `UC_built_threshold` numeric or character (default: `0.2`).
#'
#'      Additional built-up area threshold. Can be a value between `0` and `1`, representing the minimum built-up area per permanent land, or `"optimal"` (see section "Built-up area criterium" below). Ignored when `UC_built_criterium` is `FALSE`.
#' - `built_optimal_data` character / list (default: `NULL`).
#'
#'      Path to the directory with the data, or named list with the data as returned by function [DoU_preprocess_grid()] used to determine the optimal built threshold (see section "Built-up area criterium" below). Ignored when `UC_built_criterium` is `FALSE` or when `UC_built_threshold` is not `"optimal"`.
#' - `UC_smooth_pop` logical (default: `FALSE`).
#'
#'      Whether to smooth the population grid before delineating urban centres. If `TRUE`, the population grid will be smoothed with a moving average of window size `UC_smooth_pop_window`.
#' - `UC_smooth_pop_window` integer (default: `5`).
#'
#'      Size of the moving window used to smooth the population grid before delineating urban centres. Ignored when `UC_smooth_pop` is `FALSE`.
#' - `UC_gap_fill` logical (default: `TRUE`).
#'
#'      Whether to perform gap filling. If `TRUE`, gaps in urban centres smaller than `UC_max_gap` are filled.
#' - `UC_max_gap` integer (default: `15`).
#'
#'      Gaps with an area smaller than this threshold in urban centres will be filled (unit is km²). Ignored when `UC_gap_fill` is `FALSE`.
#' - `UC_smooth_edge` logical (default: `TRUE`).
#'
#'      Whether to perform edge smoothing. If `TRUE`, edges of urban centres are smoothed with the function `UC_smooth_edge_fun`.
#' - `UC_smooth_edge_fun` character / function (default: `"majority_rule_R2023A"`).
#'
#'      Function used to smooth the edges of urban centres. Ignored when `UC_smooth_edge` is `FALSE`. Possible values are:
#'      - `"majority_rule_R2022A"` to use the edge smoothing algorithm in GHSL Data Package 2022 (see section "Edge smoothing" below)
#'      - `"majority_rule_R2023A"` to use the edge smoothing algorithm in GHSL Data Package 2023 (see section "Edge smoothing" below)
#'      - a custom function with a signature similar as [apply_majority_rule()].
#' - `DUC_size_threshold` numeric (default: `1500`).
#'
#'      Minimum total population size required for a dense urban cluster
#' - `DUC_built_criterium` logical (default: `TRUE`).
#'
#'      Whether to use the additional built-up area criterium (see section "Built-up area criterium" below). If `TRUE`, not only cells that meet the population density requirement will be considered when delineating dense urban clusters, but also cells with a built-up area per permanent land above the `DUC_built_threshold`
#' - `DUC_built_threshold` numeric or character (default: `0.2`).
#'
#'      Additional built-up area threshold. Can be a value between `0` and `1`, representing the minimum built-up area per permanent land, or `"optimal"` (see section "Built-up area criterium" below). Ignored when `DUC_built_criterium` is `FALSE`.
#' - `DUC_contiguity_rule` integer (default: `4`).
#'
#'      Which cells are considered adjacent in dense urban clusters: `4` for rooks case (horizontal and vertical neighbors) or `8` for queens case (horizontal, vertical and diagonal neighbors)
#' - `SDUC_density_threshold` numeric (default: `300`).
#'
#'      Minimum population density per permanent land of a cell required to belong to a semi-dense urban cluster
#' - `SDUC_size_threshold` numeric (default: `5000`).
#'
#'      Minimum total population size required for a semi-dense urban cluster
#' - `SDUC_contiguity_rule` integer (default: `8`).
#'
#'      Which cells are considered adjacent in semi-dense urban clusters: `4` for rooks case (horizontal and vertical neighbors) or `8` for queens case (horizontal, vertical and diagonal neighbors)
#' - `SDUC_buffer_size` integer (default: `3`).
#'
#'      The minimum distance to urban centres and dense urban clusters required for a semi-dense urban cluster
#' - `SUrb_density_threshold` numeric (default: `300`).
#'
#'      Minimum population density per permanent land of a cell required to belong to a suburban or peri-urban area
#' - `SUrb_size_threshold` numeric (default: `5000`).
#'
#'      Minimum total population size required for a suburban or peri-urban area
#' - `SUrb_contiguity_rule` integer (default: `8`).
#'
#'      Which cells are considered adjacent in suburban or peri-urban area: `4` for rooks case (horizontal and vertical neighbors) or `8` for queens case (horizontal, vertical and diagonal neighbors)
#' - `RC_density_threshold` numeric (default: `300`).
#'
#'      Minimum population density per permanent land of a cell required to belong to a rural cluster
#' - `RC_size_threshold` numeric (default: `500`).
#'
#'      Minimum total population size required for a rural cluster
#' - `RC_contiguity_rule` integer (default: `8`).
#'
#'      Which cells are considered adjacent in rural clusters:  `4` for rooks case (horizontal and vertical neighbors) or `8` for queens case (horizontal, vertical and diagonal neighbors)
#' - `LDR_density_threshold` numeric (default: `50`).
#'
#'      Minimum population density per permanent land of a low density rural grid cell
#' - `water_land_threshold` numeric (default: `0.5`).
#'
#'      Maximum proportion of permanent land allowed in a water cell
#' - `water_pop_threshold` numeric (default: `0`).
#'
#'      Maximum population size allowed in a water cell
#' - `water_built_threshold` numeric (default: `0`).
#'
#'      Maximum built-up area allowed in a water cell
#'
#'
#' @section Built-up area criterium:
#'
#' In Data Package 2022, the Degree of Urbanisation includes an optional built-up area criterium to account for the presence of office parks, shopping malls, factories and transport infrastructure. When the setting is enabled, urban centres (and dense urban clusters) are created using both cells with a population density of at least 1500 inhabitants per km² *and* cells that have at least 50% built-up area on permanent land. For more information: see [GHSL Data Package 2022, footnote 25](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2022.pdf). The parameter settings `UC_built_criterium=TRUE` and `UC_built_threshold=0.5` (level 1 & 2) and `DUC_built_criterium=TRUE` and `DUC_built_threshold=0.5` (level 2) reproduce this built-up area criterium in urban centres and dense urban clusters respectively.
#'
#' In Data Package 2023, the built-up area criterium is slightly adapted and renamed to the "Reduce Fragmentation Option". Instead of using a fixed threshold of built-up area per permanent land of 50%, an "optimal" threshold is employed. The optimal threshold is dynamically identified as the global average built-up area proportion in clusters with a density of at least 1500 inhabitants per permanent land with a minimum population of 5000 people. We determined empirically that this optimal threshold is 20% for the data of 2020. For more information: see [GHSL Data Package 2023, footnote 30](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf). The "Reduce Fragmentation Option" can be reproduced with the parameter settings `UC_built_criterium=TRUE` and `UC_built_threshold="optimal"` (level 1 & 2) and `DUC_built_criterium=TRUE` and `DUC_built_threshold="optimal"` (level 2). In addition, the parameter `built_optimal_data` must contain the path to the directory with the (global) data to compute the optimal built-up area threshold.
#'
#'
#' @section Edge smoothing:
#'
#' In Data Package 2022, edges of urban centres are smoothed by an iterative majority rule. The majority rule works as follows: if a cell has at least five of the eight surrounding cells belonging to an unique urban centre, then the cell is added to that urban centre. The process is iteratively repeated until no more cells are added. The parameter setting `UC_smooth_edge=TRUE` and `UC_smooth_edge_fun="majority_rule_R2022A"` reproduces this edge smoothing rule.
#'
#' In Data Package 2023, the majority rule is slightly adapted. A cell is added to an urban centre if the majority of the surrounding cells belongs to an unique urban centre, with majority only computed among populated or land cells (proportion of permanent land > 0.5). In addition, cells with permanent water are never added to urban centres. The process is iteratively repeated until no more cells are added. For more information: see [GHSL Data Package 2023, footnote 29](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf). The parameter setting `UC_smooth_edge=TRUE` and `UC_smooth_edge_fun="majority_rule_R2023A"` reproduces this edge smoothing rule.
#'
#' @section Regions:
#' Because of the large amount of data at a global scale, the grid classification procedure is quite memory-consuming. To optimise the procedure, we divided the world in 9 pre-defined regions. These regions are the smallest grouping of GHSL tiles while ensuring that no continuous land mass is split into two different regions (for more information, see the figure below and [`GHSL_tiles_per_region`]).
#'
#' If `regions=TRUE`, a global grid classification is created by (1) executing the grid classification procedure separately in the 9 pre-defined regions, and (2) afterwards merging these classifications together. The argument `data` should contain the path to a directory with the data of all pre-defined regions (for example as created by `download_GHSLdata(... extent="regions"`). Note that although the grid classification is optimised, it still takes approx. 145 minutes and requires 116 GB RAM to execute the grid classification with the standard parameters (performed on a Kubernetes server with 32 cores and 256 GB RAM). For a concrete example on how to construct the grid classification on a global scale, see `vignette("vig3-global-scale")`. 
#'
#' \figure{figure_GHSL_tiles_per_region.png}{GHSL tiles}
#'
#' @examples
#' # load the data
#' data_belgium <- DoU_load_grid_data_belgium()
#'
#' # classify with standard parameters:
#' classification1 <- classify_grid(data = data_belgium)
#' DoU_plot_grid(classification1)
#'
#' # classify with custom parameters:
#' classification2 <- classify_grid(
#'   data = data_belgium,
#'   parameters = list(
#'     UC_density_threshold = 3000,
#'     UC_size_threshold = 75000,
#'     UC_gap_fill = FALSE,
#'     UC_smooth_edge = FALSE,
#'      UCL_contiguity_rule = 4
#'   )
#' )
#' DoU_plot_grid(classification2)
#'
#' \dontrun{
#' # classify according to GHSL Data Package 2022 (level 1)
#' classification_R2022A <- classify_grid(
#'   data = grid_data,
#'   parameters = list(
#'     UC_built_criterium = TRUE,
#'     UC_built_threshold = 0.5,
#'     UC_smooth_edge_fun = "majority_rule_R2022A"
#'   )
#' )
#'
#' # classify according to GHSL Data Package 2023  (level 1)
#' # (assuming the directory "data/global" contains global data)
#' classification_R2023A <- classify_grid(
#'   data = grid_data,
#'   parameters = list(
#'     UC_built_criterium = TRUE,
#'     UC_built_threshold = "optimal",
#'     built_optimal_data = "data/global",
#'     UC_smooth_edge_fun = "majority_rule_R2023A"
#'   )
#' )
#'
#' # classify in regions (assuming the directory "data/regions contains data
#' # for the regions)
#' classification_in_regions <- classify_grid(
#'   data = "data/regions",
#'   regions = TRUE
#' )
#' }
#' @export
classify_grid <- function(data,
                          level1 = TRUE,
                          parameters = NULL,
                          values = NULL,
                          regions = FALSE,
                          filename = NULL) {
  
  # check values argument
  if (is.null(values)) {
    values <- if (level1) c(3, 2, 1, 0) else c(30, 23, 22, 21, 13, 12, 11, 10)
  }
  if ((level1 & length(values) != 4) || (!level1 & length(values) != 8)) {
    stop("Invalid argument: the length of the values argument is not correct.")
  }
  
  # IN REGIONS
  if (regions) {
    # check if the data is valid
    if (!is.character(data)) {
      stop("Invalid argument: if regions=TRUE, 'data' should contain the path to the directory where the data for the regions is saved")
    }
    if (!all(names(flexurba::GHSL_tiles_per_region) %in% list.files(data))) {
      stop("Invalid argument: data directory does not contain the data for all regions, see download_GHSLdata(... extent='regions') to download the data")
    }
    
    # create a list to save the classifications
    classifications <- list()
    count <- 0
    
    # for each region, generate the classification
    for (region in names(flexurba::GHSL_tiles_per_region)) {
      
      # create directory to save the classification file
      if (!dir.exists(file.path(data, region, "classifications"))) {
        dir.create(file.path(data, region, "classifications"))
      }
      
      # execute grid classification
      classification1 <- classify_grid(
        data = file.path(data, region),
        level1 = level1,
        parameters = parameters,
        values = values,
        regions = FALSE,
        filename = file.path(data, region, "classifications", basename(filename))
      )
      
      # save classification in list
      count <- count + 1
      classifications[[count]] <- classification1
    }
    
    # merge the classification per region
    classification <- do.call(terra::mosaic, c(classifications, fun = "max"))
    
    # some ocean areas are not covered by the regions, so extend to cover them
    valid_mollweide <- terra::rast(system.file("extdata", "valid-mollweide.tif", package = "flexurba"))
    classification <- terra::extend(classification, valid_mollweide)
    
    # set NA values = water value
    terra::set.values(classification, which(is.na(classification[])), values[[length(values)]])
    
    # but mask non-valid mollweide cells
    classification <- mask_mollweide(classification)
    
    
    
    # NOT IN REGIONS
  } else {
    
    # get the default parameters
    default_parameters <- DoU_get_grid_parameters(level1)
    
    # check if all provided parameters are valid
    diff <- setdiff(names(parameters), names(default_parameters))
    if (length(diff) > 0) {
      stop(paste("Invalid argument: the following parameter values are not valid", paste(diff, collapse = " ")))
    }
    
    # merge default parameters with provided parameters to get all parameters
    default_parameters <- within(default_parameters, rm(list = names(parameters)))
    parameters <- c(parameters, default_parameters)
    
    
    # read data
    if (is.character(data)) {
      data <- DoU_preprocess_grid(data)
    }
    
    # check if data is valid
    if (!all(c("pop", "land", "pop_per_land", "built_per_land", "built") %in% names(data))) {
      stop("Invalid argument: The data should contain a named list of pop, land, built, pop_per_land and built_per_land as generated by the function DoU_preprocess_grid.")
    }
    
    # execute the classification
    if (level1) {
      classification <- classify_grid_level1(data, parameters, values)
    } else {
      classification <- classify_grid_level2(data, parameters, values)
    }
    
    # if a buffer was applied when cropping the grid, remove the buffer again
    if (!is.null(data$metadata_POP$buffer) && (data$metadata_POP$buffer > 0)){
      classification <- terra::crop(classification, terra::ext(data$pop) - data$metadata_POP$buffer)
    }
  }
  
  
  # WRITE OUTPUT
  if (!is.null(filename)) {
    # create directory if it does not exist
    if (!dir.exists(dirname(filename))) {
      dir.create(dirname(filename))
    }
    
    # check the filename
    if (!endsWith(filename, ".tif")) {
      stop("Invalid argument: filename should have extention '.tif'")
    }
    
    # write the output
    terra::writeRaster(classification, filename, overwrite = TRUE)
    
    # construct the metadata
    parameters$file <- filename
    if (!is.null(data) & !regions) {
      parameters$metadata_BUILT_S <- data$metadata_BUILT_S
      parameters$metadata_POP <- data$metadata_POP
      parameters$metadata_LAND <- data$metadata_LAND
    }
    parameters$from_predefined_regions <- regions
    parameters$values <- values
    if (is.list(parameters$built_optimal_data)) {
      parameters$built_optimal_data <- "from_object"
    }
    
    # write the metadata
    metadata_file <- paste0(gsub(".tif", ".json", filename))
    write_metadata(metadata_file, parameters)
  }
  
  return(classification)
}




#' Helper function for grid classification level 1
#'
#' @description
#' The function reconstructs the grid cell classification of the Degree of Urbanisation. 
#' @param data path to the directory with the data, or named list with the data as returned by function [DoU_preprocess_grid()]
#' @param parameters named list with the parameters to adapt the standard specifications in the Degree of Urbanisation classification.
#' @param values vector with the values assigned to the different classes in the resulting classification: (1) urban centres, (2) urban clusters, (3) rural grid cells and (4) water cells.
#' @returns SpatRaster
#' @noRd
classify_grid_level1 <- function(data, parameters, values) {
  
  # identify 'optimal' built-up area threshold
  if ((parameters$UC_built_threshold == "optimal") && parameters$UC_built_criterium == TRUE) {
    if (is.null(parameters$built_optimal_data)) {
      stop(paste("Invalid argument: no optimal built threshold parameter can be determined, because built_optimal_data is not provided"))
    }
    
    # get the optimal threshold
    parameters$UC_built_threshold <- ceiling(flexurba::DoU_get_optimal_builtup(
      parameters$built_optimal_data,
      parameters$UC_density_threshold,
      parameters$UCL_size_threshold,
      parameters$UC_contiguity_rule
    ) * 100) / 100
  }
  
  # CLASS 3: URBAN CENTRES
  classification <- flexurba::DoU_classify_grid_urban_centres(
    data = data,
    density_threshold = parameters$UC_density_threshold,
    size_threshold = parameters$UC_size_threshold,
    contiguity_rule = parameters$UC_contiguity_rule,
    built_criterium = parameters$UC_built_criterium,
    built_threshold = parameters$UC_built_threshold,
    gap_fill = parameters$UC_gap_fill,
    max_gap = parameters$UC_max_gap,
    smooth_edge = parameters$UC_smooth_edge,
    smooth_edge_fun = parameters$UC_smooth_edge_fun,
    smooth_pop = parameters$UC_smooth_pop,
    smooth_pop_window = parameters$UC_smooth_pop_window,
    value = values[[1]]
  )
  
  # CLASS 2: URBAN CLUSTERS
  classification <- flexurba::DoU_classify_grid_urban_clusters(
    data = data,
    density_threshold = parameters$UCL_density_threshold,
    size_threshold = parameters$UCL_size_threshold,
    contiguity_rule = parameters$UCL_contiguity_rule,
    smooth_pop = parameters$UCL_smooth_pop,
    smooth_pop_window = parameters$UCL_smooth_pop_window,
    classification = classification,
    value = values[[2]]
  )
  
  # CLASS 1: RURAL GRID CELLS
  classification <- flexurba::DoU_classify_grid_rural(
    data = data,
    classification = classification,
    value = values[[3]]
  )
  
  # CLASS 0: WATER
  classification <- flexurba::DoU_classify_grid_water(
    data = data,
    classification = classification,
    water_land_threshold = parameters$water_land_threshold,
    water_pop_threshold = parameters$water_pop_threshold,
    water_built_threshold = parameters$water_built_threshold,
    value = values[[4]],
    allow_overwrite = c(values[[3]])
  )
  
  names(classification) <- c("layer")
  
  return(classification)
}


#' Helper function for grid classification level 2
#'
#' @description
#' The function reconstructs the grid cell classification of the Degree of Urbanisation. 
#' @param data path to the directory with the data, or named list with the data as returned by function [DoU_preprocess_grid()]
#' @param parameters named list with the parameters to adapt the standard specifications in the Degree of Urbanisation classification.
#' @param values vector with the values assigned to the different classes in the resulting classification: (1) urban centres, (2) dense urban clusters, (3) semi-dense urban clusters, (4) suburban or peri-urban cells, (5) rural clusters, (6) low density rural cells, (7) very low density rural cells and (8) water cells.
#' @returns SpatRaster
#' @noRd
classify_grid_level2 <- function(data, parameters, values) {
  
  # identify 'optimal' built-up area threshold if necessary
  if ((parameters$UC_built_threshold == "optimal") && (parameters$UC_built_criterium == TRUE) ||
      (parameters$DUC_built_threshold == "optimal") && (parameters$DUC_built_criterium == TRUE)) {
    
    # check if the argument is valid
    if (is.null(parameters$built_optimal_data)) {
      stop(paste("Invalid argument:", "no optimal built threshold parameter can be determined, because built_optimal_data is not provided"))
    }
    
    # get the optimal threshold
    optimal_threshold <- ceiling(flexurba::DoU_get_optimal_builtup(
      parameters$built_optimal_data,
      parameters$DUC_density_threshold,
      parameters$DUC_size_threshold,
      parameters$DUC_contiguity_rule
    ) * 100) / 100
    
    # store the optimal threshold
    if ((parameters$UC_built_threshold == "optimal") && (parameters$UC_built_criterium == TRUE)) {
      parameters$UC_built_threshold <- optimal_threshold
    }
    
    # store the optimal threshold
    if ((parameters$DUC_built_threshold == "optimal") && (parameters$DUC_built_criterium == TRUE)) {
      parameters$DUC_built_threshold <- optimal_threshold
    }
  }
  
  
  # CLASS 30: URBAN CENTRES
  classification <- flexurba::DoU_classify_grid_urban_centres(
    data = data,
    density_threshold = parameters$UC_density_threshold,
    size_threshold = parameters$UC_size_threshold,
    contiguity_rule = parameters$UC_contiguity_rule,
    built_criterium = parameters$UC_built_criterium,
    built_threshold = parameters$UC_built_threshold,
    gap_fill = parameters$UC_gap_fill,
    max_gap = parameters$UC_max_gap,
    smooth_edge = parameters$UC_smooth_edge,
    smooth_edge_fun = parameters$UC_smooth_edge_fun,
    value = values[1]
  )
  
  # CLASS 23: DENSE URBAN CLUSTERS: 
  # dense urban clusters are similarly identified as urban centres (a minimum density, minimum built-up and minimum size criteria)
  # but without gap filling and edge smoothing
  dense_urban_cluster <- flexurba::DoU_classify_grid_urban_centres(
    data = data,
    density_threshold = parameters$DUC_density_threshold,
    size_threshold = parameters$DUC_size_threshold,
    contiguity_rule = parameters$DUC_contiguity_rule,
    built_criterium = parameters$DUC_built_criterium,
    built_threshold = parameters$DUC_built_threshold,
    gap_fill = FALSE,
    smooth_edge = FALSE,
    value = values[2]
  )
  classification <- terra::cover(classification, dense_urban_cluster)
  
  
  # CLASS 22: SEMI-DENSE URBAN CLUSTERS
  # a semi-dense urban cluster is at least 3 km away from dense urban cluster and urban centres.
  # so, create a buffer around the current classification of dense urban clusters and urban centres
  buffer <- classification
  for (i in 1:(parameters$SDUC_buffer_size - 1)) {
    buffer <- flexurba::get_adjacent(buffer)
  }
  
  # get semi-dense urban clusters
  urban_clusters <- flexurba::get_clusters(
    xden = data$pop_per_land,
    minden = parameters$SDUC_density_threshold,
    xsiz = data$pop,
    minsiz = parameters$SDUC_size_threshold,
    directions = parameters$SDUC_contiguity_rule
  )
  
  # check if the semi-dense urban clusters overlap with the buffer
  overlapping_clusters <- terra::values(urban_clusters)[which(!is.na(buffer[]))] %>%
    unique()
  
  # only keep the semi-dense urban clusters if they do not overlap with the buffer
  terra::set.values(
    classification, which(is.na(classification[]) &
                            !is.na(urban_clusters[]) &
                            !(urban_clusters[] %in% overlapping_clusters)),
    values[3]
  )
  
  # CLASS 21: SURBURBAN OR PERI-URBAN
  urban_clusters <- flexurba::get_clusters(
    xden = data$pop_per_land,
    minden = parameters$SUrb_density_threshold,
    xsiz = data$pop,
    minsiz = parameters$SUrb_size_threshold,
    directions = parameters$SUrb_contiguity_rule
  )
  
  terra::set.values(
    classification, which(is.na(classification[]) &
                            !is.na(urban_clusters[])),
    values[4]
  )
  
  
  # CLASS 13: RURAL CLUSTERS
  # rural clusters are similarly identified as urban clusters (minimum density and minimum size criteria)
  classification <- flexurba::DoU_classify_grid_urban_clusters(
    data = data,
    density_threshold = parameters$RC_density_threshold,
    size_threshold = parameters$RC_size_threshold,
    contiguity_rule = parameters$RC_contiguity_rule,
    classification = classification,
    value = values[5]
  )
  
  # check the argument
  if (!is.numeric(parameters$LDR_density_threshold)) {
    stop(paste("Invalid argument:", parameters$LDR_density_threshold, "is not a valid parameter for the minimum density threshold"))
  }
  
  # CLASS 12: LOW DENSITY RURAL CELLS
  # all remaining cell with a population density per permanent land above the low density threshold
  terra::set.values(classification, which(is.na(classification[]) & (data$pop_per_land[] >= parameters$LDR_density_threshold)), values[6])
  
  # CLASS 11: VERY LOW DENSITY RURAL CELLS
  # all remaining cells
  classification <- flexurba::DoU_classify_grid_rural(
    data = data,
    classification = classification,
    value = values[7]
  )
  
  # CLASS 10: cells
  classification <- flexurba::DoU_classify_grid_water(
    data = data,
    classification = classification,
    water_land_threshold = parameters$water_land_threshold,
    water_pop_threshold = parameters$water_pop_threshold,
    water_built_threshold = parameters$water_built_threshold,
    value = values[8],
    allow_overwrite = c(values[7])
  )
  
  # mask non-valid mollweide cells
  classification <- mask_mollweide(classification)
  
  names(classification) <- c("layer")
  
  return(classification)
}
