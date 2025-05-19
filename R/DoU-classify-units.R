#' Create the DEGURBA spatial units classification
#'
#' @description
#' The function reconstructs the spatial units classification of the Degree of Urbanisation based on the grid cell classification.
#' @param data named list with the required data, as returned by the function [DoU_preprocess_units()]
#' @param id character. Unique column in the `units` data as id for spatial units
#' @param level1 logical. Whether to classify the spatial units according to first hierarchical level (`TRUE`) or the second hierarchical level (`FALSE`). For more details, see section "Classification rules" below.
#' @param values vector with the values assigned to the different classes in the resulting units classification:
#'    - If `level1=TRUE`: the vector should contain the values for (1) cities, (2) town and semi-dense areas and (3) rural areas.
#'    - If `level1=FALSE`: the vector should contain the values for (1) cities, (2) dense towns, (3) semi-dense towns, (4) suburb or peri-urban areas, (5) villages, (6) dispersed rural areas and (7) mostly uninhabited areas.
#' @param official_workflow logical. Whether to employ the official workflow of the GHSL (`TRUE`) or the alternative workflow (`FALSE`). For more details, see section "Workflow" below.
#' @param rules_from_2021 logical. Whether to employ the original classification rules as described in the 2021 version of the DEGURBA manual. The DEUGURBA Level 2 unit classification rules have been modified in July 2024. By default, the function uses the most recent rules as described in the [online version](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Applying_the_degree_of_urbanisation_manual) of the methodological manual. For more details, see section "Modification of the unit classification rules" below.
#' @param filename character. Output filename (csv). The resulting classification together with a metadata file (in JSON format) will be saved if `filename` is not `NULL`.
#' @return dataframe with for each spatial unit the classification and the share of population per grid class
#' @section Classification rules:
#'
#' The Degree of Urbanisation consists of two hierarchical levels. In level 1, the spatial units are classified in cities, towns and semi-dense areas, and rural areas. In level 2, towns and semi-dense areas are further divided in dense towns, semi-dense towns and suburban or peri-urban areas. Rural areas are further divided in villages, dispersed rural areas and mostly uninhabited areas.
#'
#' The detailed classification rules are as follows:
#'
#' **LEVEL 1:**
#'
#' - **Cities:** units that have at least 50% of their population in urban centres
#' - **Towns and semi-dense areas:** units that have less than 50% of their population in urban centres and no more than 50% of their population in rural grid cells
#' - **Rural areas:** units that have more then 50% of their population in rural grid cells
#'
#' **LEVEL 2:**
#'
#' - **Cities:** units that have at least 50% of their population in urban centres
#' - **Dense towns:** units that have at least 50% of their population in the combination of urban centres and dense urban clusters
#' - **Semi-dense towns:** units that have less than 50% of their population in the combination of urban centres and dense urban clusters, or have less than 50% of their population in the combination of suburban and peri-urban cells and rural grid cells
#' - **Suburbs or peri-urban areas:** units that have at least 50% of their population in the combination of suburban and peri-urban cells and rural grid cells
#' - **Villages**: units that have at least 50% of their population in the combination of urban centres, urban clusters and rural clusters
#' - **Dispersed rural areas**: units that have less than 50% of their population in the combination of urban centres, urban clusters and rural clusters, or have less than 50% of their population in very low-density rural grid cells
#' - **Mostly uninhabited areas**: units that have at least 50% of their population in very low-density rural grid cells
#'
#' @section Workflow:
#'
#' The classification of small spatial units requires a vector layer with the small spatial units, a raster layer with the grid cell classification, and a raster layer with the population grid. Standard, a population grid of 100 m resolution is used by the Degree of Urbanisation.
#'
#' The function includes two different workflows to establish the spatial units classification based on these three data sources.
#'
#' **Official workflow according to the GHSL:**
#'
#' For the official workflow, the three layers should be pre-processed by [DoU_preprocess_units()]. In this function, the classification grid and population grid are resampled to a user-defined `resample_resolution` with the nearest neighbour algorithm (the Degree of Urbanisation uses standard a resample resolution of 50 m). In doing this, the values of the population grid are divided by the oversampling ratio (for example: going from a resolution of 100 m to a resolution of 50 m, the values of the grid are divided by 4).
#'
#' Afterwards, the spatial units classification is constructed with [DoU_classify_units()] as follows. The vector layer with small spatial units is rasterised to match the population and classification grid. Based on the overlap of the three grids, the share of population per flexurba grid class is computed per spatial unit with a zonal statistics procedure. The units are subsequently classified according to the classification rules (see above).
#'
#' Apart from this, there are two special cases. First, if a unit has no population, it is classified according to the share of *land area* in each of the flexurba grid classes (computed with a zonal statistics procedure). Second, if a unit initially could not be rasterised (can occur if the area of the unit < `resample_resolution`), then it is processed separately as follows. The unit is individually rasterised by all touching cells. The unit is classified according to the share of population in the flexurba grid classes in these touching cells. However, to avoid double counting of population, no population is assigned to the unit in the result.
#'
#' For more information about the official workflow to construct the units classification, see [GHSL Data Package 2023 (Section 2.7.2.3)](https://ghsl.jrc.ec.europa.eu/documents/GHSL_Data_Package_2023.pdf).
#'
#' **Alternative workflow:**
#'
#' Besides the official workflow of the GHSL, the function also includes an alternative workflow to construct the spatial units classification. The alternative workflow does not require rasterising the spatial units layer, but relies on the overlap between the spatial units layer and the grid layers.
#'
#' The three layers should again be pre-processed by the function [DoU_preprocess_units()], but this time without `resampling_resolution`. For the classification in [DoU_classify_units()],  the function [exactextractr::exact_extract()] is used to (1) overlay the grids with the spatial units layer, and (2) summarise the values of the population grid and classification grid per unit. The units are subsequently classified according to the classification rules (see above). As an exception, if a unit has no population, it is classified according to the share of *land area* in each of the flexurba grid classes. The alternative workflow is slightly more efficient as it does not require resampling the population and classification grids and rasterising the spatial units layer.
#' 
#' @section Modification of the unit classification rules:
#'
#' The unit classification rules of Level 2 of DEGURBA were updated in July 2024. By default, the function [DoU_classify_units()] applies the latest classification rules, as described in the [online version](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Applying_the_degree_of_urbanisation_manual) of the methodological manual. However, you can also use the original 2021 classification rules if desired, by setting the argument `rules_from_2021` to `TRUE`. In that case, the rules to classify units are as follows:
#' 
#' - **Cities:** units that have at least 50% of their population in urban centres
#' - **Dense towns:** units that have a larger share of the population in dense urban clusters than in semi-dense urban clusters, and that have a larger share of the population in dense + semi-dense urban clusters than in suburban or peri-urban cells
#' - **Semi-dense towns:** units that have a larger share of the population in semi-dense urban clusters than in semi-dense urban clusters, and that have a larger share of the population in dense + semi-dense urban clusters than in suburban or peri-urban cells
#' - **Suburbs or peri-urban areas:** units that have a larger share in suburban or peri-urban cells than in dense + semi-dense urban clusters
#' - **Villages**: units that have the largest share of their rural grid cell population living in rural clusters
#' - **Dispersed rural areas**: units that have the largest share of their rural grid cell population living in low density rural grid cells
#' - **Mostly uninhabited areas**: units that have the largest share of their rural grid cell population living in very low density rural grid cells
#'
#' @examples
#' # load the grid data
#' data_belgium <- flexurba::DoU_load_grid_data_belgium()
#' # load the units and filter for West-Flanders
#' units_data <- flexurba::units_belgium %>%
#'   dplyr::filter(GID_2 == "30000")
#' # classify the grid
#' classification <-DoU_classify_grid(data = data_belgium)
#'
#' \donttest{
#' # official workflow
#' data1 <- DoU_preprocess_units(
#'   units = units_data,
#'   classification = classification,
#'   pop = data_belgium$pop,
#'   resample_resolution = 50
#' )
#' units_classification1 <- DoU_classify_units(data1)
#' }
#'
#' # alternative workflow
#' data2 <- DoU_preprocess_units(
#'   units = units_data,
#'   classification = classification,
#'   pop = data_belgium$pop
#' )
#' units_classification2 <- DoU_classify_units(data2, official_workflow = FALSE)
#'
#' # spatial units classification, dissolved at level 3 (Belgian districts)
#' data3 <- DoU_preprocess_units(
#'   units = units_data,
#'   classification = classification,
#'   pop = data_belgium$pop,
#'   dissolve_units_by = "GID_3"
#' )
#' units_classification3 <- DoU_classify_units(data3, id = "GID_3")
#' @export
DoU_classify_units <- function(data, id = "UID",
                           level1 = TRUE,
                           values = NULL,
                           official_workflow = TRUE,
                           rules_from_2021 = FALSE,
                           filename = NULL) {
  
  # global variable
  .data <- NULL
  
  # use default values if values are not provided
  if (is.null(values)) {
    values <- if (level1) c(3, 2, 1) else c(30, 23, 22, 21, 13, 12, 11)
  }
  
  # check if the values are valid
  if ((level1 & length(values) != 3) | (!level1 & length(values) != 7)) {
    stop("Invalid argument: the length of the values argument is not correct.")
  }
  if (length(setdiff(unlist(terra::unique(data$classification)), values)) == length(unlist(terra::unique(data$classification)))) {
    stop("Invalid argument: values does not contain values that are present in the classification grid.")
  }
  
  # check the other arguments
  if (!(id %in% names(data$units))) {
    stop(paste("Invalid argument:", id, "is not a column in the units data."))
  }
  if (!all(c("classification", "pop", "units") %in% names(data))) {
    stop("Invalid argument: data. \n The data should contain a named list of classification, pop and units as generated by the function DoU_preprocess_units.")
  }
  
  # construct the metadata
  metadata <- as.list(environment())
  
  
  # OFFICIAL WORKFLOW
  if (official_workflow) {
    
    # rasterise the units
    units_grid <- terra::rasterize(data$units, data$pop, field = id)
    
    # Compute share of population per class
    df <- compute_share_in_classes(data$pop, units_grid, data$classification, level1 = level1, values = values)
    
  } else {
    
    # create a grid with two layers: the classification and the population layers
    grid <- data$classification
    names(grid) <- "class"
    grid$pop <- data$pop
    
    # overlap the grids with the spatial units
    gridoverlap <- exactextractr::exact_extract(
      x = grid,
      y = data$units,
      include_cols = id,
      progress = FALSE
    ) %>%
      dplyr::bind_rows()
    
    # Compute share of population per class
    df <- compute_share_in_classes2(gridoverlap, level1 = level1, values = values)
  }
  
  
  # Determine flexurba units classification
  df <- apply_unit_classification_rules(df, level1 = level1, values = values, rules_from_2021 = rules_from_2021)
  
  
  
  ##### Exception 1 : If a unit contains no population: determine classification based on share of land per class
  
  if (official_workflow) {
    
    # get units with no population
    terra::set.values(units_grid, which(!(units_grid[] %in% df[df$Tot_Pop == 0, id])), NA)
    
    # Compute share of land per class
    df_no_pop <- compute_share_in_classes(data$pop, units_grid, data$classification, population = FALSE, level1 = level1, values = values)
  } else {
    
    # get units with no population
    no_pop <- df[df$Tot_Pop == 0, id]
    
    # if there are units with no population: compute share of land per class
    if (!(length(no_pop) == 0)) {
      gridoverlap_no_pop <- gridoverlap %>%
        dplyr::filter(.data[[id]] %in% no_pop)
      df_no_pop <- compute_share_in_classes2(gridoverlap_no_pop, population = FALSE, level1 = level1, values = values)
    } else {
      df_no_pop <- data.frame()
    }
  }
  
  # if there are units with no population: add to the previous data.frame
  if (nrow(df_no_pop) != 0) {
    
    # Determine flexurba units classification
    df_no_pop <- apply_unit_classification_rules(df_no_pop, level1 = level1, values = values, rules_from_2021 = rules_from_2021)
    
    # Insert in general data.frame
    df$flexurba_L1[match(df_no_pop[[id]], df[[id]])] <- df_no_pop$flexurba_L1
    
    if (level1 == FALSE) {
      df$flexurba_L2[match(df_no_pop[[id]], df[[id]])] <- df_no_pop$flexurba_L2
    }
  }
  
  if (official_workflow) {
    
    ##### Exception 2 : If a unit could not be rasterised: rasterise it separately considering all touching raster cells
    
    # get all units that were not rasterised
    no_raster <- setdiff(data$units[[id]], df[[id]])
    
    # rasterise them separately considering all touching raster cells and calculate the classification
    for (no_raster_id in no_raster) {
      rasterized <- terra::rasterize(data$units %>% dplyr::filter(.data[[id]] == no_raster_id), data$pop, field = id, touches = TRUE)
      df_no_raster <- compute_share_in_classes(data$pop, rasterized, data$classification, level1 = level1, values = values)
      
      # if there is no population in the rasterized version, use the share of land instead
      if (df_no_raster$Tot_Pop[[1]] == 0) {
        df_no_raster <- compute_share_in_classes(data$pop, rasterized, data$classification, population = FALSE, level1 = level1, values = values)
      }
      
      df_no_raster <- apply_unit_classification_rules(df_no_raster, level1 = level1, values = values, rules_from_2021 = rules_from_2021)
      
      # Insert in general data.frame
      if (level1) {
        df[nrow(df) + 1, ] <- c(df_no_raster[[id]], rep(NA, 10))
        df[nrow(df), 'flexurba_L1'] <- df_no_raster$flexurba_L1
      } else {
        df[nrow(df) + 1, ] <- c(df_no_raster[[id]], rep(NA, 23))
        df[nrow(df), 'flexurba_L1'] <- df_no_raster$flexurba_L1
        df[nrow(df), 'flexurba_L2'] <- df_no_raster$flexurba_L2
      }
    }
  }
  
  
  # write the results
  if (!is.null(filename)) {
    
    # create directory if not exists
    if (!dir.exists(dirname(filename))) {
      dir.create(dirname(filename))
    }
    
    # check filename
    if (!endsWith(filename, ".csv")) {
      stop("Invalid argument: filename should have extention '.csv'")
    }
    
    # write the csv
    utils::write.csv(df, filename)
    
    # write the metadata
    metadata <- within(metadata, rm(data))
    metadata$data <- data$metadata
    metadata_file <- gsub(".csv", ".json", filename)
    write_metadata(metadata_file, metadata)
  }
  
  return(df)
}



#' Helper function to compute the share of population/land in flexurba grid classes, used in the standard workflow when classifying spatial units
#'
#' @description
#' The function reconstructs the spatial units classification of the Degree of Urbanisation based on the grid cell classification (without exceptions).

#' @param pop SpatRaster. Population grid
#' @param units_grid SpatRaster. Rasterised version of the units
#' @param classification SpatRaster. Grid cell classification
#' @param population logical. Whether to compute the share of population (TRUE), or the share of area (FALSE)
#' @param level1 logical. Whether the classification is according to level 1 (TRUE) or level 2 (FALSE)
#' @param values vector with the values assigned to the different classes in the resulting units classification:
#'    - If `level1=TRUE`: the vector should contain the values for (1) cities, (2) town and semi-dense areas and (3) rural areas.
#'    - If `level1=FALSE`: the vector should contain the values for (1) cities, (2) dense towns, (3) semi-dense towns, (4) suburb or peri-urban areas, (5) villages, (6) dispersed rural areas and (7) mostly uninhabited areas.
#' @return dataframe with for each spatial unit the classification and the share of population per grid class
#' @noRd
compute_share_in_classes <- function(pop, units_grid, classification, population = TRUE, level1 = TRUE, values = NULL) {
  # get the column name of the id
  id <- names(units_grid)
  
  # check if the resolution of the grids are the same
  if (length(unique(c(terra::res(pop), terra::res(units_grid), terra::res(classification)))) != 1) {
    stop("Invalid argument: the resolution of the input grids don't match")
  }
  
  # get classes with the values
  classes <- get_class_names(level1, values)
  
  df_list <- list()
  
  i <- 1
  
  # compute the share of population: (sum of pop values)
  if (population) {
    func <- "sum"
    updatevalue <- 0
    name <- "Pop"
  } else {
    # compute the share of area: (number of pop cells)
    func <- "notNA"
    updatevalue <- NA
    name <- "Area"
  }
  
  # compute total population / area per unit
  df_list[[i]] <- terra::zonal(pop, units_grid, fun = func, na.rm=TRUE)
  
  i <- i + 1
  
  # compute population / area per class
  for (class in classes) {
    df_list[[i]] <- terra::zonal(
      terra::mask(pop,
                  classification,
                  inverse = TRUE,
                  maskvalues = class,
                  updatevalue = updatevalue
      ),
      units_grid,
      fun = func,
      na.rm=TRUE
    )
    
    i <- i + 1
  }
  
  df <- Reduce(function(x, y) dplyr::inner_join(x, y, by=id), df_list)
  
  # construct name for total population / area column
  tot_name <- paste("Tot", name, sep = "_")
  
  # assign column names
  names(df) <- c(id, tot_name, paste(names(classes), name, sep = "_"))
  
  # compute share of population / area
  for (class in names(classes)) {
    df[[paste(class, "share", sep = "_")]] <- df[[paste(class, name, sep = "_")]] / df[[tot_name]]
  }
  
  return(df)
}


#' Helper function to compute the share of population/land in flexurba grid classes, used in the alternative workflow when classifying spatial units
#'
#' @description
#' The function reconstructs the spatial units classification of the Degree of Urbanisation based on the grid cell classification (without exceptions).

#' @param gridoverlap result of function `exactextractr::exact_extract` when overlapping the grids with the spatial units
#' @param population logical. Whether to compute the share of population (TRUE), or the share of area (FALSE)
#' @param level1 logical. Whether the classification is according to level 1 (TRUE) or level 2 (FALSE)
#' @param values vector with the values assigned to the different classes in the resulting units classification:
#'    - If `level1=TRUE`: the vector should contain the values for (1) cities, (2) town and semi-dense areas and (3) rural areas.
#'    - If `level1=FALSE`: the vector should contain the values for (1) cities, (2) dense towns, (3) semi-dense towns, (4) suburb or peri-urban areas, (5) villages, (6) dispersed rural areas and (7) mostly uninhabited areas.
#' @return dataframe with for each spatial unit the classification and the share of population per grid class
#' @noRd
compute_share_in_classes2 <- function(gridoverlap, population = TRUE, level1 = TRUE, values = NULL) {
  
  # global variables
  pop <- coverage_fraction <- class <- NULL
  
  # get classes with values
  classes <- get_class_names(level1, values)
  
  # get the column name of the id
  id <- names(gridoverlap)[1]
  
  # global variable
  .data <- NULL
  
  # group gridoverlap by unit id
  df <- gridoverlap %>%
    dplyr::group_by(.data[[id]])
  
  # create a list to save the dataframes
  df_list <- list()
  i <- 1
  
  # calculate the total population or area
  if (population) {
    name <- "Pop"
    df_list[[i]] <- df %>%
      dplyr::summarize(!!paste("Tot", name, sep = "_") := sum(pop * coverage_fraction, na.rm = TRUE))
  } else {
    name <- "Area"
    df_list[[i]] <- df %>%
      dplyr::summarize(!!paste("Tot", name, sep = "_") := sum(coverage_fraction, na.rm = TRUE))
  }
  
  i <- i + 1
  
  
  # calculate the proportion population per flexurba class
  for (j in seq_along(classes)) {
    if (population) {
      df_list[[i]] <- df %>% dplyr::summarize(!!paste(names(classes)[j], name, sep = "_") := sum(
        pop[class %in% classes[[j]]] *
          coverage_fraction[class %in% classes[[j]]],
        na.rm = TRUE
      ))
    } else {
      df_list[[i]] <- df %>% dplyr::summarize(!!paste(names(classes)[j], name, sep = "_") := sum(coverage_fraction[class %in% classes[[j]]],
                                                                                                 na.rm = TRUE
      ))
    }
    
    i <- i + 1
  }
  
  df <- Reduce(function(x, y) dplyr::inner_join(x, y, by=id), df_list)
  
  # compute share of population / area
  for (class in names(classes)) {
    df[[paste(class, "share", sep = "_")]] <- df[[paste(class, name, sep = "_")]] / df[[paste("Tot", name, sep = "_")]]
  }
  
  return(df)
}


#' Helper function to get the class names of grid classification and the corresponding values
#'
#' @description
#' The function returns a named list with the classes of the grid classification (and some aggregated classes) and the corresponding values

#' @param level1 logical. Whether the classification is according to level 1 (TRUE) or level 2 (FALSE)
#' @param values vector with the values assigned to the different classes in the resulting units classification:
#'    - If `level1=TRUE`: the vector should contain the values for (1) urban centres, (2) urban clusters and (3) rural grid cells.
#'    - If `level1=FALSE`: the vector should contain the values for (1) urban centres, (2) dense urban clusters, (3) semi-dense urban clusters, (4) suburban or peri-urban cells, (5) rural clusters, (6) low density rural cells, (7) very low density rural cells.
#' @return named list
#' @noRd
get_class_names <- function(level1 = TRUE, values = NULL) {
  if (level1) {
    if (is.null(values)) {
      values <- c(3, 2, 1)
    }
    if (length(values) != 3) {
      stop("Invalid argument: values should have length 3 representing the value of urban centres, urban clusters and rural cells respectively.")
    }
    return(list(
      "UCentre" = values[1],
      "UCluster" = values[2],
      "Urban" = values[1:2],
      "Rural" = values[3]
    ))
  } else {
    if (is.null(values)) {
      values <- c(30, 23, 22, 21, 13, 12, 11)
    }
    if (length(values) != 7) {
      stop("Invalid argument: values should have length 7 representing the value of urban centres, dense urban clusters, semi-dense urban clusters, suburban and peri-urban cells, rural clusters, low density rural cells and very low density rural cells respectively.")
    }
    return(list(
      "UCentre" = values[1],
      "UCluster" = values[2:4],
      "Urban" = values[1:4],
      "Rural" = values[5:7],
      "DUC" = values[2],
      "SDUC" = values[3],
      "SUrb" = values[4],
      "RC" = values[5],
      "LDR" = values[6],
      "VLDR" = values[7]
    ))
  }
}

#' Helper function to apply the unit classification rules
#'
#' @param df dataframe with for each spatial unit the classification and the share of population per grid class
#' @param level1 logical. Whether to classify the spatial units according to first hierarchical level (`TRUE`) or the second hierarchical level (`FALSE`). For more details, see section "Classification rules" below.
#' @param rules_from_2021 logical. Whether to employ the original classification rules as described in the 2021 version of the DEGURBA manual. The DEUGURBA Level 2 unit classification rules have been modified in July 2024. By default, the function uses the most recent rules as described in the [online version](https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Applying_the_degree_of_urbanisation_manual) of the methodological manual. For more details, see section "Modification of the unit classification rules" below.
#' @param values vector with the values assigned to the different classes in the resulting units classification:
#'    - If `level1=TRUE`: the vector should contain the values for (1) cities, (2) town and semi-dense areas and (3) rural areas.
#'    - If `level1=FALSE`: the vector should contain the values for (1) cities, (2) dense towns, (3) semi-dense towns, (4) suburb or peri-urban areas, (5) villages, (6) dispersed rural areas and (7) mostly uninhabited areas.
#' @return dataframe with the unit classification
#' @noRd
apply_unit_classification_rules <- function(df, level1 = TRUE, values = c(3, 2, 1), rules_from_2021 = FALSE) {
  if (level1) {
    return(df %>% dplyr::mutate(
      flexurba_L1 = factor(
        dplyr::case_when(
          UCentre_share >= 0.5 ~ values[1],
          UCentre_share < 0.5 &
            Rural_share <= 0.5 ~ values[2],
          Rural_share > 0.5 ~ values[3]
        ),
        levels = values
      )
    ))
  } else {
    df <- apply_unit_classification_rules(df, level1 = TRUE)
    
    if (!rules_from_2021){
      return(df %>% dplyr::mutate(
        flexurba_L2 = factor(
          dplyr::case_when(
            flexurba_L1 == 3 ~ values[1],
            (flexurba_L1 == 2) & ((UCentre_share + DUC_share) >= 0.5) ~ values[2],
            (flexurba_L1 == 2) & ((UCentre_share + DUC_share) < 0.5) & ((SDUC_share + Rural_share) < 0.5) ~ values[3],
            (flexurba_L1 == 2) & ((SDUC_share + Rural_share) >= 0.5) ~ values[4],
            (flexurba_L1 == 1) & (RC_share + UCentre_share + UCluster_share >= 0.5) ~ values[5],
            (flexurba_L1 == 1) & (RC_share + UCentre_share + UCluster_share < 0.5) & (VLDR_share < 0.5)  ~ values[6],
            (flexurba_L1 == 1) & (VLDR_share >= 0.5) ~ values[7]
          ),
          levels = values
        )
      ))
    } else {
      return(df %>% dplyr::mutate(
        flexurba_L2 = factor(
          dplyr::case_when(
            flexurba_L1 == 3 ~ values[1],
            (flexurba_L1 == 2) & (DUC_share >= SDUC_share) & ((DUC_share + SDUC_share) > SUrb_share) ~ values[2],
            (flexurba_L1 == 2) & (DUC_share < SDUC_share) & ((DUC_share + SDUC_share) > SUrb_share) ~ values[3],
            (flexurba_L1 == 2) & ((DUC_share + SDUC_share) <= SUrb_share) ~ values[4],
            (flexurba_L1 == 1) & (RC_share > LDR_share) & (RC_share > VLDR_share) ~ values[5],
            (flexurba_L1 == 1) & (LDR_share > RC_share) & (LDR_share > VLDR_share) ~ values[6],
            (flexurba_L1 == 1) & (VLDR_share > RC_share) & (VLDR_share > LDR_share) ~ values[7]
          ),
          levels = values
        )
      ))
    }
  }
}

#' Create the DEGURBA spatial units classification
#' 
#' @description 
#' `r lifecycle::badge("deprecated")`
#' 
#' `classify_units()` has been renamed to `DoU_classify_units()` to create a more consistent API and to better indicate that this function is specifically designed to classify units in the context of the DEGURBA classification`. 
#' @param data named list with the required data, as returned by the function [DoU_preprocess_units()]
#' @param id character. Unique column in the `units` data as id for spatial units
#' @param level1 logical. Whether to classify the spatial units according to first hierarchical level (`TRUE`) or the second hierarchical level (`FALSE`). For more details, see section "Classification rules" below.
#' @param values vector with the values assigned to the different classes in the resulting units classification:
#'    - If `level1=TRUE`: the vector should contain the values for (1) cities, (2) town and semi-dense areas and (3) rural areas.
#'    - If `level1=FALSE`: the vector should contain the values for (1) cities, (2) dense towns, (3) semi-dense towns, (4) suburb or peri-urban areas, (5) villages, (6) dispersed rural areas and (7) mostly uninhabited areas.
#' @param official_workflow logical. Whether to employ the official workflow of the GHSL (`TRUE`) or the alternative workflow (`FALSE`). For more details, see section "Workflow" below.
#' @param filename character. Output filename (csv). The resulting classification together with a metadata file (in JSON format) will be saved if `filename` is not `NULL`.
#' @return dataframe with for each spatial unit the classification and the share of population per grid class
#' @keywords internal
#' @export
classify_units <- function(data, id = "UID",
                           level1 = TRUE,
                           values = NULL,
                           official_workflow = TRUE,
                           filename = NULL){
  return(DoU_classify_units(data, id,
                            level1,
                            values,
                            official_workflow,
                            rules_from_2021 = TRUE,
                            filename))
}
