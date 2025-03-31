#' Division of GHSL tiles in 9 regions
#'
#' @description
#' The Global Human Settlement Layer (GHSL) divides the world into different rectangular tiles. To execute the Degree of Urbanisation in a memory-efficient manner, we grouped these tiles into 9 different regions. These regions are the smallest possible grouping of GHSL tiles, ensuring that no continuous land mass is split across two regions. By splitting the world into different parts, the RAM required to execute the Degree of Urbanisation is optimised. For a concrete example on how to use the regions to construct the grid classification on a global scale, see `vignette("vig3-DoU-global-scale")`. 
#'
#' The 9 regions cover approximately the following areas:
#' - **W_AEA:** Asia - Europe - Africa - Oceania (eastern hemisphere)
#' - **W_AME:** North and South America (+ Greenland and Iceland)
#' - **W_ISL1:** Hawaii
#' - **W_ISL2:** Oceanic Islands (western hemisphere)
#' - **W_ISL3:** Chatham Islands
#' - **W_ISL4:** Scott Island
#' - **W_ISL5:** Saint-Helena, Ascension and Tristan da Cunha
#' - **W_ISL6:** French Southern and Antarctic Lands
#' - **W_ANT:** Antarctica
#'
#' \figure{figure_GHSL_tiles_per_region.png}{GHSL tiles}
#'
#' For more information about the GHSL tiles and their extent see [GHSL Download page](https://ghsl.jrc.ec.europa.eu/download.php).
#'
#' @format ## `GHSL_tiles_per_region`
#' A named list of length 9:
#' - the names represent the 9 different regions (`W_AEA`, `W_AME`, `W_ISL1`, `W_ISL2`, `W_ISL3`, `W_ISL4`, `W_ISL5`, `W_ISL6`, `W_ANT`)
#' - the elements are vectors with the GHSL tile ids that make up the regions
#' @source <https://ghsl.jrc.ec.europa.eu/download/GHSL_data_54009_shapefile.zip>
"GHSL_tiles_per_region"