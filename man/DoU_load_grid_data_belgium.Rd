% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DoU-load-grid-data-belgium.R
\name{DoU_load_grid_data_belgium}
\alias{DoU_load_grid_data_belgium}
\title{Load the grid data for Belgium to reconstruct DEGURBA classification}
\usage{
DoU_load_grid_data_belgium()
}
\value{
named list with the data of Belgium required for the grid classification of the Degree of Urbanisation.
}
\description{
The function loads the data required to execute the grid classification of the Degree of Urbanisation for Belgium with the function \code{\link[=DoU_preprocess_grid]{DoU_preprocess_grid()}}.

The data was constructed with the code below:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{# download the GHSL data on a global scale
download_GHSLdata(output_directory = "inst/extdata/global")

# crop the global grid to Belgium
crop_GHSLdata(extent = terra::ext(192000, 485000, 5821000, 6030000),
              global_directory = "inst/extdata/global",
              output_directory = "inst/extdata/belgium")
}\if{html}{\out{</div>}}
}
\examples{
DoU_load_grid_data_belgium()
}
