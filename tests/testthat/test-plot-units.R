test_that("DoU_plot_units() works", {
  units <- sf::st_read(system.file("extdata", "belgium", "westflanders_units.gpkg", package = "flexurba"), quiet = TRUE)
  classification <- read.csv(system.file("extdata", "belgium", "units_classification_westflanders.csv", package = "flexurba"))

  expect_no_error(DoU_plot_units(units, classification))

  expect_no_error(DoU_plot_units(units, classification, extent = sf::st_bbox(units %>% dplyr::filter(NAME_4 == "Brugge"))))

  expect_no_error(DoU_plot_units(units, classification, extent = terra::ext(234786.7, 247479.5, 5993814.8, 6015500.3)))

  expect_error(DoU_plot_units(units, classification, extent = "ERROR"))

  expect_error(DoU_plot_units(units, classification, palette = c("3" = "#e16c72", "2" = "#fac66c", "1" = "#97c197"), labels = c("UC", "UCL")))

  expect_error(DoU_plot_units(units, classification, level1 = FALSE))
})
