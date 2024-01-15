test_that("plot_grid() works", {
  expect_no_error(plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba")))
  
  expect_no_error(plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba"), extent = terra::ext(191547.7, 265536.5, 5946882.3, 6015989.7)))
  
  expect_error(plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba"), palette = c("3" = "#e16c72", "2" = "#fac66c", "1" = "#97c197", "0" = "#acd3df"), labels = c("UC", "UCL", "RUR")))
  
  expect_warning(plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba"), level1 = FALSE))
})