test_that("DoU_plot_grid() works", {
  expect_no_error(DoU_plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba")))
  
  expect_no_error(DoU_plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba"), extent = terra::ext(191547.7, 265536.5, 5946882.3, 6015989.7)))
  
  expect_error(DoU_plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba"), palette = c("3" = "#e16c72", "2" = "#fac66c", "1" = "#97c197", "0" = "#acd3df"), labels = c("UC", "UCL", "RUR")))
  
  expect_warning(DoU_plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba"), level1 = FALSE))
  
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_no_error(DoU_plot_grid(system.file("extdata", "belgium", "classification1.tif", package = "flexurba")))
})
