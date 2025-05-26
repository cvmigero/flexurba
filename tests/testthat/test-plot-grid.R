test_that("DoU_plot_grid() works", {
  classification <- DoU_classify_grid(system.file(
    "extdata",
    "belgium",
    package = "flexurba"
  ))
  
  expect_no_error(DoU_plot_grid(classification))

  expect_no_error(DoU_plot_grid(
    classification,
    extent = terra::ext(191547.7, 265536.5, 5946882.3, 6015989.7),
    title='TEST',
    scalebar=TRUE
  ))

  expect_error(DoU_plot_grid(
    classification,
    palette = c(
      "3" = "#e16c72",
      "2" = "#fac66c",
      "1" = "#97c197",
      "0" = "#acd3df"
    ),
    labels = c("UC", "UCL", "RUR")
  ))

  expect_warning(DoU_plot_grid(classification, level1 = FALSE))

  expect_no_error(DoU_plot_grid(classification))
})
