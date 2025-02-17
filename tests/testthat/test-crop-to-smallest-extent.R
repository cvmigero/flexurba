test_that("crop_to_smallest_extent() works", {
  expect_equal(
    as.vector(terra::ext(crop_to_smallest_extent(
      terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3)),
      terra::rast(nrows = 4, ncols = 4, ext = c(0, 4, 0, 4))
    )[[2]])),
    as.vector(terra::ext(0, 3, 0, 3))
  )
  expect_equal(
    as.vector(terra::ext(crop_to_smallest_extent(
      terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3)),
      terra::rast(nrows = 4, ncols = 4, ext = c(0, 4, 0, 4))
    )[[1]])),
    as.vector(terra::ext(0, 3, 0, 3))
  )
})

test_that("DoU_classify_grid() works", {
  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UC_built_threshold = 0.5,
        UC_smooth_edge_fun = "majority_rule_R2022A"
      )
    )),
    terra::values(terra::rast(system.file("extdata", "belgium", "classification1.tif", package = "flexurba")))
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UC_density_threshold = 1000,
        UC_size_threshold = 40000,
        UC_built_criterium = FALSE,
        UC_gap_fill = FALSE,
        UC_built_threshold = 0.5,
        UC_smooth_edge = FALSE,
        UC_smooth_pop = TRUE,
        UC_smooth_pop_window = 3
      )
    )),
    terra::values(terra::rast(system.file("extdata", "belgium", "classification2.tif", package = "flexurba")))
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "PRD", package = "flexurba"),
      parameters = list(
        UCL_density_threshold = 400,
        UCL_size_threshold = 7000,
        UC_built_threshold = 0.5,
        UC_contiguity_rule = 8,
        UC_smooth_edge_fun = "majority_rule_R2022A"
      )
    )),
    terra::values(terra::rast(system.file("extdata", "PRD", "classification3.tif", package = "flexurba")))
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "PRD", package = "flexurba"),
      parameters = list(
        UC_density_threshold = 2000,
        UCL_size_threshold = 80000,
        UC_contiguity_rule = 8,
        UC_gap_fill = FALSE,
        UC_smooth_edge = FALSE,
        UC_built_threshold = 0.5
      )
    )),
    terra::values(terra::rast(system.file("extdata", "PRD", "classification4.tif", package = "flexurba")))
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "PRD", package = "flexurba"),
      parameters = list(
        UC_density_threshold = 1250,
        UCL_size_threshold = 60000,
        UC_gap_fill = FALSE,
        UC_smooth_edge_fun = "majority_rule_R2023A",
        UC_built_threshold = 0.5
      ),
      values = c(30, 20, 10, 0)
    )),
    terra::values(terra::rast(system.file("extdata", "PRD", "classification5.tif", package = "flexurba")))
  )

  expect_error(DoU_classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    parameters = list(UC_density_threshold = "error")
  ))

  expect_error(DoU_classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    parameters = list(UC_gap_fill = 5)
  ))

  expect_error(DoU_classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    parameters = list(UCL_contiguity_rule = 5)
  ))
})
