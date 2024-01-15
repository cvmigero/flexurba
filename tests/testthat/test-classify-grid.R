test_that("classify_grid() works", {
  expect_equal(
    terra::values(classify_grid(data = system.file("extdata", "belgium", package = "flexurba"), level1 = FALSE)),
    terra::values(terra::rast(system.file("extdata", "belgium", "classification6.tif", package = "flexurba")))
  )

  expect_equal(
    terra::values(classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        LDR_density_threshold = 70,
        UC_smooth_edge = FALSE,
        DUC_size_threshold = 6000,
        SDUC_buffer_size = 4
      ), level1 = FALSE
    )),
    terra::values(terra::rast(system.file("extdata", "belgium", "classification7.tif", package = "flexurba")))
  )

  expect_equal(
    terra::values(classify_grid(
      data = system.file("extdata", "PRD", package = "flexurba"),
      parameters = list(
        UC_size_threshold = 60000,
        UC_max_gap = 3,
        DUC_density_threshold = 1000,
        SDUC_contiguity_rule = 4,
        SUrb_contiguity_rule = 4
      ), level1 = FALSE
    )),
    terra::values(terra::rast(system.file("extdata", "PRD", "classification8.tif", package = "flexurba")))
  )


  expect_error(classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    parameters = list(SDUC_buffer_size = "error"), level1 = FALSE
  ))

  expect_error(classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    paramters = list(UC_gap_fill = 5), level1 = FALSE
  ))

  expect_error(classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    values = 5, level1 = FALSE
  ))
})

test_that("classify_grid() works prt2", {
  expect_equal(
    terra::values(classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UC_built_threshold = 0.5,
        UC_smooth_edge_fun = "majority_rule_R2022A"
      )
    )),
    terra::values(terra::rast(system.file("extdata", "belgium", "classification1.tif", package = "flexurba")))
  )
  
  expect_equal(
    terra::values(classify_grid(
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
    terra::values(classify_grid(
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
    terra::values(classify_grid(
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
    terra::values(classify_grid(
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
  
  expect_error(classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    parameters = list(UC_density_threshold = "error")
  ))
  
  expect_error(classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    parameters = list(UC_gap_fill = 5)
  ))
  
  expect_error(classify_grid(
    data = system.file("extdata", "PRD", package = "flexurba"),
    parameters = list(UCL_contiguity_rule = 5)
  ))
})