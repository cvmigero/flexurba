randomi <- c(14873, 19879, 44156, 44649, 11081, 33299, 2891, 6168, 27915, 47788, 36022, 36158, 33640, 47865, 62974, 2040, 7974, 44823, 66183, 63057, 12289, 27462, 23013, 2148, 30621, 948, 26150, 36813, 1340, 20439, 4238, 46025, 49123, 46819, 47402, 14129, 22984, 4325, 63191, 34252, 52215, 30000, 14992, 4848, 43788, 62939, 23006, 52372, 64814, 51888)

test_that("DoU_classify_grid() works", {
  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      level1 = FALSE
    ))[randomi],
    c(11, 12, 11, 11, 11, 11, 12, 11, 30, 11, 21, 21, 23, 11, 13, 30, 12, 12, 11, 11, 21, 12, 23, 10, 12, 10, 11, 21, 11, 30, 12, 11, 11, 12, 12, 12, 11, 10, 11, 11, 12, 12, 21, 12, 11, 11, 11, 12, 21, 11)
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        LDR_density_threshold = 70,
        UC_smooth_edge = FALSE,
        DUC_size_threshold = 6000,
        SDUC_density_threshold = 300,
        SDUC_size_threshold = 5000,
        SDUC_contiguity_rule = 8,
        SDUC_buffer_size = 3
      ), level1 = FALSE
    ))[randomi],
    c(11, 12, 11, 11, 11, 11, 12, 11, 21, 11, 22, 21, 23, 11, 13, 30, 12, 11, 11, 11, 21, 12, 23, 10, 12, 10, 11, 21, 11, 30, 12, 11, 11, 12, 12, 12, 11, 10, 11, 11, 12, 11, 21, 12, 11, 11, 11, 12, 21, 11)
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UC_size_threshold = 60000,
        UC_max_gap = 3,
        DUC_density_threshold = 1000,
        SDUC_density_threshold = 300,
        SDUC_size_threshold = 5000,
        SDUC_contiguity_rule = 4,
        SUrb_contiguity_rule = 4
      ), level1 = FALSE
    ))[randomi],
    c(11, 12, 11, 11, 11, 11, 12, 11, 30, 11, 22, 21, 23, 11, 13, 30, 12, 12, 11, 11, 21, 12, 23, 10, 12, 10, 11, 23, 11, 30, 12, 11, 11, 12, 12, 12, 11, 10, 11, 11, 12, 12, 21, 12, 11, 11, 11, 12, 23, 11)
  )


  expect_error(DoU_classify_grid(
    data = system.file("extdata", "belgium", package = "flexurba"),
    parameters = list(SDUC_buffer_size = "error"), level1 = FALSE
  ))

  expect_error(DoU_classify_grid(
    data = system.file("extdata", "belgium", package = "flexurba"),
    paramters = list(UC_gap_fill = 5), level1 = FALSE
  ))

  expect_error(DoU_classify_grid(
    data = system.file("extdata", "belgium", package = "flexurba"),
    values = 5, level1 = FALSE
  ))
})

test_that("DoU_classify_grid() works prt2", {
  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UC_built_threshold = 0.5,
        UC_smooth_edge_fun = "majority_rule_R2022A"
      )
    ))[randomi],
    c(1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 2, 1, 2, 0, 1, 0, 1, 2, 1, 3, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1)
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
    ))[randomi],
    c(1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 2, 1, 2, 0, 1, 0, 1, 3, 1, 3, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1)
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UCL_density_threshold = 400,
        UCL_size_threshold = 7000,
        UC_built_threshold = 0.5,
        UC_contiguity_rule = 8,
        UC_smooth_edge_fun = "majority_rule_R2022A"
      )
    ))[randomi],
    c(1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1, 1, 2, 1, 2, 0, 1, 0, 1, 3, 1, 3, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1)
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UC_density_threshold = 2000,
        UCL_size_threshold = 80000,
        UC_contiguity_rule = 8,
        UC_gap_fill = FALSE,
        UC_smooth_edge = FALSE,
        UC_built_threshold = 0.5
      )
    ))[randomi],
    c(1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 3, 1, 1, 1, 1, 2, 1, 2, 0, 1, 0, 1, 2, 1, 3, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1)
  )

  expect_equal(
    terra::values(DoU_classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UC_density_threshold = 1250,
        UCL_size_threshold = 60000,
        UC_gap_fill = FALSE,
        UC_smooth_edge_fun = "majority_rule_R2023A",
        UC_built_threshold = 0.5
      ),
      values = c(30, 20, 10, 0)
    ))[randomi],
    c(10, 10, 10, 10, 10, 10, 10, 10, 30, 10, 10, 20, 20, 10, 10, 30, 10, 10, 10, 10, 20, 10, 20, 0, 10, 0, 10, 30, 10, 30, 10, 10, 10, 10, 10, 10, 10, 0, 10, 10, 10, 10, 20, 10, 10, 10, 10, 10, 20, 10)
  )

  expect_error(DoU_classify_grid(
    data = system.file("extdata", "belgium", package = "flexurba"),
    parameters = list(UC_density_threshold = "error")
  ))

  expect_error(DoU_classify_grid(
    data = system.file("extdata", "belgium", package = "flexurba"),
    parameters = list(UC_gap_fill = 5)
  ))

  expect_error(DoU_classify_grid(
    data = system.file("extdata", "belgium", package = "flexurba"),
    parameters = list(UCL_contiguity_rule = 5)
  ))


  # depricated
  expect_equal(
    terra::values(classify_grid(
      data = system.file("extdata", "belgium", package = "flexurba"),
      parameters = list(
        UC_density_threshold = 1250,
        UCL_size_threshold = 60000,
        UC_gap_fill = FALSE,
        UC_smooth_edge_fun = "majority_rule_R2023A",
        UC_built_threshold = 0.5
      ),
      values = c(30, 20, 10, 0)
    ))[randomi],
    c(10, 10, 10, 10, 10, 10, 10, 10, 30, 10, 10, 20, 20, 10, 10, 30, 10, 10, 10, 10, 20, 10, 20, 0, 10, 0, 10, 30, 10, 30, 10, 10, 10, 10, 10, 10, 10, 0, 10, 10, 10, 10, 20, 10, 10, 10, 10, 10, 20, 10)
  )
})
