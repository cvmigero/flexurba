test_that("DoU_get_optimal_builtup() works", {
  expect_equal(
    round(
      DoU_get_optimal_builtup(system.file(
        "extdata",
        "belgium",
        package = "flexurba"
      )),
      7
    ),
    0.2434054
  )

  expect_equal(
    round(
      DoU_get_optimal_builtup(
        system.file("extdata", "belgium", package = "flexurba"),
        density_threshold = 1000,
        size_threshold = 10000,
        directions = 8
      ),
      7
    ),
    0.2223592
  )

  # deprecated function
  expect_equal(
    round(
      get_optimal_builtup(
        system.file("extdata", "belgium", package = "flexurba"),
        density_threshold = 1000,
        size_threshold = 10000,
        directions = 8
      ),
      7
    ),
    0.2223592
  )

  expect_error(DoU_get_optimal_builtup(
    system.file("extdata", "belgium", package = "flexurba"),
    directions = 5
  ))
})
