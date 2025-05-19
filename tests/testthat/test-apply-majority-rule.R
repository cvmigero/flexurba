test_that("apply_majority_rule works", {
  expect_equal(
    terra::values(apply_majority_rule(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 1, 1, 1, NA, NA, 1, 1, 1)
    )), version = "R2022A"),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 1, 1, 1, 1, 1, 1, 1, 1)
    ))
  )

  expect_equal(
    terra::values(apply_majority_rule(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 1, 1, 1, NA, 2, 1, 1, 1)
    ), version = "R2022A")),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 1, 1, 1, 1, 2, 1, 1, 1)
    ))
  )

  expect_equal(
    terra::values(apply_majority_rule(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 1, 2, 1, NA, NA, 2, 1, NA)
    ), version = "R2022A")),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 1, 2, 1, NA, NA, 2, 1, NA)
    ))
  )

  # no effect of permanent_water, land, pop
  x <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 1, 1, 1, NA, NA, 1, 1, 1))
  permanent_water <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(NA, NA, NA, NA, NA, NA, NA, NA, NA))
  land <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 1, 1, 1, 1, 1, 1, 1, 1))
  pop <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 1, 1, 1, 1, 1, 1, 1, 1))


  expect_equal(
    terra::values(apply_majority_rule(x, "R2023A", permanent_water, land, pop)),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 1, 1, 1, 1, 1, 1, 1, 1)
    ))
  )

  # effect of permanent_water
  x <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 1, 1, 1, NA, NA, 1, 1, 1))
  permanent_water <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(NA, NA, NA, NA, NA, 0, NA, NA, NA))
  land <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 1, 1, 1, 1, 1, 1, 1, 1))
  pop <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 1, 1, 1, 1, 1, 1, 1, 1))


  expect_equal(
    terra::values(apply_majority_rule(x, "R2023A", permanent_water, land, pop)),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 1, 1, 1, 1, NA, 1, 1, 1)
    ))
  )

  # effect of land and pop
  x <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(NA, NA, NA, NA, NA, NA, 1, 1, 1))
  permanent_water <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(NA, NA, NA, NA, NA, NA, NA, NA, NA))
  land <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 0.3, 0.3, 1, 1, 0.3, 1, 1, 1))
  pop <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 0, 0, 1, 1, 1, 1, 0, 1))


  expect_equal(
    terra::values(apply_majority_rule(x, "R2023A", permanent_water, land, pop)),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, NA, NA, NA, NA, 1, 1, 1)
    ))
  )

  x <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(NA, NA, NA, NA, NA, NA, 1, 1, 1))
  permanent_water <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(NA, NA, NA, NA, NA, NA, NA, NA, NA))
  land <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 0.3, 0.3, 1, 1, 0.3, 1, 1, 1))
  pop <- terra::rast(nrows = 3, ncols = 3, ext = c(0, 3, 0, 3), vals = c(1, 0, 0, 1, 1, 0, 1, 1, 1))


  expect_equal(
    terra::values(apply_majority_rule(x, "R2023A", permanent_water, land, pop)),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, NA, NA, 1, NA, 1, 1, 1)
    ))
  )

  # invalid version
  expect_error(apply_majority_rule(x, "ERROR"))

  # invalid permanent_land
  expect_error(apply_majority_rule(x, "R2023A", permanent_water = NULL, land = land, pop = pop))
})
