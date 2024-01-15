test_that("get_patches() functions works", {
  expect_equal(
    terra::values(get_patches(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, 1, NA, 2, NA, NA, 3, 2)
    ), directions = 4)),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, 1, NA, 2, NA, NA, 2, 2)
    ))
  )
  
  expect_equal(
    terra::values(get_patches(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, 1, NA, 2, NA, NA, 3, 2)
    ), directions = 8)),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, 1, NA, 1, NA, NA, 1, 1)
    ))
  )
  
  expect_equal(
    terra::values(get_patches(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, 1, NA, 2, NA, NA, 3, 2)
    ), directions = 4, cells = c(2))),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, NA, NA, 1, NA, NA, NA, 2)
    ))
  )
})