test_that("fill_gaps() functions works", {
  expect_equal(
    terra::values(fill_gaps(terra::rast(
      nrows = 3,
      ncols = 3,
      ext = c(0, 3, 0, 3),
      crs = "epsg:25831",
      vals = c(1, 1, 1, 1, NA, 1, 1, 1, 1)
    ))),
    terra::values(terra::rast(
      nrows = 3,
      ncols = 3,
      ext = c(0, 3, 0, 3),
      crs = "epsg:25831",
      vals = c(1, 1, 1, 1, 1, 1, 1, 1, 1)
    ))
  )

  expect_equal(
    terra::values(fill_gaps(terra::rast(
      nrows = 3,
      ncols = 3,
      ext = c(0, 3, 0, 3),
      crs = "epsg:25831",
      vals = c(1, 1, 1, 1, 2, 1, 1, 1, 1)
    ))),
    terra::values(terra::rast(
      nrows = 3,
      ncols = 3,
      ext = c(0, 3, 0, 3),
      crs = "epsg:25831",
      vals = c(1, 1, 1, 1, 2, 1, 1, 1, 1)
    ))
  )

  expect_equal(
    terra::values(fill_gaps(terra::rast(
      nrows = 3,
      ncols = 3,
      ext = c(0, 3, 0, 3),
      crs = "epsg:25831",
      vals = c(1, 1, 2, 1, NA, 2, 1, 1, 2)
    ))),
    terra::values(terra::rast(
      nrows = 3,
      ncols = 3,
      ext = c(0, 3, 0, 3),
      crs = "epsg:25831",
      vals = c(1, 1, 2, 1, NA, 2, 1, 1, 2)
    ))
  )
})
