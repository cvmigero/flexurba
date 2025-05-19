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