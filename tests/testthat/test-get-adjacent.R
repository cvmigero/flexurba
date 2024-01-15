test_that("get_adjacent() works", {
  expect_equal(
    terra::values(get_adjacent(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, 2, NA, NA, NA, NA, NA, NA)
    ))),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, 1, 1, NA, 1, 1, NA, NA, NA)
    ))
  )
  
  expect_equal(
    terra::values(get_adjacent(
      terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(NA, NA, NA, NA, 2, NA, NA, NA, NA)
      ),
      include = FALSE, adjacent_value = 3, directions = 4
    )),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, 3, NA, 3, NA, 3, NA, 3, NA)
    ))
  )
})