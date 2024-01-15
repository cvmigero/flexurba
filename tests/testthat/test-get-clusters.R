test_that("get_clusters() works", {
  # 1 density threshold
  expect_equal(
    terra::values(get_clusters(
      xden = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(1, 2, 3, 1, 2, 3, 2, 1, 0)
      ),
      minden = 2, minsiz = 9, directions = 4
    )),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, 1, 1, NA, 1, 1, NA, NA, NA)
    ))
  )
  
  expect_equal(
    terra::values(get_clusters(
      xden = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(1, 2, 3, 1, 2, 3, 2, 1, 0)
      ),
      minden = 2, minsiz = 15, directions = 8
    )),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, NA, NA, NA, NA, NA, NA, NA)
    ))
  )
  
  expect_equal(
    terra::values(get_clusters(
      xden = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(1, 2, 3, 1, 2, 3, 2, 1, 0)
      ),
      minden = 2, minsiz = 15, directions = 8,
      xsiz = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(0, 5, 5, 0, 5, 0, 5, 0, 0)
      )
    )),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, 1, 1, NA, 1, 1, 1, NA, NA)
    ))
  )
  
  # 2 density thresholds
  expect_equal(
    terra::values(get_clusters(
      xden = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(1, 2, 3, 1, 1, 3, 2, 1, 0)
      ),
      minden = 2,
      xden2 = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(0, 0, 0, 0, 1, 0, 0, 0, 0)
      ),
      minden2 = 1,
      minsiz = 9, directions = 4
    )),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, 1, 1, NA, 1, 1, NA, NA, NA)
    ))
  )
  
  expect_equal(
    terra::values(get_clusters(
      xden = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(1, 2, 3, 1, 1, 3, 2, 1, 0)
      ),
      minden = 2,
      xden2 = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(0, 0, 0, 0, 1, 0, 0, 0, 0)
      ),
      minden2 = 1, minsiz = 15, directions = 8
    )),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, NA, NA, NA, NA, NA, NA, NA, NA)
    ))
  )
  
  expect_equal(
    terra::values(get_clusters(
      xden = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(1, 2, 3, 1, 2, 3, 2, 1, 0)
      ),
      minden = 2,
      xden2 = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(0, 0, 0, 0, 1, 0, 0, 0, 0)
      ),
      minden2 = 1, minsiz = 15, directions = 8,
      xsiz = terra::rast(
        nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
        vals = c(0, 5, 5, 0, 5, 0, 5, 0, 0)
      )
    )),
    terra::values(terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(NA, 1, 1, NA, 1, 1, 1, NA, NA)
    ))
  )
  
  # extents don't match
  expect_error(terra::values(get_clusters(
    xden = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 2, 3, 1, 2, 3, 2, 1, 0)
    ),
    minden = 2,
    xden2 = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 4, 0, 4),
      vals = c(0, 0, 0, 0, 1, 0, 0, 0, 0)
    ),
    minden2 = 1, minsiz = 15, directions = 8,
    xsiz = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(0, 5, 5, 0, 5, 0, 5, 0, 0)
    )
  )))
  # invalid density threshold
  expect_error(terra::values(get_clusters(
    xden = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 2, 3, 1, 2, 3, 2, 1, 0)
    ),
    minden = "ERROR",
    xden2 = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 4, 0, 4),
      vals = c(0, 0, 0, 0, 1, 0, 0, 0, 0)
    ),
    minden2 = 1, minsiz = 15, directions = 8,
    xsiz = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(0, 5, 5, 0, 5, 0, 5, 0, 0)
    )
  )))
  
  # invalid directions
  expect_error(terra::values(get_clusters(
    xden = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(1, 2, 3, 1, 2, 3, 2, 1, 0)
    ),
    minden = "ERROR",
    xden2 = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 4, 0, 4),
      vals = c(0, 0, 0, 0, 1, 0, 0, 0, 0)
    ),
    minden2 = 1, minsiz = 15, directions = 3,
    xsiz = terra::rast(
      nrows = 3, ncols = 3, ext = c(0, 3, 0, 3),
      vals = c(0, 5, 5, 0, 5, 0, 5, 0, 0)
    )
  )))
})