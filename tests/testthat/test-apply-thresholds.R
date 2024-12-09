grid <- terra::rast(system.file("extdata", "PRD", "POP.tif",
                                package = "flexurba"))

expect_error(apply_relative_threshold(grid, 'p101'))
expect_error(apply_relative_threshold(grid, 'iets'))
expect_error(apply_relative_threshold(grid, 'p0.5'))

add(grid) <- grid
expect_warning(apply_absolute_threshold(grid, 3))

               