grid <- terra::rast(system.file("extdata", "PRD", "POP.tif",
                                package = "flexurba"))

expect_error(apply_relative_threshold(grid, 'p101'))
expect_error(apply_relative_threshold(grid, 'iets'))
expect_error(apply_relative_threshold(grid, 'p0.5'))

terra::add(grid) <- grid
expect_warning(apply_absolute_threshold(grid, 3))

# test convert_zones_to_grid
data_belgium <- load_grid_data_belgium()
units <- flexurba::units_belgium
expect_equal(length(unique(terra::values(convert_zones_to_grid(units, data_belgium$pop)))), 582)
expect_equal(length(unique(terra::values(convert_zones_to_grid(units, data_belgium$pop, 'GID_2')))), 12) 
expect_error(convert_zones_to_grid('wrongpath.geojson', data_belgium$pop))
expect_error(convert_zones_to_grid(units, data_belgium$pop, 'wrongid'))
expect_warning(convert_zones_to_grid(units, grid))

