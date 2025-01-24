proxies <- load_proxies_belgium()

# tests for absolute thresholds
pop_above_1500 <- apply_absolute_threshold(proxies$pop, 1500)
expect_equal(sum(values(pop_above_1500$rboundaries), na.rm=TRUE), 3926)
built_above_20 <- apply_absolute_threshold(proxies$built, 0.20)
expect_equal(sum(values(built_above_20$rboundaries), na.rm=TRUE), 3435)
light_above_30 <- apply_absolute_threshold(proxies$light, 30)
expect_equal(sum(values(light_above_30$rboundaries), na.rm=TRUE), 1079)

# tests for convert_zones_to_grid
units <- flexurba::units_belgium
expect_equal(length(unique(terra::values(convert_zones_to_grid(units, proxies$pop)))), 582)
expect_equal(length(unique(terra::values(convert_zones_to_grid(units, proxies$pop, 'GID_2')))), 12) 
expect_error(convert_zones_to_grid('wrongpath.geojson', proxies$pop))
expect_error(convert_zones_to_grid(units, proxies$pop, 'wrongid'))

# tests for relative thresholds
pop_above_p95 <- apply_relative_threshold(proxies$pop, 'p95')
expect_equal(sum(values(pop_above_p95$rboundaries), na.rm=TRUE), 3662)
built_above_mean <- apply_relative_threshold(proxies$built, 'mean')
expect_equal(sum(values(built_above_mean$rboundaries), na.rm=TRUE), 26134)
light_above_p75 <- apply_relative_threshold(proxies$light, 'p75')
expect_equal(sum(values(light_above_p75$rboundaries), na.rm=TRUE), 18326)

# zones can be specified by sf object, path to data, or SpatRaster
withzones1 <- apply_relative_threshold(proxies$pop, 'p95', zones=flexurba::units_belgium)
expect_equal(sum(values(withzones1$rboundaries), na.rm=TRUE), 1797)
withzones2 <- apply_relative_threshold(proxies$pop, 'p95', zones=system.file("extdata", "belgium", 'westflanders_units.gpkg', package = "flexurba"))
expect_equal(sum(values(withzones2$rboundaries), na.rm=TRUE), 188)
province_zones <- convert_zones_to_grid(flexurba::units_belgium, proxies$pop, 'GID_2')
withzones3 <- apply_relative_threshold(proxies$pop, 'p95', zones=province_zones)
expect_equal(sum(values(withzones3$rboundaries), na.rm=TRUE), 1683)

# error when function is not correctly specified
expect_error(apply_relative_threshold(proxies$pop, 'p101'))
expect_error(apply_relative_threshold(proxies$pop, 'iets'))
expect_error(apply_relative_threshold(proxies$pop, 'p0.5'))
expect_error(apply_relative_threshold(proxies$pop, terra::mask))

# expect warning when grid has more than one layer
terra::add(proxies$pop) <- proxies$built
expect_warning(apply_absolute_threshold(proxies$pop, 3))