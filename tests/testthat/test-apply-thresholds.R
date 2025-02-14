proxies <- load_proxies_belgium()

# CONVERT ZONES TO GRID
units <- flexurba::units_belgium
expect_equal(length(unique(terra::values(convert_zones_to_grid(units, proxies$pop)))), 582)
expect_equal(length(unique(terra::values(convert_zones_to_grid(units, proxies$pop, 'GID_2')))), 12) 
expect_error(convert_zones_to_grid('wrongpath.geojson', proxies$pop))
expect_error(convert_zones_to_grid(units, proxies$pop, 'wrongid'))



# PREDEFINED - ABSOLUTE
pop_above_1500 <- apply_threshold(proxies$pop, type='predefined', threshold_value=1500)
expect_equal(sum(terra::values(pop_above_1500$rboundaries), na.rm=TRUE), 3926)

built_above_20 <- apply_threshold(proxies$built, type='predefined', threshold_value=0.20)
expect_equal(sum(terra::values(built_above_20$rboundaries), na.rm=TRUE), 3435)

light_above_30 <- apply_threshold(proxies$light, type='predefined', threshold_value=30)
expect_equal(sum(terra::values(light_above_30$rboundaries), na.rm=TRUE), 1079)



# DATA-DRIVEN - ABSOLUTE
pop_above_p95 <- apply_threshold(proxies$pop, type='data-driven', fun='p95')
expect_equal(sum(terra::values(pop_above_p95$rboundaries), na.rm=TRUE), 3662)

built_above_mean <- apply_threshold(proxies$built, type='data-driven', fun='mean')
expect_equal(sum(terra::values(built_above_mean$rboundaries), na.rm=TRUE), 26134)

light_above_p75 <- apply_threshold(proxies$light, type='data-driven', fun='p75')
expect_equal(sum(terra::values(light_above_p75$rboundaries), na.rm=TRUE), 18326)


# PREDEFINED - RELATIVE
regions_zones <- convert_zones_to_grid(flexurba::units_belgium, proxies$pop, 'GID_1')
withzones1 <- apply_threshold(proxies$pop, type='predefined', threshold_value=1500, zones=regions_zones)
expect_equal(sum(terra::values(withzones1$rboundaries), na.rm=TRUE), 2051)
expect_equal(withzones1$threshold$threshold_value, c(1500, 1500, 1500))

withzones2 <- apply_threshold(proxies$pop, type='predefined', threshold_value=c(1500, 1200, 1000), zones=regions_zones)
expect_equal(sum(terra::values(withzones2$rboundaries), na.rm=TRUE), 2925)
expect_equal(withzones2$threshold$threshold_value, c(1500, 1200, 1000))


# DATA-DRIVEN - RELATIVE
withzones3 <- apply_threshold(proxies$pop, type='data-driven', fun='p95', zones=flexurba::units_belgium)
expect_equal(sum(terra::values(withzones3$rboundaries), na.rm=TRUE), 1797)

withzones4 <- apply_threshold(proxies$pop, type='data-driven', fun='p95', zones=system.file("extdata", "belgium", 'westflanders_units.gpkg', package = "flexurba"))
expect_equal(sum(terra::values(withzones4$rboundaries), na.rm=TRUE), 188)

province_zones <- convert_zones_to_grid(flexurba::units_belgium, proxies$pop, 'GID_2')
withzones5 <- apply_threshold(proxies$pop, type='data-driven', fun='p95', zones=province_zones)
expect_equal(sum(terra::values(withzones5$rboundaries), na.rm=TRUE), 1683)



# INCORRECT ARGUMENTS
expect_error(apply_threshold(proxies$pop, type='data-driven', threshold_value = 1500))
expect_error(apply_threshold(proxies$pop, type='predefined', fun = 'max'))
expect_warning(apply_threshold(proxies$pop, type='data-driven', fun='mean', threshold_value = 1500))
expect_warning(apply_threshold(proxies$pop, type='predefined', threshold_value=1500, fun = 'max'))

expect_error(apply_threshold(proxies$pop, type='data-driven', fun='p101'))
expect_error(apply_threshold(proxies$pop, type='data-driven', fun='iets'))
expect_error(apply_threshold(proxies$pop, type='data-driven', fun='p0.5'))
expect_error(apply_threshold(proxies$pop, type='data-driven', fun=terra::mask))


# WARNING WHEN MORE THAN ONE LAYER
terra::add(proxies$pop) <- proxies$built
expect_warning(apply_threshold(proxies$pop, type='predefined', threshold_value=3))

