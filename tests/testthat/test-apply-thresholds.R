proxies <- load_proxies_belgium()

# CONVERT regions TO GRID
units <- flexurba::units_belgium
expect_equal(length(unique(terra::values(convert_regions_to_grid(units, proxies$pop)))), 582)
expect_equal(length(unique(terra::values(convert_regions_to_grid(units, proxies$pop, 'GID_2')))), 12) 
expect_error(convert_regions_to_grid('wrongpath.geojson', proxies$pop))
expect_error(convert_regions_to_grid(units, proxies$pop, 'wrongid'))



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
regions <- convert_regions_to_grid(flexurba::units_belgium, proxies$pop, 'GID_1')
withregions1 <- apply_threshold(proxies$pop, type='predefined', threshold_value=1500, regions=regions)
expect_equal(sum(terra::values(withregions1$rboundaries), na.rm=TRUE), 2051)
expect_equal(withregions1$threshold$threshold_value, c(1500, 1500, 1500))

withregions2 <- apply_threshold(proxies$pop, type='predefined', threshold_value=c(1500, 1200, 1000), regions=regions)
expect_equal(sum(terra::values(withregions2$rboundaries), na.rm=TRUE), 2925)
expect_equal(withregions2$threshold$threshold_value, c(1500, 1200, 1000))


# DATA-DRIVEN - RELATIVE
withregions3 <- apply_threshold(proxies$pop, type='data-driven', fun='p95', regions=flexurba::units_belgium)
expect_equal(sum(terra::values(withregions3$rboundaries), na.rm=TRUE), 1797)

withregions4 <- apply_threshold(proxies$pop, type='data-driven', fun='p95', regions=system.file("extdata", "belgium", 'westflanders_units.gpkg', package = "flexurba"))
expect_equal(sum(terra::values(withregions4$rboundaries), na.rm=TRUE), 188)

province_regions <- convert_regions_to_grid(flexurba::units_belgium, proxies$pop, 'GID_2')
withregions5 <- apply_threshold(proxies$pop, type='data-driven', fun='p95', regions=province_regions)
expect_equal(sum(terra::values(withregions5$rboundaries), na.rm=TRUE), 1683)



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

