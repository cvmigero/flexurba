test_that("DoU_classify_units() works", {
  # LEVEL1
  # load data
  units <- system.file("extdata", "belgium", "westflanders_units.gpkg", package = "flexurba")
  classification <- system.file("extdata", "belgium", "classification1.tif", package = "flexurba")
  pop <- system.file("extdata", "belgium", "westflanders_pop_100m.tif", package = "flexurba")

  # get units classification
  data <- flexurba::DoU_preprocess_units(units, classification, pop)
  units_classification <- flexurba::DoU_classify_units(data)

  expect_equal(as.vector(units_classification$flexurba_L1), c("2", "2", "3", "1", "2", "2", "1", "2", "2", "1", "1", "1", "2", "2", "1", "1", "2", "1", "1", "2", "1", "2", "1", "2", "2", "2", "3", "3", "3", "2", "2", "1", "2", "2", "2", "3", "1", "2", "1", "2", "3", "2", "1", "2", "2", "2", "2", "1", "2", "1", "1", "2", "2", "2", "1", "1", "2", "2", "1", "1", "2", "2", "2", "2"))

  # units classification by dissolving municipalities to arrondissements
  data2 <- flexurba::DoU_preprocess_units(units, classification, pop, dissolve_units_by = "GID_3")
  units_classification2 <- flexurba::DoU_classify_units(data2, id = "GID_3")

  expect_equal(as.vector(units_classification2$flexurba_L1), c("2", "1", "1", "2", "2", "2", "2", "2"))


  # units should be in Mollweide projection
  expect_error(flexurba::DoU_preprocess_units(sf::st_transform(units, 4326), classification, pop))

  # resolution of grids should be multiple of resample resolution
  expect_error(flexurba::DoU_preprocess_units(units, classification, pop, resample_resolution = 58))

  # invalid dissolve_units_by parameter
  expect_error(flexurba::DoU_preprocess_units(units, classification, pop, dissolve_units_by = "ERROR"))

  # invalid values
  expect_error(flexurba::DoU_classify_units(data, values = "ERROR"))
  expect_error(flexurba::DoU_classify_units(data, values = c(9, 8, 7)))
  expect_error(flexurba::DoU_classify_units(data, values = c(1)))

  # LEVEL 2
  # load data
  units <- sf::st_read(system.file("extdata", "belgium", "westflanders_units.gpkg", package = "flexurba"), quiet = TRUE)
  classification <- terra::rast(system.file("extdata", "belgium", "classification6.tif", package = "flexurba"))
  pop <- terra::rast(system.file("extdata", "belgium", "westflanders_pop_100m.tif", package = "flexurba"))

  # get units classification
  data <- flexurba::DoU_preprocess_units(units, classification, pop)
  units_classification <- flexurba::DoU_classify_units(data, level1 = FALSE, rules_from_2021 = TRUE)

  expect_equal(as.vector(units_classification$flexurba_L2), c("22", "23", "30", "12", "21", "23", "12", "23", "22", "12", "12", "13", "22", "22", "12", "12", "23", "12", "13", "23", "12", "23", "13", "21", "22", "21", "30", "30", "30", "21", "23", "12", "21", "21", "23", "30", "13", "22", "13", "21", "30", "21", "13", "21", "23", "21", "23", "13", "30", "12", "13", "21", "23", "21", "13", "13", "23", "21", "12", "12", "23", "21", "21", "23"))
  
  units_classification_updated <- flexurba::DoU_classify_units(data, level1 = FALSE)
  
  expect_equal(as.vector(units_classification_updated$flexurba_L2), c("21", "23", "30", "12", "22", "23", "13", "22", "21", "12", "12", "13", "21", "21", "12", "12", "22", "12", "13", "22", "12", "23", "12", "22", "21", "22", "30", "30", "30", "22", "23", "12", "22", "22", "22", "30", "13", "21", "13", "22", "30", "22", "13", "22", "23", "21", "23", "13", "30", "13", "13", "22", "23", "22", "13", "13", "22", "22", "13", "12", "23", "22", "22", "23"))

  # units classification by dissolving municipalities to arrondissements
  data2 <- flexurba::DoU_preprocess_units(units, classification, pop, dissolve_units_by = "GID_3")
  units_classification2 <- flexurba::DoU_classify_units(data2, id = "GID_3", level1 = FALSE)

  expect_equal(as.vector(units_classification2$flexurba_L2), c("23", "13", "13", "23", "30", "22", "21", "22"))
  
  # deprecated functions
  # units classification by dissolving municipalities to arrondissements
  data2 <- flexurba::preprocess_units(units, classification, pop, dissolve_units_by = "GID_3")
  units_classification2 <- flexurba::classify_units(data2, id = "GID_3", level1 = FALSE)
  
  expect_equal(as.vector(units_classification2$flexurba_L2), c("23", "12", "12", "21", "30", "23", "21", "21"))
})
