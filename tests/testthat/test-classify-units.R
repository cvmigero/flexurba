test_that("DoU_classify_units() works", {
  # LEVEL1
  # load data
  classification <- DoU_classify_grid(system.file(
    "extdata",
    "belgium",
    package = "flexurba"
  ))
  pop <- system.file("extdata", "belgium", "POP.tif", package = "flexurba")

  # get units classification
  data <- flexurba::DoU_preprocess_units(
    flexurba::units_belgium,
    classification,
    pop
  )
  units_classification <- flexurba::DoU_classify_units(data)

  expect_equal(
    as.vector(units_classification$flexurba_L1)[20:30],
    c("3", "2", "3", "2", "2", "3", "1", "2", "2", "3", "2")
  )
  expect_equal(
    as.vector(units_classification$flexurba_L1)[100:110],
    c("2", "2", "3", "2", "2", "2", "2", "1", "2", "2", "2")
  )

  # units classification by dissolving municipalities to arrondissements
  data2 <- flexurba::DoU_preprocess_units(
    flexurba::units_belgium,
    classification,
    pop,
    dissolve_units_by = "GID_3"
  )
  units_classification2 <- flexurba::DoU_classify_units(data2, id = "GID_3")

  expect_equal(
    as.vector(units_classification2$flexurba_L1)[20:30],
    c("2", "2", "1", "3", "2", "2", "1", "2", "2", "2", "3")
  )

  # units should be in Mollweide projection
  expect_error(flexurba::DoU_preprocess_units(
    sf::st_transform(units, 4326),
    classification,
    pop
  ))

  # resolution of grids should be multiple of resample resolution
  expect_error(flexurba::DoU_preprocess_units(
    units,
    classification,
    pop,
    resample_resolution = 58
  ))

  # invalid dissolve_units_by parameter
  expect_error(flexurba::DoU_preprocess_units(
    units,
    classification,
    pop,
    dissolve_units_by = "ERROR"
  ))

  # invalid values
  expect_error(flexurba::DoU_classify_units(data, values = "ERROR"))
  expect_error(flexurba::DoU_classify_units(data, values = c(9, 8, 7)))
  expect_error(flexurba::DoU_classify_units(data, values = c(1)))

  # LEVEL 2
  # get units classification
  classification <- DoU_classify_grid(
    system.file("extdata", "belgium", package = "flexurba"),
    level1 = FALSE
  )
  data <- flexurba::DoU_preprocess_units(
    flexurba::units_belgium,
    classification,
    pop
  )
  units_classification <- flexurba::DoU_classify_units(
    data,
    level1 = FALSE,
    rules_from_2021 = TRUE
  )

  expect_equal(
    as.vector(units_classification$flexurba_L2)[20:30],
    c("30", "21", "30", "23", "21", "30", "12", "21", "21", "30", "22")
  )
  expect_equal(
    as.vector(units_classification$flexurba_L2)[100:110],
    c("23", "22", "30", "21", "22", "21", "22", "12", "21", "21", "23")
  )

  # units classification by dissolving municipalities to arrondissements
  data2 <- flexurba::DoU_preprocess_units(
    flexurba::units_belgium,
    classification,
    pop,
    dissolve_units_by = "GID_3"
  )
  units_classification2 <- flexurba::DoU_classify_units(
    data2,
    id = "GID_3",
    level1 = FALSE
  )

  expect_equal(
    as.vector(units_classification2$flexurba_L2)[20:30],
    c("21", "23", "13", "30", "22", "22", "13", "22", "23", "22", "30")
  )

  # deprecated functions
  # units classification by dissolving municipalities to arrondissements
  data2 <- flexurba::preprocess_units(
    flexurba::units_belgium,
    classification,
    pop,
    dissolve_units_by = "GID_3"
  )
  units_classification2 <- flexurba::classify_units(
    data2,
    id = "GID_3",
    level1 = FALSE
  )

  expect_equal(
    as.vector(units_classification2$flexurba_L2)[20:30],
    c("23", "21", "12", "30", "21", "23", "12", "23", "21", "21", "30")
  )
})
