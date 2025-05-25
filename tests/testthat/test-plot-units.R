test_that("DoU_plot_units() works", {
  gridclassification <- DoU_classify_grid(system.file(
    "extdata",
    "belgium",
    package = "flexurba"
  ))
  pop <- system.file("extdata", "belgium", "POP.tif", package = "flexurba")
  data <- flexurba::DoU_preprocess_units(
    flexurba::units_belgium,
    gridclassification,
    pop
  )
  classification <- flexurba::DoU_classify_units(data)

  expect_no_error(DoU_plot_units(flexurba::units_belgium, classification))

  expect_no_error(DoU_plot_units(
    flexurba::units_belgium,
    classification,
    extent = sf::st_bbox(
        flexurba::units_belgium %>% dplyr::filter(NAME_4 == "Brugge")
    ),
    column='flexurba_L1',
    title='TEST',
    scalebar=TRUE
    )
  )

  expect_no_error(DoU_plot_units(
    flexurba::units_belgium,
    classification,
    extent = terra::ext(234786.7, 247479.5, 5993814.8, 6015500.3)
  ))

  expect_error(DoU_plot_units(
    flexurba::units_belgium,
    classification,
    extent = "ERROR"
  ))

  expect_error(DoU_plot_units(
    flexurba::units_belgium,
    classification,
    palette = c("3" = "#e16c72", "2" = "#fac66c", "1" = "#97c197"),
    labels = c("UC", "UCL")
  ))

  expect_error(DoU_plot_units(
    flexurba::units_belgium,
    classification,
    level1 = FALSE
  ))
})
