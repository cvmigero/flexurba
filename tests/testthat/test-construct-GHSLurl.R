test_that("construct_GHSLurl() works", {
  expect_equal(
    construct_GHSLurl(
      "LAND",
      epoch = 2018,
      resolution = 100,
      tile_id = "R10_C12",
      release = "R2022A"
    ),
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_LAND_GLOBE_R2022A/GHS_LAND_E2018_GLOBE_R2022A_54009_100/V1-0/tiles/GHS_LAND_E2018_GLOBE_R2022A_54009_100_V1_0_R10_C12.zip"
  )
  expect_equal(
    construct_GHSLurl(
      "BUILT_S",
      epoch = 2000,
      resolution = 1000,
      release = "R2022A"
    ),
    "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2022A/GHS_BUILT_S_GLOBE_R2022A/GHS_BUILT_S_E2000_GLOBE_R2022A_54009_1000/V1-0/GHS_BUILT_S_E2000_GLOBE_R2022A_54009_1000_V1_0.zip"
  )
})
