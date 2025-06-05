test_that("crop_GHSLdata() works", {
  expect_error(
    crop_GHSLdata(
      extent=terra::ext(350000, 400000, 5950000, 6000000),
      global_directory='doesnotexist',
      output_directory=tempdir()
  ))
  
  expect_error(
    crop_GHSLdata(
      extent='wrong',
      global_directory=system.file("extdata", "belgium", package = "flexurba"),
      output_directory=tempdir()
    ))
  
  expect_error(
    crop_GHSLdata(
      extent=terra::ext(350000, 400000, 5950000, 6000000),
      global_directory=system.file("extdata", "belgium", package = "flexurba"),
      buffer='wrong',
      output_directory=tempdir()
    ))
  
  expect_error(
    crop_GHSLdata(
      extent=terra::ext(350000, 400000, 5950000, 6000000),
      global_directory=system.file("extdata", "belgium", package = "flexurba"),
      output_directory=tempdir(),
      output_filenames = c('POP', 'BUILT', 'LAND'),
      global_filenames = c('DOES NOT EXIST')
    ))
  
  expect_no_error(
    crop_GHSLdata(
      extent=terra::ext(350000, 400000, 5950000, 6000000),
      global_directory=system.file("extdata", "belgium", 
                                   package = "flexurba"),
      output_directory=tempdir()
    ))
  unlink(file.path(tempdir(), 'LAND.tif'))
  unlink(file.path(tempdir(), 'LAND.json'))
  unlink(file.path(tempdir(), 'POP.tif'))
  unlink(file.path(tempdir(), 'POP.json'))
  unlink(file.path(tempdir(), 'BUILT_S.tif'))
  unlink(file.path(tempdir(), 'BUILT_S.json'))
})