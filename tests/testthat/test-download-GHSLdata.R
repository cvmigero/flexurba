test_that("download_GHSLdata() works", {
  expect_error(
    download_GHSLdata(
      products = c('POP'),
      output_directory=tempdir(),
      epoch = 2022,
      filenames = c('POP.tif')
    ))
  
  expect_error(
    download_GHSLdata(
      products = c('ONLY ONE'),
      output_directory=tempdir()
    ))
  
  expect_error(
    download_GHSLdata(
      products = c('POP', 'BUILT', 'WRONG'),
      output_directory=tempdir()
    ))
})