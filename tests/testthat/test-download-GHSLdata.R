test_that("download_GHSLdata() works", {
  expect_no_error(
    download_GHSLdata(
      output_directory=tempdir(),
      products = c("POP"),
      epoch = 1975,
      crs = 54009,
      resolution = 1000,
      extent = "R1_C13",
      filenames = paste0(paste0(sample(letters, 8, 
                                       replace = TRUE), collapse = ""), '.tif')
    ))
  
  expect_no_error(
    download_GHSLdata(
      output_directory=file.path(tempdir(), 'test'),
      products = c("POP", 'BUILT_S'),
      epoch = 1975,
      crs = 54009,
      resolution = 1000,
      extent = c("R1_C13", 'R1_C14'),
      filenames = paste0(replicate(2, paste0(sample(letters, 8, replace = TRUE), collapse = "")), '.tif')
  ))
  
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
  
  expect_warning(
    download_GHSLdata(
      products = c('POP'),
      output_directory=tempdir(),
      extent = c("R1_C11", 'R1_C13'),
      filenames = paste0(paste0(sample(letters, 8, 
                                       replace = TRUE), collapse = ""), '.tif')
    ))
  
  expect_warning(
    download_GHSLdata(
      products = c('LAND'),
      release = 'R2022A',
      output_directory=tempdir(),
      extent = c('R1_C13'),
      filenames = paste0(paste0(sample(letters, 8, 
                                       replace = TRUE), collapse = ""), '.tif')
    ))
  
  expect_warning(
    download_GHSLdata(
      products = c('LAND'),
      epoch = '2018',
      output_directory=tempdir(),
      extent = c('R1_C13'),
      filenames = paste0(paste0(sample(letters, 8, 
                                       replace = TRUE), collapse = ""), '.tif')
    ))
})