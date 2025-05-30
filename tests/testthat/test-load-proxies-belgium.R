test_that("load_proxies_belgium() works", {
  proxies <- load_proxies_belgium()
  expect_in(names(proxies), c("pop", "built", "light"))
  expect_equal(round(mean(terra::values(proxies$pop), na.rm = TRUE)), 323)
  expect_equal(round(mean(terra::values(proxies$built), na.rm = TRUE) * 100), 4)
  expect_equal(round(mean(terra::values(proxies$light), na.rm = TRUE)), 4)
})
