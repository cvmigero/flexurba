test_that("is_GHSLtile() works", {
  expect_equal(is_GHSLtile("R1_C18"), TRUE)
  expect_equal(is_GHSLtile("R0_C18"), FALSE)
})
