context("Make Outliers NA")

x <- rnorm(1000)

test_that("make_outliers_na handles missing data", {
  expect_error(make_outliers_na())
  expect_error(make_outliers_na(x = 1:10))
  expect_error(make_outliers_na(multiplier = 4))
})

test_that("make_outliers_na handles invalid inputs", {
  expect_error(make_outliers_na(x = 1:10, 'a'))
  expect_error(make_outliers_na(x = 1:10, list(1)))
  expect_error(make_outliers_na(x = list(1:10), 1))
  expect_error(make_outliers_na(x = letters, 1))
  
  expect_error(make_outliers_na(1, 1))
  expect_error(make_outliers_na(1:10, -4))
})

test_that("make_outliers_na converts values to na", {
  expect_false(any(is.na(x)))
  expect_true(any(is.na(make_outliers_na(x, 2))))
})

test_that("make_outliers_na converts appropriate values to na", {
  one_sd <- make_outliers_na(x, 1)
  expect_equal( mean(is.na(one_sd)), 1 - 0.68, tolerance = 0.015)
  
  two_sd <- make_outliers_na(x, 2)
  expect_equal( mean(is.na(two_sd)), 1 - 0.95, tolerance = 0.015)
  
  three_sd <- make_outliers_na(x, 3)
  expect_equal( mean(is.na(three_sd)), 1 - 0.997, tolerance = 0.015)
})
