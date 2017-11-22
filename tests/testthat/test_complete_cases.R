context("Complete Cases by Col")

library(dplyr)
d <- data.frame(
  y = c(rep(NA, 2), 1:5, NA),
  x1 = c(1:2, rep(NA, 2), rep(0, 4)),
  x2 = 1:8,
  x3 = c(1:5, NA, 7:8)
)

test_that("complete_cases removes na on specified columns", {
  actual <- dplyr::slice(d, c(1:2, 5, 7:8))
  expect_equal(complete_cases(d, x1, x2, x3) %>% as_data_frame(), actual)
  expect_equal(complete_cases(d, starts_with("x")) %>% as_data_frame(), actual)
})

test_that("complete_cases on all columns", {
  actual <- subset(d, complete.cases(d))
  expect_equal(complete_cases(d), actual)
})

test_that("complete_cases when there are no NA", {
  clean <- subset(d, complete.cases(d))
  expect_equal(complete_cases(clean), clean)
})

test_that("complete_cases when there are all NA", {
  d2 <- d
  d2$x3 <- NA
  expect_equal(complete_cases(d2), head(d2, 0))
})

test_that("complete_cases handles empty data", {
  expect_equal(complete_cases(head(d, 0)), head(d, 0))
})

test_that("complete_cases handles missing and invalid arguments", {
  expect_error(complete_cases(1:10))
  expect_error(complete_cases(list(1,2,3)))
})

test_that("complete_cases only accepts columns within data frame", {
  expect_error(view_duplicated(d, q, v))
  expect_error(view_duplicated(d, list()))
  expect_error(view_duplicated(d, ends_with("\\dx")))
})

