context("To Be or Not To Be")

library(dplyr)
d <- data_frame(
  x = rep(1:3, each = 4),
  y = rep(1:4, each = 3),
  z = rep(1:2, 6), 
  id = 1:12
)
empty_df <- head(d, 0)

test_that("to_be splits data frames", {
  evens <- purrr::keep(1:nrow(d), ~ .x %% 2 == 0)
  odds <- purrr::discard(1:nrow(d), ~ .x %% 2 == 0)
  slice_evens <- dplyr::slice(d, evens)
  slice_odds <- dplyr::slice(d, odds)
  to_be_slice <- to_be(d, dplyr::slice, evens)
  expect_equal(to_be_slice$to_be, slice_evens)
  expect_equal(to_be_slice$not_to_be, slice_odds)
  
  eq <- subset(d, x == y)
  neq <- subset(d, x != y)
  to_be_subset <- to_be(d, subset, x == y)
  expect_equal(to_be_subset$to_be, eq)
  expect_equal(to_be_subset$not_to_be, neq)
  
  cols <- c("x", "y")
  xy <- dplyr::select(d, dplyr::one_of(cols))
  z <- dplyr::select(d, -dplyr::one_of(cols))
  to_be_select <- to_be(d, dplyr::select, dplyr::one_of(cols))
  expect_equal(to_be_select$to_be, xy)
  expect_equal(to_be_select$not_to_be, z)
  
  n <- 5
  hd <- head(d, n)
  tl <- tail(d, nrow(d) - n)
  to_be_head <- to_be(d, head, n)
  expect_equal(to_be_head$to_be, hd)
  expect_equal(to_be_head$not_to_be, tl)
  
  dups <- view_duplicated(d, x, y)
  no_dups <- remove_duplicated(d, x, y, opt_delete = "all")
  to_be_dups <- to_be(d, view_duplicated, x, y)
  expect_equal(to_be_dups$to_be, dups)
  expect_equal(to_be_dups$not_to_be, no_dups)
  
  n <- 3
  set.seed(1)
  random <- dplyr::sample_n(d, n)
  not_in_random <- subset(d, !(id %in% random$id))
  set.seed(1)
  to_be_random <- to_be(d, dplyr::sample_n, n)
  expect_equal(to_be_random$to_be, random)
  expect_equal(to_be_random$not_to_be, not_in_random)
})

test_that("to_be empty data frames", {
  to_be_empty_rows <- to_be(d, subset, x != 100)
  expect_equal(to_be_empty_rows$to_be, d)
  expect_equal(to_be_empty_rows$not_to_be, empty_df)
  
  empty_cols <- dplyr::select(d, dplyr::one_of("a"))
  to_be_empty_cols <- to_be(d, dplyr::select, dplyr::one_of("a"))
  expect_equal(to_be_empty_cols$to_be, empty_cols)
  expect_equal(to_be_empty_cols$not_to_be, d)
  
  to_be_empty <- to_be(empty_df, subset, x == 1)
  expect_equal(to_be_empty$to_be, empty_df)
  expect_equal(to_be_empty$not_to_be, empty_df)
})

test_that("to_be requires a data frame", {
  expect_error(to_be())
  expect_error(to_be(1:10))
  expect_error(to_be(matrix(1:10, nrow = 5)))
})

test_that("to_be requires a function that returns a data frame", {
  expect_error(to_be(d, 1:10))
  expect_error(to_be(d, TRUE))
  expect_error(to_be(d, d))
  
  wrong_function <- function(d) "hi"
  expect_error(to_be(d, wrong_function))
  
  expect_error(to_be(d, dplyr::select, list()))
})

test_that("to_be requires a function that works on only one dimension", {
  expect_error(to_be(d, subset, subset = x == 1, select = id))
})
