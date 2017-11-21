context("Merge Multiple Data Frames")

library(dplyr)
x1 <- data_frame(id = rep(1:5, 3), x = rnorm(15))
x2 <- data_frame(id = 1:15, id2 = rep(c("A", "B", "C"), 5), x = rnorm(15))
x3 <- data_frame(id = 1:15, id2 = rep(c("A", "B", "C"), 5), x = runif(15))

y1 <- data_frame(id = 1:5, x = rnorm(5))
y2 <- data_frame(id = 3:7, x = rnorm(5))
y3 <- data_frame(id = 7:11, x = rnorm(5))

merge_mult_to_tibble <- function(...){
  as_data_frame(merge_mult(...))
}

test_that("merge_mult simple merges on 2+ data frames", {
  test1 <- merge_mult_to_tibble(x1, x2, f = merge, by = "id", suffixes = 1:2)
  test2 <- merge_mult_to_tibble(list(x1, x2), f = merge, by = "id", suffixes = 1:2)
  actual <- dplyr::inner_join(x1, x2, by = "id", suffix = paste0("_", 1:2))
  expect_equal(test1, actual)
  expect_equal(test2, actual)
  
  test <- merge_mult_to_tibble(x1, x2, x3, f = merge, by = "id", suffixes = letters[1:3])
  actual <- dplyr::inner_join(x1, x2, by = "id", suffix = c('_a', '')) %>% dplyr::inner_join(x3, by = "id", suffix = c("_b", "_c"))
  expect_equal(test, actual)
  
  test <- merge_mult_to_tibble(x1, x2, x3, f = dplyr::inner_join, by = c("id", "x"), suffixes = letters[1:3])
  actual <- data_frame(id = integer(1), x = 0.5, id2_b = "A", id2_c = "B") %>% head(0)
  expect_equal(test, actual)
})

test_that("merge_mult complex merges on 3+ data frames varying by", {
  test <- merge_mult_to_tibble(x1, x2, x3, f = merge, by = list("id", c("id", "id2")), suffixes = letters[1:3])
  actual <- dplyr::inner_join(x1, x2, by = "id", suffix = c('_a', '')) %>% dplyr::inner_join(x3, by = c("id", "id2"), suffix = c("_b", "_c"))
  expect_equal(test, actual)
})

test_that("merge_mult merges varying merge functions", {
  test_me <- function(f){
    s <- list(c("_a", ""), c("_b", "_c"))
    by <- "id"
    test <- merge_mult_to_tibble(y1, y2, y3, f = f, by = by, suffixes = letters[1:3])
    actual <- f(y1, y2, by = by, suffix = s[[1]]) %>% f(y3, by = by, suffix = s[[2]])
    expect_equal(test, actual)
  }
  test_me(dplyr::inner_join)
  test_me(dplyr::full_join)
  test_me(dplyr::left_join)
  test_me(dplyr::right_join)
  
  test_me_no_suffix <- function(f){
    by <- "id"
    test <- merge_mult_to_tibble(y1, y2, y3, f = f, by = by)
    actual <- f(y1, y2, by = by) %>% f(y3, by = by)
    expect_equal(test, actual)
  }
  test_me_no_suffix(dplyr::semi_join)
  test_me_no_suffix(dplyr::anti_join)
})

test_that("merge_mult handles empty data frames", {
  test <- merge_mult_to_tibble(y1, y2, y3, f = dplyr::inner_join, by = c("id", "x"), suffixes = letters[1:3])
  actual <- head(y1, 0)
  expect_equal(test, actual)
  
  test <- merge_mult_to_tibble(y1, head(y1, 0), f = dplyr::inner_join, by = "id", suffixes = letters[1:2])
  actual <- data_frame(id = integer(1), x_a = 0.5, x_b = 0.5) %>% head(0)
  expect_equal(test, actual)
    
  test <- merge_mult_to_tibble(y1, head(y1, 0), f = dplyr::full_join, by = "id", suffixes = letters[1:2])
  actual <- dplyr::full_join(y1, head(y1, 0), by = "id", suffix = c("_a", "_b")) 
  expect_equal(test, actual)
})

test_that("merge_mult requires at least 2 data frames or a list of 2 or more data frames", {
  f <- dplyr::inner_join
  by <- "id"
  suffixes = "a"
  
  expect_error(merge_mult_to_tibble(f = f, by = by, suffixes = suffixes))
  expect_error(merge_mult_to_tibble(x1, f = f, by = by, suffixes = suffixes))
  expect_error(merge_mult_to_tibble(list(x1), f = f, by = by, suffixes = suffixes))
  expect_error(merge_mult_to_tibble(list(x1), list(x2), f = f, by = by, suffixes = suffixes))
  expect_error(merge_mult_to_tibble(list(x1, matrix(rnorm(9), nrow = 3)), f = f, by = by, suffixes = suffixes))
})

test_that("merge_mult requires a valid merging function", {
  l <- list(x1, x2, x3)
  by <- "id"
  suffixes <- letters[1:3]
  
  expect_error(merge_mult_to_tibble(l, by = by, suffixes = suffixes)) 
  expect_error(merge_mult_to_tibble(l, f = 1, by = by, suffixes = suffixes)) 
  expect_error(merge_mult_to_tibble(l, f = function(){}, by = by, suffixes = suffixes)) 
})
  
test_that("merge_mult requires a valid by columns", {
  l <- list(x1, x2, x3)
  f <- dplyr::inner_join
  by <- "id"
  suffixes <- letters[1:3]
  
  expect_error(merge_mult_to_tibble(l, f = 1, suffixes = suffixes)) # missing
  expect_error(merge_mult_to_tibble(l, f = f, by = 1:2, suffixes = suffixes)) # type
  expect_error(merge_mult_to_tibble(l, f = f, by = data_frame(), suffixes = suffixes)) # type
  expect_error(merge_mult_to_tibble(l, f = f, by = list("id", c("id", "id2"), "id"), suffixes = suffixes)) # length
  expect_error(merge_mult_to_tibble(l, f = f, by = "a", suffixes = suffixes)) # columns not available
})

test_that("merge_mult requires a valid suffix argument", {
  l <- list(x1, x2, x3)
  f <- dplyr::inner_join
  by <- "id"

  expect_error(merge_mult_to_tibble(l, f = f, by = by))
  expect_error(merge_mult_to_tibble(l, f = f, by = by, suffixes = c(TRUE, FALSE, FALSE))) 
  expect_error(merge_mult_to_tibble(l, f = f, by = by, suffixes = 1:100)) 
})
