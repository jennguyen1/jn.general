context("Working with Lists")

library(purrr)
library(dplyr)

test_that("is_iterative_list requires a list", {
  expect_error(is_iterative_list(1:10))
  expect_error(is_iterative_list(iris))
})

test_that("is_iterative_list requires a list of length 2 or more", {
  l <- rerun(1, x = 1, y = 2)
  expect_error(is_iterative_list(l))
  
  l <- rerun(2, x = 1, y = 2)
  expect_true(is_iterative_list(l))
})

test_that("is_iterative_list detects lists with consistent names", {
  l <- rerun(5, x = list(b = rnorm(15), c = c(1, 2)), y1 = runif(12), i = if(sample(c(TRUE, FALSE), 1)) head(iris) else NULL)
  expect_true(is_iterative_list(l))

  names(l) <- paste0("l", 1:length(l))
  expect_true(is_iterative_list(l))
})

test_that("is_iterative_list detects lists with varying names", {
  l <- rerun(5, x = list(b = rnorm(15), c = c(1, 2)), y1 = runif(12), i = if(sample(c(TRUE, FALSE), 1)) head(iris) else NULL)
  l[[3]]$sneak <- 1
  expect_false(is_iterative_list(l))
  
  l[[1]]$sneak <- 1
  expect_false(is_iterative_list(l))
})

test_that("is_iterative_list detects lists with no names", {
  l <- rerun(5, x = list(a = runif(1), b = rnorm(15), c = list(1, head(iris))), matrix(1:6, nrow = 2))
  expect_false(is_iterative_list(l))
})

test_that("rename_list two configurations both return same object", {
  l <- purrr::rerun(5, x = list(a = head(mtcars), b = head(iris), c = list(o = 1, p = list(q = head(airquality), u = 1:10))), y = head(mtcars))
  t1 <- rename_list(l, names = paste0("id", 1:5))
  names(l) <- paste0("id", 1:5)
  t2 <- rename_list(l)
  expect_equal(t1, t2)
})

test_that("rename_list applies correct name to each element", {
  l <- purrr::rerun(5, x = list(a = head(airquality), b = head(iris), c = list(o = 1, p = list(q = head(airquality), u = 1:10))), y = head(iris))
  names(l) <- paste0("id", 1:5)
  test <- rename_list(l)
  actual <- purrr::map2(l, 1:length(l), function(e, i){
    name <- paste0("id", i)
    e$x$a$name_id <- name
    e$x$b$name_id <- name
    e$x$c$p$q$name_id <- name
    e$y$name_id <- name
    e$x$a <- select(e$x$a, name_id, everything())
    e$x$b <- select(e$x$b, name_id, everything())
    e$x$c$p$q <- select(e$x$c$p$q, name_id, everything())
    e$y <- select(e$y, name_id, everything())
    e
  })
  names(actual) <- names(l)
  expect_equal(test, actual)
})

test_that("rename_list takes in a named list or list and a vector of names", {
  expect_error(rename_list())
  expect_error(rename_list(head(iris)))
  
  l <- rerun(3, x = head(iris), y = list(a = head(iris), b = 1:10))
  expect_error(rename_list(l))
  expect_error(rename_list(l, "not a good name"))
  expect_error(rename_list(l, c("a", "b", "b")))
  expect_error(rename_list(l, c("a", NA, NA)))
  expect_error(rename_list(l, c("a", "b", "c", "d")))
  names(l) <- letters[1:2]
  expect_error(rename_list(l))
  names(l) <- c("a", "b", "b")
  expect_error(rename_list(l))
})

test_that("extract_list takes in an iterative list and a set of booleans", {
  l <- rerun(5, x = list(b = rnorm(15), c = c(1, 2)), y1 = runif(12), i = if(sample(c(TRUE, FALSE), 1)) head(iris) else NULL)
  expect_error(extract_list())
  expect_error(extract_list(l, 1))
  expect_error(extract_list(l, TRUE, list()))
  
  l[[4]]$error <- ":("
  expect_error(extract_list(l))
})

test_that("extract_list takes in an iterative list and a set of booleans", {
  l <- rerun(3, x = list(b = rnorm(15), c = c(1, 2)), i = if(sample(c(TRUE, FALSE), 1)) head(iris) else NULL)
  test <- extract_list(l)
  actual <- list(
    x = list(
      b = list(l[[1]]$x$b, l[[2]]$x$b, l[[3]]$x$b),
      c = lapply(1:3, function(i) c(1,2))
    ),
    i = dplyr::bind_rows(list(l[[1]]$i, l[[2]]$i, l[[3]]$i))
  )
  expect_equal(test, actual)
})
