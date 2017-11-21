context("Working with Duplicates")

dups <- data.frame(
  x = rep(1:3, each = 4),
  y = rep(1:4, each = 3),
  z = rep(1:2, 6), 
  a = rep(1:6, 2)
)
empty_df <- head(dups, 0)
dups_on_yz <- dplyr::bind_rows(list(
  data.frame(x = 1, y = 1, z = 1, a = 1),
  data.frame(x = 1, y = 1, z = 1, a = 3),
  data.frame(x = 1, y = 2, z = 2, a = 4), 
  data.frame(x = 2, y = 2, z = 2, a = 6), 
  data.frame(x = 2, y = 3, z = 1, a = 1),
  data.frame(x = 3, y = 3, z = 1, a = 3),
  data.frame(x = 3, y = 4, z = 2, a = 4), 
  data.frame(x = 3, y = 4, z = 2, a = 6)
))
dups_on_xyz <- dplyr::bind_rows(list(
  data.frame(x = 1, y = 1, z = 1, a = 1),
  data.frame(x = 1, y = 1, z = 1, a = 3),
  data.frame(x = 3, y = 4, z = 2, a = 4),
  data.frame(x = 3, y = 4, z = 2, a = 6)
))

all_dups <- data.frame(
  x = c(1:10, 1:10),
  y = c(11:20, 11:20)
)


# testing view_duplicated
test_that("view_duplicated finds duplicates", {
  expect_equal(view_duplicated(dups, y, z), dups_on_yz)
  expect_equal(view_duplicated(dups, x, y, z), dups_on_xyz)
})

test_that("view_duplicated when there are no duplicates", {
  expect_equal(view_duplicated(empty_df), empty_df)
  expect_equal(view_duplicated(dups), head(dups, 0))
})

test_that("view_duplicated when there are all duplicates", {
  expect_equal(view_duplicated(all_dups), all_dups)
})

test_that("view_duplicated handles missing and invalid arguments", {
  expect_error(view_duplicated())
  expect_error(view_duplicated(1:10))
  expect_error(view_duplicated(list()))
  expect_equal(view_duplicated(tibble::as.tibble(dups)), empty_df)
})

test_that("view_duplicated only accepts columns within data frame", {
  expect_error(view_duplicated(dups, q, v))
})


# testing remove_duplicated
test_that("removes_duplicated removes duplicates", {
  rm_first_on_yz <- dplyr::anti_join(dups, dplyr::slice(dups_on_yz, c(2, 4, 6, 8)))
  rm_last_on_yz <- dplyr::anti_join(dups, dplyr::slice(dups_on_yz, c(1, 3, 5, 7)))
  rm_all_on_yz <- dplyr::anti_join(dups, dups_on_yz)
  
  expect_equal(remove_duplicated(dups, y, z, opt_delete = "from first"), rm_first_on_yz)
  expect_equal(remove_duplicated(dups, y, z, opt_delete = "from last"), rm_last_on_yz)
  expect_equal(remove_duplicated(dups, y, z, opt_delete = "all"), rm_all_on_yz)
  
  rm_first_on_xyz <- dplyr::anti_join(dups, dplyr::slice(dups_on_xyz, c(2, 4)))
  rm_last_on_xyz <- dplyr::anti_join(dups, dplyr::slice(dups_on_xyz, c(1, 3)))
  rm_all_on_xyz <- dplyr::anti_join(dups, dups_on_xyz)
  
  expect_equal(remove_duplicated(dups, x, y, z, opt_delete = "from first"), rm_first_on_xyz)
  expect_equal(remove_duplicated(dups, x, y, z, opt_delete = "from last"), rm_last_on_xyz)
  expect_equal(remove_duplicated(dups, x, y, z, opt_delete = "all"), rm_all_on_xyz)
})

test_that("remove_duplicated when there are no duplicates", {
  expect_equal(remove_duplicated(empty_df), empty_df)
  expect_equal(remove_duplicated(dups), dups)
})

test_that("remove_duplicated when there all duplicates", {
  expect_equal(remove_duplicated(all_dups), head(all_dups, 10))
})

test_that("remove_duplicated handles missing data and invalid arguments", {
  expect_error(remove_duplicated())
  expect_error(remove_duplicated(1:10))
  expect_error(remove_duplicated(list()))
  expect_error(remove_duplicated(dups, opt_delete = "something"))
  expect_error(remove_duplicated(dups, opt_delete = 1))
  expect_error(remove_duplicated(dups, opt_delete = TRUE))
  expect_error(remove_duplicated(dups, opt_summary = "something"))
  expect_error(remove_duplicated(dups, opt_summary = 1))
})

