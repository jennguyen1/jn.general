#' Makes and Combines Xwalk
#'
#' Simplify the ids in dataset by combing all identifying columns into a grp_id
#' column. Revert back to original dataset by merging back in the xwalk
#'
#' make_xwalk makes a grp_id column from the unique grp_vars column(s)
#' merge_xwalk converts the grp_id column back into the unique columns(s)
#'
#' return
#' make_xwalk list with entires d for data.frame with grp_id column and xwalk
#' merge_xwalk data.frame with original unique identifying columns
#'

make_xwalk <- function(d, grp_vars){
  assert_cols_in(d, grp_vars)

  # makes xwalk
  xwalk <- d %>%
    dplyr::select(dplyr::one_of(grp_vars)) %>%
    dplyr::distinct() %>%
    dplyr::arrange()
  xwalk$grp_id <- as.character(1:nrow(xwalk))

  # remove grp_vars from data
  m <- merge(d, xwalk) %>%
    dplyr::select(-dplyr::one_of(grp_vars))
  assertthat::assert_that(nrow(m) == nrow(d), msg = "Xwalk creation failed")

  # results
  list(d = m, xwalk = xwalk)
}

merge_xwalk <- function(d, xwalk){
  assert_cols_in(d, "grp_id")
  o <- merge(xwalk, d) %>%
    dplyr::mutate(grp_id = as.numeric(grp_id)) %>%
    dplyr::arrange(grp_id) %>%
    dplyr::select(-grp_id)
  assertthat::assert_that(nrow(o) == nrow(d), msg = "Xwalk merge failed")
  o
}




#' Aggregates Replicates
#'
#' Computes mean, sd, pcv, n
#'
compute_mean_pcv <- function(d, var, ...){
  var <- rlang::enquo(var)
  assert_cols_in(d, !!var, ...)
  assertthat::assert_that(is.numeric(dplyr::pull(d, !!var)), msg = "var is not numeric")

  # create values response_(n)
  individual1 <- d %>%
    dplyr::select(..., !!var) %>%
    dplyr::group_by(...)
  n_len <- individual1 %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    stringr::str_length() %>%
    max()
  individual <- individual1 %>%
    dplyr::mutate(
      my_col_id = stringr::str_pad(1:dplyr::n(), width = n_len, pad = "0"),
      my_col_nms = paste0("response_", my_col_id),
      my_col_id = NULL
    ) %>%
    tidyr::spread(my_col_nms, !!var) %>%
    dplyr::select(..., dplyr::starts_with("response_"))

  # aggregated values
  aggregated <- d %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      n = length(!!var),
      response_mean = mean(!!var),
      response_sd = sd(!!var),
      response_pcv = sd(!!var) / mean(!!var) * 100
    ) %>%
    dplyr::ungroup()

  # combine and order columns
  m <- merge(individual, aggregated)
  dplyr::select(m, ..., n, dplyr::starts_with("response_"), response_mean, response_sd, response_pcv)
}

# tests
a1 <- rnorm(11)
a2 <- 1:3
b1 <- rnorm(2)
b3 <- 0
d <- data.frame(
  x = c(rep('a', 14), rep('b', 3)),
  y = c(rep(1, 11), rep(2, 3), 1, 1, 3),
  r = c(a1, a2, b1, b3),
  a = 'a'
)
o.a <- rbind(
  a1,
  c(a2, rep(NA, 8)),
  c(b1, rep(NA, 9)),
  c(b3, rep(NA, 10))
) %>%
  as.data.frame() %>%
  purrr::set_names(c(paste0("response_0", 1:9), "response_10", "response_11"))
o.b <- data.frame(x = c('a', 'a', 'b', 'b'), y = c(1, 2, 1, 3), o.a)
rownames(o.b) <- NULL
all <- list(a1, a2, b1, b3)
o.c <- data.frame(
  o.b,
  n = purrr::map_dbl(all, length),
  response_mean = purrr::map_dbl(all, mean),
  response_sd = purrr::map_dbl(all, sd),
  response_pcv = purrr::map_dbl(all, ~ sd(.x) / mean(.x) * 100)
)
o <- dplyr::select(o.c, x, y, n, dplyr::everything())
expect_equal(compute_mean_pcv(d, var = r, x, y), o)
expect_error(compute_mean_pcv(d, var = a, x, y), "not a numeric")
expect_error(compute_mean_pcv(d, var = r, e, y), "not available in")
