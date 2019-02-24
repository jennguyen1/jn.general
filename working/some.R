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
  assert_cols_in(d, !!var)

  # create values response_(n)
  individual <- d %>%
    dplyr::select(..., !!var) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(
      my_col_id = stringr::str_pad(1:dplyr::n(), width = stringr::str_length(dplyr::n()), pad = "0"),
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
