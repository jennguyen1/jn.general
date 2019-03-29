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
