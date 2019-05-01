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


#' Generate Pretty Plot Breaks
#'
#' Steps are equidistant from each other and are multiples of the chosen steps
#'
#' @param mn numeric minimum break
#' @param mx numeric maximum break
#' @param steps numeric vector of step by options
#' @param between numeric vector in ascending order specifying the allowed number of breaks/steps
#'
#' @return numeric vector
#'
#' @export
#'
plot_breaks <- function(mn, mx, steps, between = c(5, 10)){
  assertthat::assert_that(
    is.numeric(mn), is.numeric(mx), is.numeric(between),
    length(mn) == 1, length(mx) == 1, length(between) == 2,
    is.numeric(steps), between[1] < between[2]
  )

  # create potential break options
  opts <- purrr::map(steps, ~ seq(mn, mx, by = .x))
  names(opts) <- purrr::set_names(as.character(steps))

  # compare options; keep ones where # breaks is between specified
  keep <- tail(purrr::keep(opts, ~ dplyr::between(length(.x), between[1], between[2])), 1)
  o <- purrr::pluck(keep, 1)

  # reformat to look pretty
  if(is.null(o)){
    warning("No best found, consider changing steps or between")
    purrr::pluck(tail(opts, 1), 1)
  } else{
    breaks_pretty(o, as.numeric(names(keep)))
  }

}

breaks_pretty <- function(x, n){
  if(x[1] %% n == 0){
    x
  } else{
    o <- x - (x[1] %% n)
    c(o, max(o) + n)
  }
}


