#' Aggregates Replicates
#'
#' Computes mean, sd, pcv, n
#'
#' @param d data.frame
#' @param var name of variable to aggregate (no quotes)
#' @param ... name of variables to group by
#'
#' @return
#' data.frame
#'
#' @export
#'
#' @examples
#' compute_mean_pcv(iris, var = Petal.Lenth, Species)
#'
compute_mean_pcv <- function(d, var, ...){
  var <- rlang::enquo(var)
  assert_cols_in(d, !!var, ...)
  assertthat::assert_that(is.numeric(dplyr::pull(d, !!var)), msg = "var is not a numeric")

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
