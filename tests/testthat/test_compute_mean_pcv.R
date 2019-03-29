context('Aggregate Replicates and Compute Stats')

test_that("compute_mean_pcv computes successfully", {
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
})
