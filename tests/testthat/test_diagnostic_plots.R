context("Diagnostic Plots")

library(dplyr)
library(survival)

test_that("diagnostic_plots handles missing data", {
  expect_error(diagnostic_plots(mtcars))
  expect_error(diagnostic_plots())
})

test_that("diagnostic_plots only works with lm or glm", {
  mod1 <- survfit(Surv(time, status) ~ x, data = aml)
  expect_error(diagnostic_plots(mod1))
  mod2 <- coxph(Surv(futime, fustat) ~ resid.ds + rx + ecog.ps, data = ovarian)
  expect_error(diagnostic_plots(mod2))
})

test_that("diagnostic_plots returns lists of expected sizes", {
  mod3 <- lm(mpg ~ disp + hp + qsec, data = mtcars)
  p3 <- diagnostic_plots(mod3)
  expect_true(is.list(p3))
  expect_true(length(p3) == 6)
  
  mod4 <- glm(am ~ mpg + gear + cyl, data = mtcars)
  p4 <- diagnostic_plots(mod4)
  expect_true(is.list(p4))
  expect_true(length(p4) == 4)
})

