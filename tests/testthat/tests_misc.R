context("Misc Functions")

library(dplyr)
library(survival)

test_that("diagnostic_plots parameter checks", {
  expect_error(diagnostic_plots(mtcars))
  expect_error(diagnostic)
  
  mod1 <- survfit(Surv(time, status) ~ x, data = aml)
  expect_error(diagnostic_plots(mod1))
  mod2 <- coxph(Surv(futime, fustat) ~ resid.ds + rx + ecog.ps, data = ovarian)
  expect_error(diagnostic_plots(mod2))
  
  mod3 <- lm(mpg ~ disp + hp + qsec, data = mtcars)
  p3 <- diagnostic_plots(mod3)
  expect_true(is.list(p3))
  expect_true(length(p3) == 6)
  
  mod4 <- lm(am ~ mpg + gear + cyl, data = mtcars)
  p4 <- diagnostic_plots(mod4)
  expect_true(is.list(p4))
  expect_true(length(p4) == 4)
})

