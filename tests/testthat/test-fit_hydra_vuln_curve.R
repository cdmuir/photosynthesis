library(testthat)
library(photosynthesis)
context("Fitting hydraulic vulnerability curves")

df <- data.frame(
  psi = c(0, 1.5, 3, 4.6, 5.3, 6.1),
  PLC = c(0, 7, 17, 38, 66, 90)
)

model <- fit_hydra_vuln_curve(df)

test_that("Outputs", {
  expect_is(object = model[1], class = "list")
  expect_is(object = model[[2]], class = "data.frame")
  expect_is(object = model[[3]], class = "data.frame")
  expect_is(object = model[4], class = "list")
  expect_is(object = model[5], class = "list")
  expect_length(object = model, 5)
})
