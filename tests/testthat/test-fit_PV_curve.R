library(testthat)
library(photosynthesis)
context("Fitting pressure volume curves")

df <- data.frame(
  psi = c(
    -0.14, -0.8, -1.2, -1.75, -2.15,
    -2.5, -3, -4
  ),
  mass = c(
    3.47, 3.43, 3.39, 3.33, 3.22,
    3.15, 3.07, 2.98
  ),
  leaf_mass = c(rep(0.56, 8)),
  bag_mass = c(rep(1.53, 8)),
  leaf_area = c(rep(95, 8))
)

model <- fit_PV_curve(df)

test_that("Outputs", {
  expect_is(object = model[[1]], class = "data.frame")
  expect_is(object = model[2], class = "list")
  expect_is(object = model[3], class = "list")
  expect_length(object = model, 3)
})
