library(testthat)
library(photosynthesis)
context("Fitting many pressure volume curves")

df <- data.frame(
  psi = c(
    -0.14, -0.8, -1.2, -1.75, -2.15,
    -2.5, -3, -4,
    -0.14, -0.8, -1.2, -1.75, -2.15,
    -2.5, -3, -4
  ),
  mass = c(
    3.47, 3.43, 3.39, 3.33, 3.22,
    3.15, 3.07, 2.98,
    3.47, 3.43, 3.39, 3.33, 3.22,
    3.15, 3.07, 2.98
  ),
  leaf_mass = c(rep(0.56, 8), rep(0.4, 8)),
  bag_mass = c(rep(1.53, 8), rep(1.14, 8)),
  leaf_area = c(rep(95, 8), rep(60, 8)),
  ID = c(rep("A", 8), rep("B", 8))
)

model <- fit_many(
  data = df,
  group = "ID",
  funct = fit_PV_curve
)

test_that("Outputs", {
  expect_is(object = model, class = "list")
  expect_is(object = model[[1]][[1]], class = "data.frame")
  expect_is(object = model[[2]][[1]], class = "data.frame")
  expect_is(object = model[[1]][2], class = "list")
  expect_is(object = model[[2]][2], class = "list")
  expect_is(object = model[[1]][3], class = "list")
  expect_is(object = model[[2]][3], class = "list")
  expect_length(object = model, 2)
  expect_length(object = model[[1]], 3)
  expect_length(object = model[[2]], 3)
})
