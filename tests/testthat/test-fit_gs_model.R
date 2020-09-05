library(testthat)
library(photosynthesis)
context("Fitting stomatal conductance models")

df <- data.frame(
  A_net = c(15, 10, 6.5, 2, -0.2, 14.5, 19, 21, 22.5, 23),
  g_sw = c(
    0.34, 0.35, 0.35, 0.37, 0.4, 0.42, 0.45, 0.41, 0.39,
    0.36
  ),
  C_air = c(320, 240, 170, 90, 50, 340, 510, 710, 1070, 1450),
  RH = c(
    0.49, 0.49, 0.49, 0.52, 0.54, 0.56, 0.60, 0.61, 0.61,
    0.60
  ),
  VPD = c(
    1.76, 1.76, 1.73, 1.63, 1.54, 1.34, 1.32, 1.33, 1.34,
    1.33
  )
)

model <- fit_gs_model(
  data = df,
  varnames = list(
    A_net = "A_net",
    C_air = "C_air",
    g_sw = "g_sw",
    RH = "RH",
    VPD = "VPD"
  ),
  model = c(
    "BallBerry",
    "Leuning",
    "Medlyn_partial",
    "Medlyn_full"
  ),
  D0 = 3
)

test_that("Outputs", {
  expect_is(object = model, "list")
  expect_length(object = model, 4)
  expect_is(object = model[[1]], "list")
  expect_length(object = model[[1]], 3)
  expect_is(object = model[[2]], "list")
  expect_length(object = model[[2]], 3)
  expect_is(object = model[[3]], "list")
  expect_length(object = model[[3]], 3)
  expect_is(object = model[[4]], "list")
  expect_length(object = model[[4]], 3)
  expect_is(object = model[[1]][1], "list")
  expect_is(object = model[[1]][[2]], "data.frame")
  expect_is(object = model[[1]][3], "list")
  expect_is(object = model[[2]][1], "list")
  expect_is(object = model[[2]][[2]], "data.frame")
  expect_is(object = model[[2]][3], "list")
  expect_is(object = model[[3]][1], "list")
  expect_is(object = model[[3]][[2]], "data.frame")
  expect_is(object = model[[3]][3], "list")
  expect_is(object = model[[4]][1], "list")
  expect_is(object = model[[4]][[2]], "data.frame")
  expect_is(object = model[[4]][3], "list")
})
