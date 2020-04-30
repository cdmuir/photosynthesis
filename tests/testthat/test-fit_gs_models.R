library(testthat)
library(photosynthesis)
context("Fitting stomatal conductance models")

df <- data.frame(A_net = c(15, 10, 6.5, 2, -0.2, 14.5, 19, 21, 22.5, 23),
                 g_sw = c(0.34, 0.35, 0.35, 0.37, 0.4, 0.42, 0.45, 0.41, 0.39,
                          0.36),
                 C_air = c(320, 240, 170, 90, 50, 340, 510, 710, 1070, 1450),
                 RH = c(0.49, 0.49, 0.49, 0.52, 0.54, 0.56, 0.60, 0.61, 0.61,
                        0.60),
                 VPD = c(1.76, 1.76, 1.73, 1.63, 1.54, 1.34, 1.32, 1.33, 1.34,
                         1.33))

model <- fit_gs_mod_ballberry(df)

test_that("Outputs", {
  expect_is(object = model[1], "list")
  expect_is(object = model[[2]], "data.frame")
  expect_is(object = model[3], "list")
  expect_length(object = model, 3)
})

model <- fit_gs_mod_leuning(df, D0 = 3)

test_that("Outputs", {
  expect_is(object = model[1], "list")
  expect_is(object = model[[2]], "data.frame")
  expect_is(object = model[3], "list")
  expect_length(object = model, 3)
})

model <- fit_gs_mod_medlyn(df)

test_that("Outputs", {
  expect_is(object = model[1], "list")
  expect_is(object = model[[2]], "data.frame")
  expect_is(object = model[3], "list")
  expect_length(object = model, 3)
})