library(testthat)
library(photosynthesis)
context("Fitting gm")

df <- data.frame(
  A_net = c(15, 10, 6.5, 2, -0.2, 14.5, 19, 21, 22.5, 23),
  J_etr = c(130, 110, 95, 75, 60, 130, 145, 150, 150, 145),
  C_i = c(320, 240, 170, 90, 50, 340, 510, 710, 1070, 1450)
)

model <- fit_g_mc_variableJ(df, gamma_star = 42, R_d = 1)

test_that("Outputs", {
  expect_length(object = model, length(df) + 6)
})
