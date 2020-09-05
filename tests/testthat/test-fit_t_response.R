library(testthat)
library(photosynthesis)
context("Fitting temperature response curves")

df <- data.frame(
  Par = c(38, 42, 55, 58, 63, 62, 83, 104, 116, 98),
  Tleaf = c(17, 20, 22, 25, 27, 30, 32, 35, 37, 40)
)
df$T_leaf <- df$Tleaf + 273.15

model <- suppressWarnings(fit_t_response(df))

test_that("Outputs", {
  expect_is(object = model, class = "list")
  expect_is(object = model[1], class = "list")
  expect_is(object = model[[2]][[2]], class = "data.frame")
  expect_is(object = model[[3]][3], class = "list")
  expect_length(object = model, 7)
  expect_length(object = model[[4]], 3)
})
