library(testthat)
library(photosynthesis)
context("Fitting ACi curves")

df = data.frame(
  A_net = c(15, 10, 6.5, 2, -0.2, 14.5, 19, 21, 22.5, 23),
  T_leaf = rep(298, 10),
  C_i = c(320, 240, 170, 90, 50, 340, 510, 710, 1070, 1450),
  PPFD = c(rep(1500, 10))
)

model = fit_aci_response(df)

test_that("Outputs", {
  expect_is(object = model[[1]], class = "data.frame")
  expect_is(object = model[2], class = "list")
  expect_is(object = model[[3]], class = "data.frame")
  expect_length(object = model, 3)
})
