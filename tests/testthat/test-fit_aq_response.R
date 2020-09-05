library(testthat)
library(photosynthesis)
context("Fitting light response curves")

df <- data.frame(
  A_net = c(10, 9.5, 8, 3.5, 2.5, 2.0, 1, 0.2),
  PPFD = c(1500, 750, 375, 125, 100, 75, 50, 25)
)

model <- fit_aq_response(df)

test_that("Outputs", {
  expect_is(object = model[1], class = "list")
  expect_is(object = model[[2]], class = "data.frame")
  expect_is(object = model[3], class = "list")
  expect_length(object = model, 3)
})
