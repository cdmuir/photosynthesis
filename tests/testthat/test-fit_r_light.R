library(testthat)
library(photosynthesis)
context("Fitting Rlight")

df <- data.frame(
  A_net = c(
    seq(from = -3, to = -1.5, by = 0.5),
    seq(from = -1.25, to = 1, by = 0.25)
  ),
  PPFD = c(seq(from = 0, to = 130, by = 10))
)

model <- fit_r_light_kok(df)

test_that("Outputs", {
  expect_is(object = model, class = "numeric")
})

df <- data.frame(
  A_net = c(
    seq(from = 1, to = 5, by = 1),
    seq(from = 0.5, to = 2.5, by = 0.5),
    seq(from = 0.25, to = 1.25, by = 0.25)
  ),
  C_i = c(
    50, 100, 150, 200, 250,
    75, 125, 175, 225, 275,
    100, 150, 200, 250, 299
  ),
  PPFD = c(
    rep(1500, 5),
    rep(750, 5),
    rep(375, 5)
  )
)

model <- fit_r_light_WalkerOrt(df)

test_that("Outputs", {
  expect_is(object = model[1], class = "list")
  expect_is(object = model[2], class = "list")
  expect_is(object = model[[3]], class = "data.frame")
  expect_length(object = model, 3)
})

df <- data.frame(
  A_net = c(
    seq(from = -3, to = -1.5, by = 0.5),
    seq(from = -1.25, to = 1, by = 0.25)
  ),
  PPFD = c(seq(from = 0, to = 130, by = 10)),
  phi_PSII = c(seq(from = 0.74, to = 0.61, by = -0.01))
)

model <- fit_r_light_yin(df)

test_that("Outputs", {
  expect_is(object = model, class = "numeric")
})
