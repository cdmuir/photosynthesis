library(testthat)
library(photosynthesis)
context("Estimating light respiration")

df = data.frame(
  .A = c(
    seq(from = -3, to = -1.5, by = 0.5),
    seq(from = -1.25, to = 1, by = 0.25)
  ),
  .Q = c(seq(from = 0, to = 130, by = 10))
)

fit = fit_photosynthesis(
  .data = df,
  .photo_fun = "r_light",
  .model = "kok_1956",
  Q_lower = 0,
  Q_upper = 130
)

test_that("kok_1956 output", {
  expect_is(object = fit, class = "lm")
})

df = data.frame(
  .A = c(
    seq(from = 1, to = 5, by = 1),
    seq(from = 0.5, to = 2.5, by = 0.5),
    seq(from = 0.25, to = 1.25, by = 0.25)
  ),
  .C = c(
    50, 100, 150, 200, 250,
    75, 125, 175, 225, 275,
    100, 150, 200, 250, 299
  ),
  .Q = c(
    rep(1500, 5),
    rep(750, 5),
    rep(375, 5)
  )
)

fit = fit_photosynthesis(
  .data = df,
  .photo_fun = "r_light",
  .model = "walker_ort_2015",
  C_upper = 300,
  Q_levels = c(1500, 750, 375)
)

test_that("walker_ort_2015 output", {
  expect_is(object = fit, class = "lm")
})


df = data.frame(
  .A = c(
    seq(from = -3, to = -1.5, by = 0.5),
    seq(from = -1.25, to = 1, by = 0.25)
  ),
  .Q = c(seq(from = 0, to = 130, by = 10)),
  .phiPSII = c(seq(from = 0.74, to = 0.61, by = -0.01))
)

fit = fit_photosynthesis(
  .data = df,
  .photo_fun = "r_light",
  .model = "yin_etal_2011",
  Q_lower = 0,
  Q_upper = 130
)

test_that("yin_etal_2011 output", {
  expect_is(object = fit, class = "lm")
})
