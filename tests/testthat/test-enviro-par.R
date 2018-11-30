context("enviro_par class")
library(photosynthesis)

test_that("constants returns class enviro_par and list", {
  ep <- make_enviropar()
  expect_s3_class(ep, "enviro_par")
  expect_s3_class(ep, "list")
})

test_that("fails when a parameter is left out", {
  ep <- make_enviropar()
  ep$P <- NULL
  expect_error(enviro_par(ep))
})

test_that("removes an improper parameter", {
  ep <- make_enviropar()
  ep$foo <- set_units(1)
  ep %<>% enviro_par()
  expect_true(is.null(ep$foo))
})
