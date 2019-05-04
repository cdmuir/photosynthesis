context("enviro_par class")
library(photosynthesis)

test_that("constants returns class enviro_par and list", {
  ep <- make_enviropar(use_tealeaves = FALSE)
  expect_s3_class(ep, "enviro_par")
  expect_s3_class(ep, "list")
})

test_that("fails when a parameter is left out", {
  ep <- make_enviropar(use_tealeaves = FALSE)
  ep$P <- NULL
  expect_error(enviro_par(ep, use_tealeaves = FALSE))
})

test_that("removes an improper parameter", {
  ep <- make_enviropar(use_tealeaves = FALSE)
  ep$foo <- set_units(1)
  ep %<>% enviro_par(use_tealeaves = FALSE)
  expect_true(is.null(ep$foo))
})
