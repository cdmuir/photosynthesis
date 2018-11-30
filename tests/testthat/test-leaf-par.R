context("leaf_par class")
library(photosynthesis)

test_that("constants returns class leaf_par and list", {
  lp <- make_leafpar()
  expect_s3_class(lp, "leaf_par")
  expect_s3_class(lp, "list")
})

test_that("fails when a parameter is left out", {
  lp <- make_leafpar()
  lp$g_mc25 <- NULL
  expect_error(leaf_par(lp))
})

test_that("removes an improper parameter", {
  lp <- make_leafpar()
  lp$foo <- set_units(1)
  lp %<>% leaf_par()
  expect_true(is.null(lp$foo))
})
