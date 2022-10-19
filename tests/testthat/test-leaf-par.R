context("leaf_par class")
library(photosynthesis)

test_that("constants returns class leaf_par and list", {
  lp = make_leafpar(use_tealeaves = FALSE)
  expect_s3_class(lp, "leaf_par")
  expect_s3_class(lp, "list")
})

test_that("fails when a parameter is left out", {
  lp = make_leafpar(use_tealeaves = FALSE)
  lp$g_mc25 = NULL
  expect_error(leaf_par(lp, use_tealeaves = FALSE))
})

test_that("removes an improper parameter", {
  lp = make_leafpar(use_tealeaves = FALSE)
  lp$foo = set_units(1)
  lp %<>% leaf_par(use_tealeaves = FALSE)
  expect_true(is.null(lp$foo))
})

test_that("produces error when legacy conductance units are used", {
  expect_error(lp = make_leafpar(
    replace = list(g_mc25 = set_units(1, umol / m^2 / s / Pa)), 
    use_tealeaves = FALSE
  ))
  expect_error(lp = make_leafpar(
    replace = list(g_sc = set_units(1, umol / m^2 / s / Pa)), 
    use_tealeaves = FALSE
  ))
  expect_error(lp = make_leafpar(
    replace = list(g_uc = set_units(1, umol / m^2 / s / Pa)), 
    use_tealeaves = FALSE
  ))
})
