context("constants class")
library(photosynthesis)

test_that("constants returns class constants and list", {
  cnstnts <- make_constants(use_tealeaves = FALSE)
  expect_s3_class(cnstnts, "constants")
  expect_s3_class(cnstnts, "list")

  cnstnts <- make_constants(use_tealeaves = TRUE)
  expect_s3_class(cnstnts, "constants")
  expect_s3_class(cnstnts, "list")
})

test_that("fails when a parameter is left out", {
  cnstnts <- make_constants(use_tealeaves = FALSE)
  cnstnts$D_c0 <- NULL
  expect_error(constants(cnstnts))

  cnstnts <- make_constants(use_tealeaves = TRUE)
  cnstnts$D_c0 <- NULL
  expect_error(constants(cnstnts))
})

test_that("removes an improper parameter", {
  cnstnts <- make_constants(use_tealeaves = FALSE)
  cnstnts$foo <- set_units(1)
  cnstnts %<>% constants(use_tealeaves = FALSE)
  expect_true(is.null(cnstnts$foo))
})

test_that("nu_constant returns a list of two numbers", {
  cnstnts <- make_constants(use_tealeaves = FALSE)
  T_high <- set_units(298.15, "K")
  T_low <- set_units(300, "K")
  nu <- cnstnts$nu_constant(
    set_units(3999), "forced",
    T_high, T_low, "lower", FALSE
  )
  expect_true(is.list(nu))
  expect_true(length(nu) == 2L)
  expect_true(length(nu[[1]]) == 1L)
  expect_true(length(nu[[2]]) == 1L)
  expect_true(is.numeric(nu[[1]]))
  expect_true(is.numeric(nu[[2]]))

  nu <- cnstnts$nu_constant(
    set_units(4001), "forced",
    T_high, T_low, "lower", FALSE
  )
  expect_true(is.list(nu))
  expect_true(length(nu) == 2L)
  expect_true(length(nu[[1]]) == 1L)
  expect_true(length(nu[[2]]) == 1L)
  expect_true(is.numeric(nu[[1]]))
  expect_true(is.numeric(nu[[2]]))

  nu <- cnstnts$nu_constant(
    set_units(1), "free",
    T_high, T_low, "lower", FALSE
  )
  expect_true(is.list(nu))
  expect_true(length(nu) == 2L)
  expect_true(length(nu[[1]]) == 1L)
  expect_true(length(nu[[2]]) == 1L)
  expect_true(is.numeric(nu[[1]]))
  expect_true(is.numeric(nu[[2]]))

  nu <- cnstnts$nu_constant(
    set_units(1), "free",
    T_high, T_low, "upper", FALSE
  )
  expect_true(is.list(nu))
  expect_true(length(nu) == 2L)
  expect_true(length(nu[[1]]) == 1L)
  expect_true(length(nu[[2]]) == 1L)
  expect_true(is.numeric(nu[[1]]))
  expect_true(is.numeric(nu[[2]]))

  nu1 <- cnstnts$nu_constant(
    set_units(1), "free",
    T_high, T_low, "lower", FALSE
  )
  nu2 <- cnstnts$nu_constant(
    set_units(1), "free",
    T_low, T_high, "upper", FALSE
  )
  expect_equal(nu1, nu2)

  expect_error(cnstnts$nu_constant(
    set_units(1), "foo",
    T_high, T_low, "lower"
  ))
})

test_that("sh_constant returns a vector of one unitless number of numeric
          class", {
  cnstnts <- make_constants(use_tealeaves = FALSE)
  sh <- cnstnts$sh_constant("forced")
  expect_true(length(sh) == 1L)
  expect_true(is(sh, "numeric"))
  expect_true(!is(sh, "units"))

  sh <- cnstnts$sh_constant("free")
  expect_true(length(sh) == 1L)
  expect_true(!is(sh, "units"))
})
