context("photosynthesis parameters")
library(photosynthesis)

test_that("symbol, R, description, note are class character", {
  checkmate::check_character(photo_parameters$symbol, any.missing = FALSE) |>
      expect_true()
  checkmate::check_character(photo_parameters$R, any.missing = FALSE) |>
    expect_true()
  checkmate::check_character(photo_parameters$description, any.missing = FALSE) |>
    expect_true()
  checkmate::check_character(photo_parameters$note, any.missing = TRUE) |>
    expect_true()
})

test_that("units are valid", {
  checkmate::check_character(photo_parameters$units, any.missing = FALSE) |>
    expect_true()
  photo_parameters |>
    dplyr::transmute(units1 = stringr::str_replace(units, "none", "1")) |>
    dplyr::pull(units1) |>
    purrr::map_lgl(units:::ud_is_parseable) |>
    all() |>
    expect_true()
})

test_that("default is numeric", {
  checkmate::check_numeric(photo_parameters$default, any.missing = TRUE) |>
    expect_true()
})

test_that("type is constants, bake, enviro, or leaf", {
  checkmate::check_subset(
    photo_parameters$type, 
    choices = photosynthesis:::get_par_types(),
    empty.ok = FALSE
  ) |>
    expect_true()
})

test_that("temperature_response, tealeaves are logical", {
  checkmate::check_logical(photo_parameters$temperature_response, any.missing = FALSE) |>
    expect_true()
  checkmate::check_logical(photo_parameters$tealeaves, any.missing = FALSE) |>
    expect_true()
})
