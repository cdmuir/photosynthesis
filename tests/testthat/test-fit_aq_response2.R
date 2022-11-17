context("fit_photosynthesis()")
library(photosynthesis)

test_that("'fit_photosynthesis()' accepts data.frames and tibbles", {
  
  df1 = data.frame(
    .A = c(10, 9.5, 8, 3.5, 2.5, 2.0, 1, 0.2),
    .Q = c(1500, 750, 375, 125, 100, 75, 50, 25)
  )
  df2 = tibble::as_tibble(df1)
  
  expect_no_condition({fit_photosynthesis(.data = df1, .photo_fun = "aq_response")})
  expect_no_condition({fit_photosynthesis(.data = df2, .photo_fun = "aq_response")})
  
})

test_that(".vars argument renames variables", {
  
  df1 = data.frame(
    .A = c(10, 9.5, 8, 3.5, 2.5, 2.0, 1, 0.2),
    .Q = c(1500, 750, 375, 125, 100, 75, 50, 25)
  )
  df2 = dplyr::rename(df1, Photo = .A)
  df3 = dplyr::rename(df1, PPFD = .Q)
  df4 = dplyr::rename(df1, Photo = .A, PPFD = .Q)
  
  expect_error({fit_photosynthesis(.data = df2, .photo_fun = "aq_response")})
  expect_error({fit_photosynthesis(.data = df3, .photo_fun = "aq_response")})
  expect_error({fit_photosynthesis(.data = df4, .photo_fun = "aq_response")})

  expect_no_condition({
    fit_photosynthesis(.data = df2, .photo_fun = "aq_response", .vars = list(.A = Photo))
  })
  expect_no_condition({
    fit_photosynthesis(.data = df3, .photo_fun = "aq_response", .vars = list(.Q = PPFD))
  })
  expect_no_condition({
    fit_photosynthesis(.data = df4, .photo_fun = "aq_response", .vars = list(.A = Photo, .Q = PPFD))
  })
  
  expect_error({fit_photosynthesis(.data = df2, .photo_fun = "aq_response", .vars(.A = foo))})
  expect_error({fit_photosynthesis(.data = df3, .photo_fun = "aq_response", .vars(.Q = bar))})

  expect_no_condition({
    fit_photosynthesis(.data = dplyr::mutate(df4, A1 = Photo), .photo_fun = "aq_response",
                     .vars = list(.A = Photo, .Q = PPFD))
  })
  
})
