context("fit_aq_response2()")
library(photosynthesis)

test_that("'fit_aq_response2()' accepts data.frames and tibbles", {
  
  df1 = data.frame(
    .A = c(10, 9.5, 8, 3.5, 2.5, 2.0, 1, 0.2),
    .Q = c(1500, 750, 375, 125, 100, 75, 50, 25)
  )
  df2 = tibble::as_tibble(df1)
  
  expect_no_condition({fit_aq_response2(.data = df1)})
  expect_no_condition({fit_aq_response2(.data = df2)})
  
})

test_that(".vars argument renames variables", {
  
  df1 = data.frame(
    .A = c(10, 9.5, 8, 3.5, 2.5, 2.0, 1, 0.2),
    .Q = c(1500, 750, 375, 125, 100, 75, 50, 25)
  )
  df2 = dplyr::rename(df1, Photo = .A)
  df3 = dplyr::rename(df1, PPFD = .Q)
  df4 = dplyr::rename(df1, Photo = .A, PPFD = .Q)
  
  expect_error({fit_aq_response2(.data = df2)})
  expect_error({fit_aq_response2(.data = df3)})
  expect_error({fit_aq_response2(.data = df4)})

  expect_no_condition({
    fit_aq_response2(.data = df2, .vars = list(.A = Photo))
  })
  expect_no_condition({
    fit_aq_response2(.data = df3, .vars = list(.Q = PPFD))
  })
  expect_no_condition({
    fit_aq_response2(.data = df4, .vars = list(.A = Photo, .Q = PPFD))
  })
  
  expect_error({fit_aq_response2(.data = df2, .vars(.A = foo))})
  expect_error({fit_aq_response2(.data = df3, .vars(.Q = bar))})

  expect_no_condition({
    fit_aq_response2(.data = dplyr::mutate(df4, A1 = Photo), 
                     .vars = list(.A = Photo, .Q = PPFD))
  })
  
})
