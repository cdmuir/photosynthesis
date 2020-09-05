context("photosynthesis() crossing")
library(photosynthesis)

test_that("photosynthesis output has correct dimensions when tealeaves = FALSE", {
  bp <- make_bakepar()
  cs <- make_constants(use_tealeaves = FALSE)
  ep <- make_enviropar(
    replace = list(
      PPFD = set_units(c(1000, 1500), "umol/m^2/s")
    ), use_tealeaves = FALSE
  )
  lp <- make_leafpar(
    replace = list(
      g_sc = set_units(c(2, 4), "umol/m^2/s/Pa")
    ), use_tealeaves = FALSE
  )

  ph <- photosynthesis(
    lp, ep, bp, cs,
    use_tealeaves = FALSE, progress = FALSE, quiet = TRUE
  )
  n <- exp(sum(log(c(
    sapply(bp, length), sapply(lp, length),
    sapply(ep, length)
  ))))
  expect_equal(nrow(ph), n)
})

test_that("photosynthesis output has correct dimensions when tealeaves = TRUE", {
  bp <- make_bakepar()
  cs <- make_constants(use_tealeaves = TRUE)
  ep <- make_enviropar(
    replace = list(
      PPFD = set_units(c(1000, 1500), "umol/m^2/s")
    ), use_tealeaves = TRUE
  )
  lp <- make_leafpar(
    replace = list(
      g_sc = set_units(c(2, 4), "umol/m^2/s/Pa")
    ), use_tealeaves = TRUE
  )

  ph <- photosynthesis(
    lp, ep, bp, cs,
    use_tealeaves = TRUE, progress = FALSE, quiet = TRUE
  )
  n <- exp(sum(log(c(
    sapply(bp, length), sapply(lp, length),
    sapply(ep, length)
  ))))
  expect_equal(nrow(ph), n)
})
