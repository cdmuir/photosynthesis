context("temperature responses")
library(photosynthesis)

test_that("baked parameters do not equal unbaked unless Temp = 25", {
  cs <- make_constants(use_tealeaves = FALSE)
  bp <- make_bakepar()
  ep <- make_enviropar(use_tealeaves = FALSE)
  lp <- make_leafpar(use_tealeaves = FALSE)

  lp$T_leaf <- set_units(298.15, "K")
  blp <- bake(lp, bp, cs)

  blp %>%
    as.data.frame() %>%
    dplyr::select(tidyselect::ends_with("25")) %>%
    colnames() %>%
    stringr::str_remove("25$") %>%
    glue::glue("blp${x} == blp${x}25", x = .) %>%
    purrr::map_lgl(function(x) eval(parse(text = x))) %>%
    all() %>%
    expect_true()

  lp$T_leaf <- set_units(305, "K")
  blp <- bake(lp, bp, cs)

  blp %>%
    as.data.frame() %>%
    dplyr::select(tidyselect::ends_with("25")) %>%
    colnames() %>%
    stringr::str_remove("25$") %>%
    glue::glue("blp${x} == blp${x}25", x = .) %>%
    purrr::map_lgl(function(x) eval(parse(text = x))) %>%
    any() %>%
    expect_false()
})
