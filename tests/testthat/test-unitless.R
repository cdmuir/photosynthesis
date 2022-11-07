context("unitless values match unit-ed values")
library(photosynthesis)

test_that("unitless values match unit-ed values (!tealeaves, new g_tc)", {
  cs1 = make_constants(use_tealeaves = FALSE)
  cs2 = purrr::map_if(cs1, function(x) is(x, "units"), drop_units)

  lp1 = leaf_par(list(
    A_mes_A = set_units(runif(1, 5, 40), 1), 
    delta_ias_lower = set_units(runif(1, 0, 1000), um), 
    delta_ias_upper = set_units(runif(1, 0, 1000), um), 
    g_liqc25 = set_units(runif(1, 0, 0.1), mol/m^2/s),
    g_mc25 = set_units(runif(1), mol/m^2/s),
    g_sc = set_units(runif(1), mol/m^2/s),
    g_uc = set_units(runif(1), mol/m^2/s),
    gamma_star25 = set_units(runif(1, 30, 50), umol/mol),
    J_max25 = set_units(runif(1, 50, 200), umol/m^2/s),
    K_C25 = set_units(268.3, umol/mol),
    K_O25 = set_units(165084.2, umol/mol),
    k_mc = set_units(runif(1, 0.01, 100)),
    k_sc = set_units(runif(1, 0.01, 100)),
    k_uc = set_units(runif(1, 0.01, 100)),
    leafsize = set_units(runif(1), m),
    phi_J = set_units(0.331),
    R_d25 = set_units(runif(1, 0, 5), umol/m^2/s),
    T_leaf = set_units(runif(1, 273.15, 313.15), K),
    theta_J = set_units(0.825),
    V_cmax25 = set_units(runif(1, 50, 200), umol/m^2/s),
    V_tpu25 = set_units(runif(1, 50, 200), umol/m^2/s)
  ), use_tealeaves = FALSE)

  lp2 = purrr::map_if(lp1, function(x) is(x, "units"), drop_units)

  ep1 = enviro_par(list(
    C_air = set_units(runif(1, 10, 2000), umol/mol),
    O = set_units(0.21, mol/mol),
    P = set_units(101.3246, kPa),
    RH = set_units(runif(1)),
    PPFD = set_units(runif(1, 0, 2000), umol/m^2/s),
    wind = set_units(runif(1, 0, 20), m/s)
  ), use_tealeaves = FALSE)

  ep1$T_air = lp1$T_leaf
  ep2 = purrr::map_if(ep1, function(x) is(x, "units"), drop_units)
  
  bp1 = make_bakepar()
  bp2 = purrr::map_if(bp1, ~ is(.x, "units"), drop_units)

  lp1 %<>% bake(ep1, bp1, cs1, assert_units = TRUE)
  lp2 %<>% bake(ep2, bp2, cs2, assert_units = FALSE)

  purrr::map2(lp1, lp2[names(lp1)], function(.x, .y) {
    dplyr::near(drop_units(.x), drop_units(.y))
  }) %>%
    purrr::map(expect_true)
  
  C_chl = set_units(runif(1, 200, 400), umol/mol)
  pars1 = c(cs1, lp1, ep1)
  pars2 = purrr::map_if(pars1, function(x) is(x, "units"), drop_units)

  Ad1 = A_demand(C_chl, pars1, unitless = FALSE)
  Ad2 = A_demand(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(drop_units(Ad1), Ad2)

  As1 = A_supply(C_chl, pars1, unitless = FALSE, use_legacy_version = FALSE)
  As2 = A_supply(drop_units(C_chl), pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(drop_units(As1), As2)

  gbc1 = drop_units(.get_gbc(pars1, "lower", unitless = FALSE, use_legacy_version = FALSE))
  gbc2 = .get_gbc(pars2, "lower", unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gbc1, gbc2)

  gbc3 = drop_units(.get_gbc(pars1, "upper", unitless = FALSE, use_legacy_version = FALSE))
  gbc4 = .get_gbc(pars2, "upper", unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gbc3, gbc4)

  gmc1 = drop_units(.get_gmc(pars1, "lower", unitless = FALSE))
  gmc2 = .get_gmc(pars2, "lower", unitless = TRUE)
  expect_equal(gmc1, gmc2)

  gmc3 = drop_units(.get_gmc(pars1, "upper", unitless = FALSE))
  gmc4 = .get_gmc(pars2, "upper", unitless = TRUE)
  expect_equal(gmc3, gmc4)

  gsc1 = drop_units(.get_gsc(pars1, "lower", unitless = FALSE))
  gsc2 = .get_gsc(pars2, "lower", unitless = TRUE)
  expect_equal(gsc1, gsc2)

  gsc3 = drop_units(.get_gsc(pars1, "upper", unitless = FALSE))
  gsc4 = .get_gsc(pars2, "upper", unitless = TRUE)
  expect_equal(gsc3, gsc4)

  gtc1 = drop_units(.get_gtc(pars1, unitless = FALSE, use_legacy_version = FALSE))
  gtc2 = .get_gtc(pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gtc1, gtc2)

  gtc3 = drop_units(.get_gtc(pars1, unitless = FALSE, use_legacy_version = FALSE))
  gtc4 = .get_gtc(pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gtc3, gtc4)

  guc1 = drop_units(.get_guc(pars1, "lower", unitless = FALSE))
  guc2 = .get_guc(pars2, "lower", unitless = TRUE)
  expect_equal(guc1, guc2)

  guc3 = drop_units(.get_guc(pars1, "upper", unitless = FALSE))
  guc4 = .get_guc(pars2, "upper", unitless = TRUE)
  expect_equal(guc3, guc4)

  FvCB1 = FvCB(C_chl, pars1, unitless = FALSE)
  FvCB2 = FvCB(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(drop_units(FvCB1$A), FvCB2$A)

  J1 = drop_units(J(pars1, unitless = FALSE))
  J2 = J(pars2, unitless = TRUE)
  expect_equal(J1, J2)

  Wc1 = drop_units(W_carbox(C_chl, pars1, unitless = FALSE))
  Wc2 = W_carbox(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wc1, Wc2)

  Wr1 = drop_units(W_regen(C_chl, pars1, unitless = FALSE))
  Wr2 = W_regen(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wr1, Wr2)

  Wt1 = drop_units(W_tpu(C_chl, pars1, unitless = FALSE))
  Wt2 = W_tpu(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wt1, Wt2)
})

test_that("unitless values match unit-ed values (!tealeaves, old g_tc)", {
  cs1 = make_constants(use_tealeaves = FALSE)
  cs2 = purrr::map_if(cs1, function(x) is(x, "units"), drop_units)
  
  lp1 = leaf_par(list(
    A_mes_A = set_units(numeric(0), 1), 
    delta_ias_lower = set_units(numeric(0), um), 
    delta_ias_upper = set_units(numeric(0), um), 
    g_liqc25 = set_units(numeric(0), mol/m^2/s),
    g_mc25 = set_units(runif(1), mol/m^2/s),
    g_sc = set_units(runif(1), mol/m^2/s),
    g_uc = set_units(runif(1), mol/m^2/s),
    gamma_star25 = set_units(runif(1, 30, 50), umol/mol),
    J_max25 = set_units(runif(1, 50, 200), umol/m^2/s),
    K_C25 = set_units(268.3, umol/mol),
    K_O25 = set_units(165084.2, umol/mol),
    k_mc = set_units(runif(1, 0.01, 100)),
    k_sc = set_units(runif(1, 0.01, 100)),
    k_uc = set_units(runif(1, 0.01, 100)),
    leafsize = set_units(runif(1), m),
    phi_J = set_units(0.331),
    R_d25 = set_units(runif(1, 0, 5), umol/m^2/s),
    T_leaf = set_units(runif(1, 273.15, 313.15), K),
    theta_J = set_units(0.825),
    V_cmax25 = set_units(runif(1, 50, 200), umol/m^2/s),
    V_tpu25 = set_units(runif(1, 50, 200), umol/m^2/s)
  ), use_tealeaves = FALSE)
  
  lp2 = purrr::map_if(lp1, function(x) is(x, "units"), drop_units)
  
  ep1 = enviro_par(list(
    C_air = set_units(runif(1, 10, 2000), umol/mol),
    O = set_units(0.21, mol/mol),
    P = set_units(101.3246, kPa),
    RH = set_units(runif(1)),
    PPFD = set_units(runif(1, 0, 2000), umol/m^2/s),
    wind = set_units(runif(1, 0, 20), m/s)
  ), use_tealeaves = FALSE)
  
  ep1$T_air = lp1$T_leaf
  ep2 = purrr::map_if(ep1, function(x) is(x, "units"), drop_units)
  
  bp1 = make_bakepar()
  bp2 = purrr::map_if(bp1, ~ is(.x, "units"), drop_units)
  
  lp1 %<>% bake(ep1, bp1, cs1, assert_units = TRUE)
  lp2 %<>% bake(ep2, bp2, cs2, assert_units = FALSE)
  
  purrr::map2(lp1, lp2[names(lp1)], function(.x, .y) {
    if (length(.x) == 0) {TRUE} else {dplyr::near(drop_units(.x), drop_units(.y))}
  }) |>
    purrr::map(expect_true)
  
  C_chl = set_units(runif(1, 200, 400), umol/mol)
  pars1 = c(cs1, lp1, ep1)
  pars2 = purrr::map_if(pars1, function(x) is(x, "units"), drop_units)
  
  Ad1 = A_demand(C_chl, pars1, unitless = FALSE)
  Ad2 = A_demand(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(drop_units(Ad1), Ad2)
  
  As1 = A_supply(C_chl, pars1, unitless = FALSE, use_legacy_version = FALSE)
  As2 = A_supply(drop_units(C_chl), pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(drop_units(As1), As2)
  
  gbc1 = drop_units(.get_gbc(pars1, "lower", unitless = FALSE, use_legacy_version = FALSE))
  gbc2 = .get_gbc(pars2, "lower", unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gbc1, gbc2)
  
  gbc3 = drop_units(.get_gbc(pars1, "upper", unitless = FALSE, use_legacy_version = FALSE))
  gbc4 = .get_gbc(pars2, "upper", unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gbc3, gbc4)
  
  gmc1 = drop_units(.get_gmc(pars1, "lower", unitless = FALSE))
  gmc2 = .get_gmc(pars2, "lower", unitless = TRUE)
  expect_equal(gmc1, gmc2)
  
  gmc3 = drop_units(.get_gmc(pars1, "upper", unitless = FALSE))
  gmc4 = .get_gmc(pars2, "upper", unitless = TRUE)
  expect_equal(gmc3, gmc4)
  
  gsc1 = drop_units(.get_gsc(pars1, "lower", unitless = FALSE))
  gsc2 = .get_gsc(pars2, "lower", unitless = TRUE)
  expect_equal(gsc1, gsc2)
  
  gsc3 = drop_units(.get_gsc(pars1, "upper", unitless = FALSE))
  gsc4 = .get_gsc(pars2, "upper", unitless = TRUE)
  expect_equal(gsc3, gsc4)
  
  gtc1 = drop_units(.get_gtc(pars1, unitless = FALSE, use_legacy_version = FALSE))
  gtc2 = .get_gtc(pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gtc1, gtc2)
  
  gtc3 = drop_units(.get_gtc(pars1, unitless = FALSE, use_legacy_version = FALSE))
  gtc4 = .get_gtc(pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gtc3, gtc4)
  
  guc1 = drop_units(.get_guc(pars1, "lower", unitless = FALSE))
  guc2 = .get_guc(pars2, "lower", unitless = TRUE)
  expect_equal(guc1, guc2)
  
  guc3 = drop_units(.get_guc(pars1, "upper", unitless = FALSE))
  guc4 = .get_guc(pars2, "upper", unitless = TRUE)
  expect_equal(guc3, guc4)
  
  FvCB1 = FvCB(C_chl, pars1, unitless = FALSE)
  FvCB2 = FvCB(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(drop_units(FvCB1$A), FvCB2$A)
  
  J1 = drop_units(J(pars1, unitless = FALSE))
  J2 = J(pars2, unitless = TRUE)
  expect_equal(J1, J2)
  
  Wc1 = drop_units(W_carbox(C_chl, pars1, unitless = FALSE))
  Wc2 = W_carbox(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wc1, Wc2)
  
  Wr1 = drop_units(W_regen(C_chl, pars1, unitless = FALSE))
  Wr2 = W_regen(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wr1, Wr2)
  
  Wt1 = drop_units(W_tpu(C_chl, pars1, unitless = FALSE))
  Wt2 = W_tpu(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wt1, Wt2)
})

test_that("unitless values match unit-ed values (tealeaves, new g_tc)", {
  
  T_leaf = NA
  while (is.na(T_leaf)) {
    cs1 = make_constants(use_tealeaves = TRUE)
    
    lp1 = leaf_par(list(
      A_mes_A = set_units(runif(1, 5, 40), 1), 
      abs_l = set_units(runif(1), 1),
      abs_s = set_units(runif(1), 1),
      delta_ias_lower = set_units(runif(1, 0, 1000), um), 
      delta_ias_upper = set_units(runif(1, 0, 1000), um), 
      g_liqc25 = set_units(runif(1, 0, 0.1), mol/m^2/s),
      g_mc25 = set_units(runif(1), mol/m^2/s),
      g_sc = set_units(runif(1), mol/m^2/s),
      g_uc = set_units(runif(1), mol/m^2/s),
      gamma_star25 = set_units(runif(1, 30, 50), umol/mol),
      J_max25 = set_units(runif(1, 50, 200), umol/m^2/s),
      K_C25 = set_units(268.3, umol/mol),
      K_O25 = set_units(165084.2, umol/mol),
      k_mc = set_units(runif(1, 0.01, 100)),
      k_sc = set_units(runif(1, 0.01, 100)),
      k_uc = set_units(runif(1, 0.01, 100)),
      leafsize = set_units(runif(1), m),
      phi_J = set_units(0.331),
      R_d25 = set_units(runif(1, 0, 5), umol/m^2/s),
      theta_J = set_units(0.825),
      V_cmax25 = set_units(runif(1, 50, 200), umol/m^2/s),
      V_tpu25 = set_units(runif(1, 50, 200), umol/m^2/s)
    ), use_tealeaves = TRUE)
    
    ep1 = enviro_par(list(
      C_air = set_units(runif(1, 10, 2000), umol/mol),
      E_q = set_units(220, kJ/mol),
      f_par = set_units(runif(1), 1),
      O = set_units(0.21, mol/mol),
      P = set_units(101.3246, kPa),
      r = set_units(runif(1), 1),
      RH = set_units(runif(1)),
      PPFD = set_units(runif(1, 0, 2000), umol/m^2/s),
      T_air = set_units(runif(1, 273.15, 313.15), K),
      T_sky = photosynthesis:::get_f_parameter("T_sky"),
      wind = set_units(runif(1, 0, 20), m/s)
    ), use_tealeaves = TRUE)
    
    bp1 = make_bakepar()
    
    lp1 = photosynthesis:::add_Tleaf_photo(lp1, ep1, cs1, TRUE)
    T_leaf = lp1$T_leaf
  }
  
  lp1 %<>% bake(ep1, bp1, cs1, assert_units = TRUE)
  
  # Drop units
  cs2 = purrr::map_if(cs1, function(x) is(x, "units"), drop_units)
  lp2 = purrr::map_if(lp1, function(x) is(x, "units"), drop_units)
  ep2 = purrr::map_if(ep1, function(x) is(x, "units"), drop_units)
  bp2 = purrr::map_if(bp1, ~ is(.x, "units"), drop_units)
  
  lp2 %<>% bake(ep2, bp2, cs2, assert_units = FALSE)
  
  purrr::map2(lp1, lp2[names(lp1)], function(.x, .y) {
    dplyr::near(drop_units(.x), drop_units(.y))
  }) %>%
    purrr::map(expect_true)
  
  C_chl = set_units(runif(1, 200, 400), umol/mol)
  pars1 = c(cs1, lp1, ep1)
  pars2 = purrr::map_if(pars1, function(x) is(x, "units"), drop_units)
  
  Ad1 = A_demand(C_chl, pars1, unitless = FALSE)
  Ad2 = A_demand(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(drop_units(Ad1), Ad2)
  
  As1 = A_supply(C_chl, pars1, unitless = FALSE, use_legacy_version = FALSE)
  As2 = A_supply(drop_units(C_chl), pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(drop_units(As1), As2)
  
  gbc1 = drop_units(.get_gbc(pars1, "lower", unitless = FALSE, use_legacy_version = FALSE))
  gbc2 = .get_gbc(pars2, "lower", unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gbc1, gbc2)
  
  gbc3 = drop_units(.get_gbc(pars1, "upper", unitless = FALSE, use_legacy_version = FALSE))
  gbc4 = .get_gbc(pars2, "upper", unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gbc3, gbc4)
  
  gmc1 = drop_units(.get_gmc(pars1, "lower", unitless = FALSE))
  gmc2 = .get_gmc(pars2, "lower", unitless = TRUE)
  expect_equal(gmc1, gmc2)
  
  gmc3 = drop_units(.get_gmc(pars1, "upper", unitless = FALSE))
  gmc4 = .get_gmc(pars2, "upper", unitless = TRUE)
  expect_equal(gmc3, gmc4)
  
  gsc1 = drop_units(.get_gsc(pars1, "lower", unitless = FALSE))
  gsc2 = .get_gsc(pars2, "lower", unitless = TRUE)
  expect_equal(gsc1, gsc2)
  
  gsc3 = drop_units(.get_gsc(pars1, "upper", unitless = FALSE))
  gsc4 = .get_gsc(pars2, "upper", unitless = TRUE)
  expect_equal(gsc3, gsc4)
  
  gtc1 = drop_units(.get_gtc(pars1, unitless = FALSE, use_legacy_version = FALSE))
  gtc2 = .get_gtc(pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gtc1, gtc2)
  
  gtc3 = drop_units(.get_gtc(pars1, unitless = FALSE, use_legacy_version = FALSE))
  gtc4 = .get_gtc(pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gtc3, gtc4)
  
  guc1 = drop_units(.get_guc(pars1, "lower", unitless = FALSE))
  guc2 = .get_guc(pars2, "lower", unitless = TRUE)
  expect_equal(guc1, guc2)
  
  guc3 = drop_units(.get_guc(pars1, "upper", unitless = FALSE))
  guc4 = .get_guc(pars2, "upper", unitless = TRUE)
  expect_equal(guc3, guc4)
  
  FvCB1 = FvCB(C_chl, pars1, unitless = FALSE)
  FvCB2 = FvCB(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(drop_units(FvCB1$A), FvCB2$A)
  
  J1 = drop_units(J(pars1, unitless = FALSE))
  J2 = J(pars2, unitless = TRUE)
  expect_equal(J1, J2)
  
  Wc1 = drop_units(W_carbox(C_chl, pars1, unitless = FALSE))
  Wc2 = W_carbox(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wc1, Wc2)
  
  Wr1 = drop_units(W_regen(C_chl, pars1, unitless = FALSE))
  Wr2 = W_regen(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wr1, Wr2)
  
  Wt1 = drop_units(W_tpu(C_chl, pars1, unitless = FALSE))
  Wt2 = W_tpu(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wt1, Wt2)
})

test_that("unitless values match unit-ed values (tealeaves, old g_tc)", {
  
  T_leaf = NA
  while (is.na(T_leaf)) {
    cs1 = make_constants(use_tealeaves = TRUE)
    
    lp1 = leaf_par(list(
      A_mes_A = set_units(numeric(0), 1), 
      abs_l = set_units(runif(1), 1),
      abs_s = set_units(runif(1), 1),
      delta_ias_lower = set_units(numeric(0), um), 
      delta_ias_upper = set_units(numeric(0), um), 
      g_liqc25 = set_units(numeric(0), mol/m^2/s),
      g_mc25 = set_units(runif(1), mol/m^2/s),
      g_sc = set_units(runif(1), mol/m^2/s),
      g_uc = set_units(runif(1), mol/m^2/s),
      gamma_star25 = set_units(runif(1, 30, 50), umol/mol),
      J_max25 = set_units(runif(1, 50, 200), umol/m^2/s),
      K_C25 = set_units(268.3, umol/mol),
      K_O25 = set_units(165084.2, umol/mol),
      k_mc = set_units(runif(1, 0.01, 100)),
      k_sc = set_units(runif(1, 0.01, 100)),
      k_uc = set_units(runif(1, 0.01, 100)),
      leafsize = set_units(runif(1), m),
      phi_J = set_units(0.331),
      R_d25 = set_units(runif(1, 0, 5), umol/m^2/s),
      theta_J = set_units(0.825),
      V_cmax25 = set_units(runif(1, 50, 200), umol/m^2/s),
      V_tpu25 = set_units(runif(1, 50, 200), umol/m^2/s)
    ), use_tealeaves = TRUE)
    
    ep1 = enviro_par(list(
      C_air = set_units(runif(1, 10, 2000), umol/mol),
      E_q = set_units(220, kJ/mol),
      f_par = set_units(runif(1), 1),
      O = set_units(0.21, mol/mol),
      P = set_units(101.3246, kPa),
      r = set_units(runif(1), 1),
      RH = set_units(runif(1)),
      PPFD = set_units(runif(1, 0, 2000), umol/m^2/s),
      T_air = set_units(runif(1, 273.15, 313.15), K),
      T_sky = photosynthesis:::get_f_parameter("T_sky"),
      wind = set_units(runif(1, 0, 20), m/s)
    ), use_tealeaves = TRUE)
    
    bp1 = make_bakepar()
    
    lp1 = photosynthesis:::add_Tleaf_photo(lp1, ep1, cs1, TRUE)
    T_leaf = lp1$T_leaf
  }
  
  lp1 %<>% bake(ep1, bp1, cs1, assert_units = TRUE)
  
  # Drop units
  cs2 = purrr::map_if(cs1, function(x) is(x, "units"), drop_units)
  lp2 = purrr::map_if(lp1, function(x) is(x, "units"), drop_units)
  ep2 = purrr::map_if(ep1, function(x) is(x, "units"), drop_units)
  bp2 = purrr::map_if(bp1, ~ is(.x, "units"), drop_units)
  
  lp2 %<>% bake(ep2, bp2, cs2, assert_units = FALSE)
  
  purrr::map2(lp1, lp2[names(lp1)], function(.x, .y) {
    if (length(.x) == 0) {TRUE} else {dplyr::near(drop_units(.x), drop_units(.y))}
  }) |>
    purrr::map(expect_true)
  
  C_chl = set_units(runif(1, 200, 400), umol/mol)
  pars1 = c(cs1, lp1, ep1)
  pars2 = purrr::map_if(pars1, function(x) is(x, "units"), drop_units)
  
  Ad1 = A_demand(C_chl, pars1, unitless = FALSE)
  Ad2 = A_demand(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(drop_units(Ad1), Ad2)
  
  As1 = A_supply(C_chl, pars1, unitless = FALSE, use_legacy_version = FALSE)
  As2 = A_supply(drop_units(C_chl), pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(drop_units(As1), As2)
  
  gbc1 = drop_units(.get_gbc(pars1, "lower", unitless = FALSE, use_legacy_version = FALSE))
  gbc2 = .get_gbc(pars2, "lower", unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gbc1, gbc2)
  
  gbc3 = drop_units(.get_gbc(pars1, "upper", unitless = FALSE, use_legacy_version = FALSE))
  gbc4 = .get_gbc(pars2, "upper", unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gbc3, gbc4)
  
  gmc1 = drop_units(.get_gmc(pars1, "lower", unitless = FALSE))
  gmc2 = .get_gmc(pars2, "lower", unitless = TRUE)
  expect_equal(gmc1, gmc2)
  
  gmc3 = drop_units(.get_gmc(pars1, "upper", unitless = FALSE))
  gmc4 = .get_gmc(pars2, "upper", unitless = TRUE)
  expect_equal(gmc3, gmc4)
  
  gsc1 = drop_units(.get_gsc(pars1, "lower", unitless = FALSE))
  gsc2 = .get_gsc(pars2, "lower", unitless = TRUE)
  expect_equal(gsc1, gsc2)
  
  gsc3 = drop_units(.get_gsc(pars1, "upper", unitless = FALSE))
  gsc4 = .get_gsc(pars2, "upper", unitless = TRUE)
  expect_equal(gsc3, gsc4)
  
  gtc1 = drop_units(.get_gtc(pars1, unitless = FALSE, use_legacy_version = FALSE))
  gtc2 = .get_gtc(pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gtc1, gtc2)
  
  gtc3 = drop_units(.get_gtc(pars1, unitless = FALSE, use_legacy_version = FALSE))
  gtc4 = .get_gtc(pars2, unitless = TRUE, use_legacy_version = FALSE)
  expect_equal(gtc3, gtc4)
  
  guc1 = drop_units(.get_guc(pars1, "lower", unitless = FALSE))
  guc2 = .get_guc(pars2, "lower", unitless = TRUE)
  expect_equal(guc1, guc2)
  
  guc3 = drop_units(.get_guc(pars1, "upper", unitless = FALSE))
  guc4 = .get_guc(pars2, "upper", unitless = TRUE)
  expect_equal(guc3, guc4)
  
  FvCB1 = FvCB(C_chl, pars1, unitless = FALSE)
  FvCB2 = FvCB(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(drop_units(FvCB1$A), FvCB2$A)
  
  J1 = drop_units(J(pars1, unitless = FALSE))
  J2 = J(pars2, unitless = TRUE)
  expect_equal(J1, J2)
  
  Wc1 = drop_units(W_carbox(C_chl, pars1, unitless = FALSE))
  Wc2 = W_carbox(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wc1, Wc2)
  
  Wr1 = drop_units(W_regen(C_chl, pars1, unitless = FALSE))
  Wr2 = W_regen(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wr1, Wr2)
  
  Wt1 = drop_units(W_tpu(C_chl, pars1, unitless = FALSE))
  Wt2 = W_tpu(drop_units(C_chl), pars2, unitless = TRUE)
  expect_equal(Wt1, Wt2)
})
