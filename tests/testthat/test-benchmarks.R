context("benchmarks")
library(magrittr)
library(photosynthesis)

test_that("benchmarks haven't changed", {
  
  cs <- make_constants()
  lp <- make_leafpar()
  ep <- make_enviropar()
  bp <- make_bakepar()
  
  ph <- photo(lp, ep, bp, cs)
  expect_equal(round(ph$A, 5), set_units(27.67919, umol/m^2/s))
  
  # # Several parameters vary; others are fixed to current defaults
  # p <- tidyr::crossing(
  #   
  #   # constants
  #   D_c0 = set_units(1.29e-05, m^2/s),
  #   D_h0 = set_units(1.9e-05, m^2/s),
  #   D_m0 = set_units(1.33e-05, m^2/s),
  #   epsilon = set_units(0.622),
  #   eT = set_units(1.75),
  #   G = set_units(9.8, m/s^2),
  #   R = set_units(8.31446, J/K/mol),
  #   s = set_units(5.67e-08, W/K^4/m^2),
  #   
  #   # bake parameters
  #   Ds_gmc = set_units(487.29, J/K/mol),
  #   Ds_Jmax = set_units(388.04, J/K/mol),
  #   Ea_gammastar = set_units(24459.97, J/mol),
  #   Ea_gmc = set_units(68901.56, J/mol),
  #   Ea_Jmax = set_units(56095.18, J/mol),
  #   Ea_KC = set_units(80989.78, J/mol),
  #   Ea_KO = set_units(23719.97, J/mol),
  #   Ea_Rd = set_units(40446.75, J/mol),
  #   Ea_Vcmax = set_units(52245.78, J/mol),
  #   Ea_Vtpu = set_units(52245.78, J/mol),
  #   Ed_gmc = set_units(148788.6, J/mol),
  #   Ed_Jmax = set_units(121244.8, J/mol),
  # 
  #   # leaf parameters
  #   g_mc25 = set_units(10 ^ seq(-1, 1, 0.2), umol/m^2/s/Pa),
  #   g_sc = set_units(10 ^ seq(-1, 1, 0.2), umol/m^2/s/Pa),
  #   g_uc = set_units(0.1, umol/m^2/s/Pa),
  #   gamma_star25 = set_units(3.743, Pa),
  #   J_max25 = set_units(c(100, 150, 200), umol/m^2/s),
  #   K_C25 = set_units(27.238, Pa),
  #   K_O25 = set_units(16.582, kPa),
  #   k_mc = set_units(1),
  #   k_sc = set_units(c(0.1, 1, 10)),
  #   k_uc = set_units(1),
  #   leafsize = set_units(c(0.004, 0.04, 0.4), m),
  #   phi_J = set_units(0.331),
  #   R_d25 = set_units(2, umol/m^2/s),
  #   T_leaf = set_units(seq(273.15, 313.15, 10), K),
  #   theta_J = set_units(0.825),
  #   V_cmax25 = set_units(c(50, 75, 150), umol/m^2/s),
  #   V_tpu25 = set_units(200, umol/m^2/s),
  #   
  #   # environmental parameters
  #   C_air = set_units(c(10, 40, 100), Pa),
  #   O = set_units(21.27565, kPa),
  #   P = set_units(101.3246, kPa),
  #   PPFD = set_units(c(100, 500, 1500), umol/m^2/s),
  #   RH = set_units(0.5),
  #   wind = set_units(c(0.01, 0.1, 1, 10), m/s)
  #   
  # )
  # 
  # p$A <- numeric(nrow(p))
  # p$row <- 1:nrow(p)
  # 
  # # Create benchmarks (last updated: 2019-04-30)
  # safely_photo <- purrr::safely(photo)
  # set.seed(518661640)
  # p <- p[sample(nrow(p), 1e3), ]
  # 
  # for (i in 1:nrow(p)) {
  #   
  #   # Set variable values
  #   lp$g_mc25 <- set_units(p$g_mc25[i], umol/m^2/s/Pa)
  #   lp$g_sc <- set_units(p$g_sc[i], umol/m^2/s/Pa)
  #   lp$J_max25 <- set_units(p$J_max25[i], umol/m^2/s)
  #   lp$leafsize <- set_units(p$leafsize[i], m)
  #   lp$T_leaf <- set_units(p$T_leaf[i], K)
  #   lp$V_cmax25 <- set_units(p$V_cmax25[i], umol/m^2/s)
  #   ep$C_air <- set_units(p$C_air[i], Pa)
  #   ep$PPFD <- set_units(p$PPFD[i], umol/m^2/s)
  #   ep$T_air <- lp$T_leaf
  #   ep$wind <- set_units(p$wind[i], m/s)
  #   
  #   ph <- safely_photo(lp, ep, bp, cs, quiet = TRUE)
  #   if (is.null(ph$result)) {
  #     p <- p[-i, ]
  #   } else {
  #     p$A[i] <- ph$result$A
  #   }
  # }
  # 
  # readr::write_csv(p, "tests/testthat/benchmarks.csv")
  
  p <- read.csv("benchmarks.csv")
  for (i in sample(nrow(p), 5)) {

    # Set variable values
    lp$g_mc25 <- set_units(p$g_mc25[i], umol/m^2/s/Pa)
    lp$g_sc <- set_units(p$g_sc[i], umol/m^2/s/Pa)
    lp$J_max25 <- set_units(p$J_max25[i], umol/m^2/s)
    lp$leafsize <- set_units(p$leafsize[i], m)
    lp$T_leaf <- set_units(p$T_leaf[i], K)
    lp$V_cmax25 <- set_units(p$V_cmax25[i], umol/m^2/s)
    ep$C_air <- set_units(p$C_air[i], Pa)
    ep$PPFD <- set_units(p$PPFD[i], umol/m^2/s)
    ep$T_air <- lp$T_leaf
    ep$wind <- set_units(p$wind[i], m/s)
    
    ph <- photo(lp, ep, bp, cs, quiet = TRUE)
    expect_equal(round(ph$A, 3), set_units(round(p$A[i], 3), umol/m^2/s))
  }
  
})
