#' \code{photosynthesis}: model C3 photosynthesis
#' 
#' @param leaf_par A list of leaf parameters. This can be generated using the \code{make_leafpar} function.
#' 
#' @param enviro_par A list of environmental parameters. This can be generated using the \code{make_enviropar} function.
#' 
#' @param constants A list of physical constants. This can be generated using the \code{make_constants} function.
#' 
#' @param progress Logical. Should a progress bar be displayed?
#' 
#' @param quiet Logical. Should messages be displayed?
#' 
#' @return 
#' 
#' \code{photosynthesis}: \cr
#' \cr
#' A data.frame (more information coming soon!)
#' \cr
#' 
#' @examples 
#' 
#' leaf_par <- make_leafpar()
#' enviro_par <- make_enviropar()
#' constants <- make_constants()
#' ph <- photo(leaf_par, enviro_par, constants)
#' # ph
#' 
#' @export
#' 

photosynthesis <- function(leaf_par, enviro_par, constants, progress = TRUE, 
                           quiet = FALSE) {
  
  pars <- c(leaf_par, enviro_par)
  par_units <- purrr::map(pars, units) %>%
    magrittr::set_names(names(pars))
  
  pars %<>%
    names() %>%
    glue::glue("{x} = pars${x}", x = .) %>%
    stringr::str_c(collapse = ", ") %>%
    glue::glue("tidyr::crossing({x})", x = .) %>%
    parse(text = .) %>%
    eval() %>%
    purrr::transpose()
  
  tidyr::crossing(i = seq_len(length(pars)),
                  par = names(pars[[1]])) %>%
    dplyr::transmute(ex = glue::glue("units(pars[[{i}]]${par}) <<- par_units${par}", 
                                     i = .data$i, par = .data$par)) %>%
    dplyr::pull("ex") %>%
    parse(text = .) %>%
    eval()
  
  if (!quiet) {
    glue::glue("\nSolving for photosynthetic rate from {n} parameter set{s}...", 
               n = length(pars), s = dplyr::if_else(length(pars) > 1, "s", "")) %>%
      crayon::green() %>%
      message(appendLF = FALSE)
  }
  
  if (progress) pb <- dplyr::progress_estimated(length(pars))
  
  soln <- pars %>%
    purrr::map_dfr(~{
      
      ret <- photo(leaf_par(.x), enviro_par(.x), constants, progress = FALSE, 
                   quiet = TRUE)
      if (progress) pb$tick()$print()
      ret
      
    })
  
  pars %<>% purrr::map_dfr(purrr::flatten_dfr)
  
  colnames(pars) %>%
    glue::glue("units(pars${x}) <<- par_units${x}", x = .) %>%
    parse(text = .) %>%
    eval()
  
  pars %<>% dplyr::bind_cols(soln)
  
  # units(pars$T_leaf) <- "K"
  # units(pars$R_abs) <- "W/m^2"
  # units(pars$S_r) <- "W/m^2"
  # units(pars$H) <- "W/m^2"
  # units(pars$L) <- "W/m^2"
  
  pars
  
}

#' \code{photo}: find leaf temperatures for a single parameter set
#' @rdname photosynthesis
#' @export

photo <- function(leaf_par, enviro_par, constants, progress = TRUE, 
                  quiet = FALSE) {
  
  # ??? -----
  enviro_par$T_leaf %<>% set_units("K") # convert T_leaf to Kelvin before dropping units
  # init <- ???
  soln <- set_units(1)
  
  # if (progress) pb <- dplyr::progress_estimated(n_start)
  # 
  # soln <- seq(from = init - 30, to = init + 30, length.out = n_start + 2) %>%
  #   magrittr::extract(2:(n_start + 1)) %>%
  #   purrr::map_dfr(~{
  #     
  #     ret <- stats::optim(.x, fn = energy_balance, leaf_par = leaf_par,
  #                         enviro_par = enviro_par, constants = constants,
  #                         abs_val = TRUE, quiet = TRUE, method = "Brent",
  #                         lower = drop_units(enviro_par$T_air - set_units(30, "K")),
  #                         upper = drop_units(enviro_par$T_air + set_units(30, "K")))
  #     if (progress) pb$tick()$print()
  #     
  #     data.frame(T_leaf = ret$par, init = .x, value = ret$value, 
  #                convergence = ret$convergence)
  #     
  #   })
  
  # Check results -----
  # if (!any(soln$convergence == 0)) {
  #   "The solver did not converge for any of the initial starting value. Inspect results carefully" %>%
  #     crayon::red() %>%
  #     message()
  # }
  
  # soln %<>% dplyr::filter(.data$convergence == 0)
  
  # ??? -----
  # components <- suppressWarnings(
  #   soln %>%
  #     dplyr::pull("T_leaf") %>%
  #     purrr::map_dfr(~{
  #       .x %<>% set_units("K")
  #       ret <- energy_balance(.x, leaf_par = leaf_par, enviro_par = enviro_par, 
  #                             constants = constants, quiet = TRUE, components = TRUE)
  #       ret$components
  #     })
  # )
  # 
  # soln %<>% dplyr::bind_cols(components)
  
  # Return -----
  soln
  
}

#' CO2 supply and demand function (mol / m^2 s)
#' 
#' @param C_air Atmospheric CO2 concentration in Pa of class \code{units}
#' @param C_chl Chloroplastic CO2 concentration in Pa of class \code{units}
#' @param gamma_star Chloroplastic CO2 compensation point in Pa of class \code{units}
#' @param g_tc Total conductance to CO2 in \eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) of class \code{units}
#' @param R_d Mitochondrial respiration in \eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) of class \code{units}
#' 
#' @return Value in mol / (m^2 s) of class \code{units}
#' 
#' @details 
#' 
#' \bold{Supply function:}
#' \cr
#' \deqn{A = g_\mathrm{tc} (C_\mathrm{air} - C_\mathrm{chl})}{A = g_tc (C_air - C_chl)}
#' 
#' \bold{Demand function:}
#' \cr
#' \deqn{? = A (1 - \Gamma* / C_\mathrm{chl}) - R_\mathrm{d}}{? = A (1 - \Gamma* / C_chl) - R_d}
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{A} \tab \code{A} \tab photosynthetic rate \tab \eqn{\mu}mol CO2 / (m^2 s) \tab calculated \cr
#' \eqn{g_\mathrm{tc}}{g_tc} \tab \code{g_tc} \tab total conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab \link[=.get_gtc]{calculated} \cr
#' \eqn{C_\text{air}}{C_air} \tab \code{C_air} \tab atmospheric CO2 concentration \tab Pa \tab 41 \cr
#' \eqn{C_\text{chl}}{C_chl} \tab \code{C_chl} \tab chloroplastic CO2 concentration \tab Pa \tab calculated\cr
#' \eqn{R_\text{d}}{R_d} \tab \code{R_d} \tab Mitochondrial respiration \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab 2 \cr
#' \eqn{\Gamma*} \tab \code{gamma_star} \tab Chloroplastic CO2 compensation point \tab Pa \tab 3.743
#' }

As <- function(C_air, C_chl, g_tc, gamma_star, R_d) {
  
  set_units(g_tc * (C_air - C_chl), "umol/m^2/s")
  
}

#' Total conductance to CO2 (umol / (m^2 s Pa))
#' 
#' @param pars Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})
#' 
#' @return Value in umol / (m^2 s Pa) of class \code{units}
#' 
#' @details 
#' 
#' Total conductance to CO2 is the sum of parallel conductances on the lower (\eqn{gc_\mathrm{lower}}{gc_lower}) and upper (\eqn{gc_\mathrm{upper}}{gc_upper}) leaf surfaces that are in series with intercellular conductance ((\eqn{g_\mathrm{ic}}{g_ic})):
#' 
#' \deqn{g_\mathrm{tc} = 1 / ((1 / g_\mathrm{ic}) + (1 / (gc_\mathrm{upper} + gc_\mathrm{lower})))}{g_tc = 1 / ((1 / g_ic) + (1 / (gc_upper + gc_lower)))}
#' 
#' The upper and lower conductances are determined by the intercellular (\eqn{g_\mathrm{xc}}{g_xc}) and stomatal (\eqn{g_\mathrm{sc}}{g_sc}) conductances in series. For a naming convenction, I assume that the lower surface is abaxial and conists of spongy mesophyll; the upper surface is adaxial and consists of palisade mesophyll:
#' 
#' \deqn{gc_\mathrm{lower} = 1 / ((1 / gsc_\mathrm{abaxial}) + (1 / gxc_\mathrm{spongy}))}{gc_lower = 1 / ((1 / gsc_abaxial) + (1 / gxc_spongy))}
#' \deqn{gc_\mathrm{upper} = 1 / ((1 / gsc_\mathrm{adaxial}) + (1 / gxc_\mathrm{palisade}))}{gc_upper = 1 / ((1 / gsc_adaxial) + (1 / gxc_palisade))}
#' 
#' The total stomatal and intercellular conductances are partitioned between abaxial/adaxial and spongy/palisade, respectively, according to two partitioning facotrs \eqn{k_\mathrm{sc}{k_sc} and \eqn{k_\mathrm{xc}{k_xc}:
#' 
#' \deqn{gsc_\mathrm{abaxial} = g_\mathrm{sc} (1 / (1 + k_\mathrm{sc}))}{gsc_abaxial = g_sc (1 / (1 + k_sc))}
#' \deqn{gsc_\mathrm{adaxial} = g_\mathrm{sc} (k_\mathrm{sc} / (1 + k_\mathrm{sc}))}{gsc_adaxial = g_sc (k_sc / (1 + k_sc))}
#' \deqn{gxc_\mathrm{spongy} = g_\mathrm{xc} (1 / (1 + k_\mathrm{xc}))}{gxc_\mathrm{spongy} = g_\mathrm{xc} (1 / (1 + k_\mathrm{xc}))}
#' \deqn{gxc_\mathrm{palisade} = g_\mathrm{xc} (k_\mathrm{xc} / (1 + k_\mathrm{xc}))}{gxc_palisade = g_xc (k_xc / (1 + k_xc))}
#' 
#' 
#' \tabular{lllll}{
#' \emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
#' \eqn{g_\text{sc}}{g_sc} \tab \code{g_sc} \tab stomatal conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab 5\cr
#' \eqn{g_\text{xc}}{g_xc} \tab \code{g_xc} \tab intercellular conductance to CO2 \tab (\eqn{\mu}mol CO2) / (m\eqn{^2} s Pa) \tab 10 \cr
#' \eqn{g_\text{ic}}{g_ic} \tab \code{g_ic} \tab intracellular conductance to CO2 \tab (mol CO2) / (m\eqn{^2} s) \tab 10 \cr
#' \eqn{k\mathrm{xs}{k_sc} \tab \code{k_sc} \tab partition of \eqn{g_\text{sc}}{g_sc} to abaxial (lower) surface \tab none \tab 1 \cr
#' \eqn{k\mathrm{xc}{k_xc} \tab \code{k_xc} \tab partition of \eqn{g_\text{xc}}{g_xc} to spongy mesophyll \tab none \tab 1
#' }
#' 

.get_gtc <- function(pars) {
  
  gsc_adaxial <- pars$g_sc * (pars$k_sc / (set_units(1) + pars$k_sc))
  gsc_abaxial <- pars$g_sc * (set_units(1) / (set_units(1) + pars$k_sc))
  gxc_palisade <- pars$g_xc * (pars$k_xc / (set_units(1) + pars$k_xc))
  gxc_spongy <- pars$g_xc * (set_units(1) / (set_units(1) + pars$k_xc))
  gc_lower <- set_units(1) / ((set_units(1) / gsc_abaxial) + (set_units(1) / gxc_spongy))
  gc_upper <- set_units(1) / ((set_units(1) / gsc_adaxial) + (set_units(1) / gxc_palisade))
  g_tc <- set_units(1) / ((set_units(1) / pars$g_ic) + (set_units(1) / (gc_lower + gc_upper)))
  g_tc %<>% set_units(g_tc, "umol/m^2/s/Pa")
  
  g_tc
  
}

#' Ad
#' @rdname As
#' @export

Ad <- function(gamma_star, R_d) {
  
  NULL
  
}

