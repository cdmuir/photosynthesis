% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FvCB.R
\name{FvCB}
\alias{FvCB}
\alias{W_carbox}
\alias{W_regen}
\alias{W_tpu}
\title{Farquhar-von Caemmerer-Berry (FvCB) C3 photosynthesis model}
\usage{
FvCB(C_chl, pars, unitless = FALSE)

W_carbox(C_chl, pars, unitless = FALSE)

W_regen(C_chl, pars, unitless = FALSE)

W_tpu(C_chl, pars, unitless = FALSE)
}
\arguments{
\item{C_chl}{Chloroplastic CO2 concentration in Pa of class \code{units}}

\item{pars}{Concatenated parameters (\code{leaf_par}, \code{enviro_par}, and \code{constants})}

\item{unitless}{Logical. Should \code{units} be set? The function is faster when FALSE, but input must be in correct units or else results will be incorrect without any warning.}
}
\value{
A list of four values with units umol CO2 / (m^2 s) of class \code{units}:
\cr
\itemize{
\item \code{W_carbox}: Rubisco-limited assimilation rate \cr
\item \code{W_regen}: RuBP regeneration-limited assimilation rate \cr
\item \code{W_tpu}: TPU-limited assimilation rate \cr
\item \code{A}: minimum of W_carbox, W_regen, and W_tpu
}
}
\description{
Farquhar-von Caemmerer-Berry (FvCB) C3 photosynthesis model

Rubisco-limited assimilation rate

RuBP regeneration-limited assimilation rate

TPU-limited assimilation rate
}
\details{
Equations following Buckley and Diaz-Espejo (2015): \cr
\cr
\strong{Rubisco-limited assimilation rate:} \cr
\cr
\deqn{W_\mathrm{carbox} = V_\mathrm{c,max} C_\mathrm{chl} / (C_\mathrm{chl} + K_\mathrm{m})}{W_carbox = V_cmax C_chl / (C_chl + K_m)}

where:

\deqn{K_\mathrm{m} = K_\mathrm{C} (1 + O / K_\mathrm{O})}{K_m = K_c (1 + O / K_o)}

\strong{RuBP regeneration-limited assimilation rate:} \cr
\cr
\deqn{W_\mathrm{regen} = J C_\mathrm{chl} / (4 C_\mathrm{chl} + 8 \Gamma*)}{W_regen = J C_chl / (4 C_chl + 8 \Gamma*)}

where \eqn{J} is a function of PPFD, obtained by solving the equation:

\deqn{0 = \theta_J J ^ 2 - J (J_\mathrm{max} + \phi_J PPFD) + J_\mathrm{max} \phi_J PPFD}{0 = \theta_J J ^ 2 - J (J_max + \phi_J PPFD) + J_max \phi_J PPFD}

\strong{TPU-limited assimilation rate:} \cr

\deqn{W_\mathrm{tpu} = 3 V_\mathrm{tpu} C_\mathrm{chl} / (C_\mathrm{chl} - \Gamma*)}{W_tpu = 3 V_tpu C_chl / (C_chl - \Gamma*)}
\cr
\tabular{lllll}{
\emph{Symbol} \tab \emph{R} \tab \emph{Description} \tab \emph{Units} \tab \emph{Default}\cr
\eqn{C_\mathrm{chl}}{C_chl} \tab \code{C_chl} \tab chloroplastic CO2 concentration \tab Pa \tab input \cr
\eqn{\Gamma*} \tab \code{gamma_star} \tab chloroplastic CO2 compensation point (T_leaf) \tab Pa \tab \link[=bake]{calculated} \cr
\eqn{J_\mathrm{max}}{J_max} \tab \code{J_max} \tab potential electron transport (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
\eqn{K_\mathrm{C}}{K_C} \tab \code{K_C} \tab Michaelis constant for carboxylation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=bake]{calculated} \cr
\eqn{K_\mathrm{O}}{K_O} \tab \code{K_O} \tab Michaelis constant for oxygenation (T_leaf) \tab \eqn{\mu}mol / mol \tab \link[=bake]{calculated} \cr
\eqn{O} \tab \code{O} \tab atmospheric O2 concentration \tab kPa \tab 21.27565 \cr
\eqn{\phi_J} \tab \code{phi_J} \tab initial slope of the response of J to PPFD \tab none \tab 0.331 \cr
PPFD \tab \code{PPFD} \tab photosynthetic photon flux density \tab umol quanta / (m^2 s) \tab 1500 \cr
\eqn{R_\mathrm{d}}{R_d} \tab \code{R_d} \tab nonphotorespiratory CO2 release (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
\eqn{\theta_J} \tab \code{theta_J} \tab curvature factor for light-response curve \tab none \tab 0.825 \cr
\eqn{V_\mathrm{c,max}}{V_c,max} \tab \code{V_cmax} \tab maximum rate of carboxylation (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated} \cr
\eqn{V_\mathrm{tpu}}{V_tpu} \tab \code{V_tpu} \tab rate of triose phosphate utilization (T_leaf) \tab \eqn{\mu}mol CO2 / (m\eqn{^2} s) \tab \link[=bake]{calculated}
}
}
\examples{
bake_par = make_bakepar()
constants = make_constants(use_tealeaves = FALSE)
enviro_par = make_enviropar(use_tealeaves = FALSE)
leaf_par = make_leafpar(use_tealeaves = FALSE)
leaf_par = bake(leaf_par, enviro_par, bake_par, constants)

pars = c(leaf_par, enviro_par, constants)
C_chl = set_units(246.0161, umol / mol)
FvCB(C_chl, pars)
}
\references{
Buckley TN and Diaz-Espejo A. 2015. Partitioning changes in photosynthetic
rate into contributions from different variables. Plant, Cell & Environment
38: 1200-11.

Farquhar GD, Caemmerer S, Berry JA. 1980. A biochemical model of
photosynthetic CO2 assimilation in leaves of C3 species. Planta 149: 78–90.
}
