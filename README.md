photosynthesis <img src="hex-sticker/hex-sticker.png" align="right" height="200" width="200"/>
==============================================================================================

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build
Status](https://travis-ci.com/cdmuir/photosynthesis.svg?token=yprDUtRtPBa2Ma9G4sFP&branch=master)](https://travis-ci.com/cdmuir/photosynthesis)
<!---
[![codecov](https://codecov.io/gh/cdmuir/photosynthesis/branch/master/graph/badge.svg)](https://codecov.io/gh/cdmuir/photosynthesis)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/photosynthesis)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/photosynthesis)](https://cran.r-project.org/package=photosynthesis)
-->

Model C3 Photosynthesis
-----------------------

Description
-----------

`photosynthesis` is a lightweight R package to model C3 photosynthesis
using the Farquhar-von Caemmerer-Berry model. It uses the R package
[units](https://cran.r-project.org/web/packages/units/index.html) to
ensure that parameters are properly specified and transformed before
calculations.

Get `photosynthesis`
--------------------

or from GitHib

    install.packages("devtools")
    devtools::install_github("cdmuir/photosynthesis")

And load `photosynthesis`

    library("photosynthesis")

Vignette
--------

The `photosynthesis` package simulates photosynthetic rate given a set
of leaf traits and environmental conditions by solving the Farquhar-von
Caemmerer-Berry C3 biochemical model. There are two main steps to using
`photosynthesis`:

1.  define leaf parameters, environmental parameters, temperature
    response parameters, and physical constants; and
2.  solve for the chloroplastic CO2 concentration that balances CO2
    supply and demand (`photo` and `photosynthesis` for single and
    multiple parameter sets, respectively).

In this vignette, I'll show you how to:

-   run a minimum worked example using default parameters
-   replace default parameters
-   simulate photosynthetic rate along a gradient of CO2 concentrations
    (A-Cc curve)

Minimum worked example
----------------------

You can use the default parameter settings and simulate photosynthetic
rate in a single leaf using the `make_*()` functions and `photo()`.


    library(magrittr)
    library(photosynthesis)

    # Leaving the make_* functions empty will automatically default to defaults
    # parameters.
    leaf_par   <- make_leafpar()   # leaf parameters
    enviro_par <- make_enviropar() # environmental parameters
    bake_par   <- make_bakepar()   # temperature response parameters
    constants  <- make_constants() # physical constants

    photo(leaf_par, enviro_par, bake_par, constants, quiet = TRUE)
    #>           C_chl         value convergence            g_mc25
    #> 1 25.56715 [Pa] -1.193432e-06           0 4 [umol/m^2/Pa/s]
    #>                g_sc                g_uc gamma_star25          J_max25
    #> 1 4 [umol/m^2/Pa/s] 0.1 [umol/m^2/Pa/s]   3.743 [Pa] 200 [umol/m^2/s]
    #>         K_C25        K_O25  k_mc  k_sc  k_uc leafsize       phi
    #> 1 27.238 [Pa] 16.582 [kPa] 1 [1] 1 [1] 1 [1]  0.1 [m] 0.331 [1]
    #>            R_d25     T_leaf   theta_J         V_cmax25          V_tpu25
    #> 1 2 [umol/m^2/s] 298.15 [K] 0.825 [1] 150 [umol/m^2/s] 200 [umol/m^2/s]
    #>                g_mc gamma_star            J_max         K_C          K_O
    #> 1 4 [umol/m^2/Pa/s] 3.743 [Pa] 200 [umol/m^2/s] 27.238 [Pa] 16.582 [kPa]
    #>              R_d           V_cmax            V_tpu   C_air              O
    #> 1 2 [umol/m^2/s] 150 [umol/m^2/s] 200 [umol/m^2/s] 41 [Pa] 21.27565 [kPa]
    #>                P              PPFD      RH      T_air    wind
    #> 1 101.3246 [kPa] 1500 [umol/m^2/s] 0.5 [1] 298.15 [K] 2 [m/s]
    #>                       g_tc                     A
    #> 1 1.813404 [umol/m^2/Pa/s] 27.98599 [umol/m^2/s]

Replace default parameters
--------------------------

You can look at default parameters settings in the manual (run
`?make_parameters`). These defaults are reasonable, but of course you
will probably want to use different choices and allow some parameters to
vary. Here, I'll demonstrate how to replace a default. In the next
section, I'll show you how to set up a gradient of parameter values over
which to solve for leaf temperature.


    # Use the `replace` argument to replace defaults. This must be a named list, and
    # each named element must have the proper units specified. See `?make_parameters`
    # for all parameter names and proper units.

    # First, we'll change stomatal conductance to 3 umol / (m^2 s Pa)
    leaf_par <- make_leafpar(
      replace = list(
        g_sc = set_units(3, "umol/m^2/s/Pa")
        )
      )

    # Next, we'll change photosynthetic photon flux density to 1000 umol / (m^2 s)
    enviro_par <- make_enviropar(
      replace = list(
        PPFD = set_units(1000, "umol/m^2/s")
        )
      )

    # Temperature response parameters can be updated (but we won't do that here)
    bake_par <- make_bakepar()

    # Physical constants probably do not need to be replaced in most cases,
    # that's why we call them 'constants'!
    constants  <- make_constants()

    photo <- photo(leaf_par, enviro_par, bake_par, constants, quiet = TRUE)

    photo
    #>           C_chl         value convergence            g_mc25
    #> 1 24.96474 [Pa] -1.250944e-06           0 4 [umol/m^2/Pa/s]
    #>                g_sc                g_uc gamma_star25          J_max25
    #> 1 3 [umol/m^2/Pa/s] 0.1 [umol/m^2/Pa/s]   3.743 [Pa] 200 [umol/m^2/s]
    #>         K_C25        K_O25  k_mc  k_sc  k_uc leafsize       phi
    #> 1 27.238 [Pa] 16.582 [kPa] 1 [1] 1 [1] 1 [1]  0.1 [m] 0.331 [1]
    #>            R_d25     T_leaf   theta_J         V_cmax25          V_tpu25
    #> 1 2 [umol/m^2/s] 298.15 [K] 0.825 [1] 150 [umol/m^2/s] 200 [umol/m^2/s]
    #>                g_mc gamma_star            J_max         K_C          K_O
    #> 1 4 [umol/m^2/Pa/s] 3.743 [Pa] 200 [umol/m^2/s] 27.238 [Pa] 16.582 [kPa]
    #>              R_d           V_cmax            V_tpu   C_air              O
    #> 1 2 [umol/m^2/s] 150 [umol/m^2/s] 200 [umol/m^2/s] 41 [Pa] 21.27565 [kPa]
    #>                P              PPFD      RH      T_air    wind
    #> 1 101.3246 [kPa] 1000 [umol/m^2/s] 0.5 [1] 298.15 [K] 2 [m/s]
    #>                       g_tc                     A
    #> 1 1.599325 [umol/m^2/Pa/s] 25.64559 [umol/m^2/s]

Environmental gradients
-----------------------

In the previous two examples, I used the `photo` function to solve for a
single parameter set. In most cases, you'll want to solve for many
parameter sets. The function `photosynthesis` generalizes `photo` and
makes it easy to solve for multiple parameter sets using the same
argument structure. All you need to do is specify multiple values for
one or more leaf or environmental parameters and `photosynthesis` uses
the `tidyr::crossing` function to fit all combinations[1].


    # As before, use the `replace` argument to replace defaults, but this time we
    # enter multiple values

    # First, we'll change stomatal conductance to to 2 and 4 umol / (m^2 s Pa)
    leaf_par  <- make_leafpar(
      replace = list(
        g_sc = set_units(c(2, 4), "umol/m^2/s/Pa")
        )
      )

    # Next, we'll change the PPFD to 1000 and 1500 umol / (m^2 s)
    enviro_par <- make_enviropar(
      replace = list(
        PPFD = set_units(c(1000, 1500), "umol/m^2/s")
        )
      )

    bake_par <- make_bakepar()
    constants  <- make_constants()

    # Now there should be 4 combinations (high and low g_swccrossed with high and low PPFD)
    ph <- photosynthesis(leaf_par, enviro_par, bake_par, constants, 
                               progress = FALSE, quiet = TRUE)

    ph %>% dplyr::select(g_sc, PPFD, A)
    #>                g_sc              PPFD                     A
    #> 1 2 [umol/m^2/Pa/s] 1000 [umol/m^2/s] 24.32671 [umol/m^2/s]
    #> 2 2 [umol/m^2/Pa/s] 1500 [umol/m^2/s] 25.66757 [umol/m^2/s]
    #> 3 4 [umol/m^2/Pa/s] 1000 [umol/m^2/s] 26.30417 [umol/m^2/s]
    #> 4 4 [umol/m^2/Pa/s] 1500 [umol/m^2/s] 27.98599 [umol/m^2/s]

Contributors
------------

-   [Chris Muir](https://github.com/cdmuir)

Comments and contributions
--------------------------

I welcome comments, criticisms, and especially contributions! GitHub
issues are the preferred way to report bugs, ask questions, or request
new features. You can submit issues here:

<https://github.com/cdmuir/photosynthesis/issues>

Meta
----

-   Please [report any issues or
    bugs](https://github.com/cdmuir/photosynthesis/issues).
-   License: MIT
    <!--- * Get citation information for `photosynthesis` in R doing `citation(package = 'photosynthesis')` -->
-   Please note that this project is released with a [Contributor Code
    of Conduct](CONDUCT.md). By participating in this project you agree
    to abide by its terms.

[1] Since optimization is somewhat time-consuming, be careful about
crossing too many combinations. Use `progress = TRUE` to show progress
bar with estimated time remaining.
