
<!-- README.md is generated from README.Rmd. Please edit that file -->

# photosynthesis <img src="man/figures/logo.png" align="right" height="200" width="200"/>

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/cdmuir/photosynthesis.svg?branch=master)](https://travis-ci.org/cdmuir/photosynthesis)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/photosynthesis)](https://cran.r-project.org/package=photosynthesis)
<!-- badges: end -->

## Model C3 Photosynthesis

## Description

**photosynthesis** is a lightweight R package to model C3 photosynthesis
using the Farquhar-von Caemmerer-Berry model. It uses the R package
[**units**](https://CRAN.R-project.org/package=units) to ensure that
parameters are properly specified and transformed before calculations.

## Get **photosynthesis**

From GitHib

``` r
install.packages("devtools")
remotes::install_github("cdmuir/photosynthesis")
```

And load `photosynthesis`

``` r
library("photosynthesis")
```

## Vignette

The **photosynthesis** package simulates photosynthetic rate given a set
of leaf traits and environmental conditions by solving the Farquhar-von
Caemmerer-Berry C3 biochemical model. There are two main steps to using
**photosynthesis**:

1.  define leaf parameters, environmental parameters, temperature
    response parameters, and physical constants; and
2.  solve for the chloroplastic CO2 concentration that balances CO2
    supply and demand (`photo` and `photosynthesis` for single and
    multiple parameter sets, respectively).

In this vignette, I’ll show you how to:

  - run a minimum worked example using default parameters
  - replace default parameters
  - simulate photosynthetic rate along a gradient of CO\(_2\)
    concentrations (\(A-C_\mathrm{c}\) curve)

## Minimum worked example

You can use the default parameter settings and simulate photosynthetic
rate in a single leaf using the `make_*()` functions and `photo()`.

``` r

library(dplyr)
library(magrittr)
library(photosynthesis)

# Leaving the make_* functions empty will automatically default to defaults
# parameters.
bake_par   <- make_bakepar()                       # temperature response parameters
constants  <- make_constants(use_tealeaves = FALSE) # physical constants
leaf_par   <- make_leafpar(use_tealeaves = FALSE)   # leaf parameters
enviro_par <- make_enviropar(use_tealeaves = FALSE) # environmental parameters

photo(leaf_par, enviro_par, bake_par, constants, quiet = TRUE,
      use_tealeaves = FALSE)
#>           C_chl        value convergence                     g_tc
#> 1 24.52925 [Pa] -1.09648e-06           0 1.668765 [umol/m^2/Pa/s]
#>                       A            g_mc25              g_sc
#> 1 27.48581 [umol/m^2/s] 4 [umol/m^2/Pa/s] 4 [umol/m^2/Pa/s]
#>                  g_uc gamma_star25          J_max25       K_C25
#> 1 0.1 [umol/m^2/Pa/s]   3.743 [Pa] 200 [umol/m^2/s] 27.238 [Pa]
#>          K_O25  k_mc  k_sc  k_uc leafsize     phi_J          R_d25
#> 1 16.582 [kPa] 1 [1] 1 [1] 1 [1]  0.1 [m] 0.331 [1] 2 [umol/m^2/s]
#>       T_leaf   theta_J         V_cmax25          V_tpu25 g_mc gamma_star
#> 1 298.15 [K] 0.825 [1] 150 [umol/m^2/s] 200 [umol/m^2/s]    4      3.743
#>   J_max    K_C    K_O R_d V_cmax V_tpu   C_air              O
#> 1   200 27.238 16.582   2    150   200 41 [Pa] 21.27565 [kPa]
#>                P              PPFD      RH    wind
#> 1 101.3246 [kPa] 1500 [umol/m^2/s] 0.5 [1] 2 [m/s]
```

## Replace default parameters

You can look at default parameters settings in the manual (run
`?make_parameters`). These defaults are reasonable, but of course you
will probably want to use different choices and allow some parameters to
vary. Here, I’ll demonstrate how to replace a default. In the next
section, I’ll show you how to set up a gradient of parameter values over
which to solve for leaf temperature.

``` r

# Use the `replace` argument to replace defaults. This must be a named list, and
# each named element must have the proper units specified. See `?make_parameters`
# for all parameter names and proper units.

# Temperature response parameters can be updated (but we won't do that here)
bake_par <- make_bakepar()

# Physical constants probably do not need to be replaced in most cases,
# that's why we call them 'constants'!
constants  <- make_constants(use_tealeaves = FALSE)

# First, we'll change photosynthetic photon flux density to 1000 umol / (m^2 s)
enviro_par <- make_enviropar(
  replace = list(
    PPFD = set_units(1000, "umol/m^2/s")
    ), use_tealeaves = FALSE
  )

# Next, we'll change stomatal conductance to 3 umol / (m^2 s Pa)
leaf_par <- make_leafpar(
  replace = list(
    g_sc = set_units(3, "umol/m^2/s/Pa")
    ), use_tealeaves = FALSE
  )

photo <- photo(leaf_par, enviro_par, bake_par, constants, quiet = TRUE,
               use_tealeaves = FALSE)

photo %>%
  select(PPFD, C_chl, A) %>%
  knitr::kable()
```

|                PPFD |         C\_chl |                       A |
| ------------------: | -------------: | ----------------------: |
| 1000 \[umol/m^2/s\] | 24.0449 \[Pa\] | 25.21885 \[umol/m^2/s\] |

## Environmental gradients

In the previous two examples, I used the `photo` function to solve for a
single parameter set. In most cases, you’ll want to solve for many
parameter sets. The function `photosynthesis` generalizes `photo` and
makes it easy to solve for multiple parameter sets using the same
argument structure. All you need to do is specify multiple values for
one or more leaf or environmental parameters and `photosynthesis` uses
the `purrr::cross` function to fit all combinations\[1\].

``` r

# As before, use the `replace` argument to replace defaults, but this time we
# enter multiple values

bake_par <- make_bakepar()
constants  <- make_constants(use_tealeaves = FALSE)

# First, we'll change the PPFD to 1000 and 1500 umol / (m^2 s)
enviro_par <- make_enviropar(
  replace = list(
    PPFD = set_units(c(1000, 1500), "umol/m^2/s")
    ), use_tealeaves = FALSE
  )

# Next, we'll change stomatal conductance to to 2 and 4 umol / (m^2 s Pa)
leaf_par  <- make_leafpar(
  replace = list(
    g_sc = set_units(c(2, 4), "umol/m^2/s/Pa")
    ), use_tealeaves = FALSE
  )

# Now there should be 4 combinations (high and low g_sc crossed with high and low PPFD)
ph <- photosynthesis(leaf_par, enviro_par, bake_par, constants, 
                     use_tealeaves = FALSE, progress = FALSE, quiet = TRUE)

ph %>% 
  select(g_sc, PPFD, A) %>%
  knitr::kable()
```

|               g\_sc |                PPFD |                       A |
| ------------------: | ------------------: | ----------------------: |
| 2 \[umol/m^2/Pa/s\] | 1000 \[umol/m^2/s\] | 23.90532 \[umol/m^2/s\] |
| 4 \[umol/m^2/Pa/s\] | 1000 \[umol/m^2/s\] | 25.87941 \[umol/m^2/s\] |
| 2 \[umol/m^2/Pa/s\] | 1500 \[umol/m^2/s\] | 25.17778 \[umol/m^2/s\] |
| 4 \[umol/m^2/Pa/s\] | 1500 \[umol/m^2/s\] | 27.48581 \[umol/m^2/s\] |

## Parallel processing

It can take a little while to simulate many different parameter sets. If
you have multiple processors available, you can speed things up by
running simulations in parallel. In the `photosynthesis` function,
simply use the `parallel = TRUE` argument to simulate in parallel. Here
I’ll provide an example simulating an A-Cc curve.

``` r

# We'll use the `replace` argument to enter multiple atmospheric CO2 concentrations

bake_par <- make_bakepar()
constants  <- make_constants(use_tealeaves = FALSE)

enviro_par <- make_enviropar(
  replace = list(
    C_air = set_units(seq(1, 200, length.out = 20), "Pa")
    ), use_tealeaves = FALSE
  )

leaf_par  <- make_leafpar(use_tealeaves = FALSE)

ph <- photosynthesis(leaf_par, enviro_par, bake_par, constants, 
                     use_tealeaves = FALSE, progress = FALSE, 
                     quiet = TRUE, parallel = TRUE)

# Plot C_c versus A
library(ggplot2)

## Drop units for plotting
ph %<>% mutate_if(~ is(.x, "units"), drop_units)
ggplot(ph, aes(C_chl, A)) +
  geom_line(size = 2) +
  xlab(expression(paste(C[chl], " [Pa]"))) +
  ylab(expression(paste("A [", mu, "mol ", m^-2~s^-1, "]"))) +
  theme_bw() +
  NULL
```

<img src="man/figures/README-parallel-example-1.png" width="100%" />

## Incorporating leaf temperature using **tealeaves**

In experiments, leaf temperature can be kept close to air temperature,
but in nature, leaf temperature can be quite a bit different than air
temperature in the shade depending on environmental and leaf parameters.
If `use_tealeaves = TRUE`, `photo()` and `photosynthesis()` will call on
the [**tealeaves**](https://CRAN.R-project.org/package=tealeaves)
package to calculate leaf temperature using an energy balance model.

``` r

# You will need to set use_tealeaves = TRUE when making parameters because additional parameters are needed for tealeaves.

bake_par <- make_bakepar()
constants  <- make_constants(use_tealeaves = TRUE)

enviro_par <- make_enviropar(
  replace = list(
    T_air = set_units(seq(288.15, 313.15, 1), K)
    ), use_tealeaves = TRUE
  )

leaf_par <- make_leafpar(replace = list(
    g_sc = set_units(c(2, 4), umol/m^2/s/Pa)
    ), use_tealeaves = TRUE
  )

ph <- photosynthesis(leaf_par, enviro_par, bake_par, constants, 
                     use_tealeaves = TRUE, progress = FALSE, 
                     quiet = TRUE, parallel = TRUE)

# Plot temperature and photosynthesis
library(ggplot2)

## Drop units for plotting
ph %<>% 
  mutate_if(~ is(.x, "units"), drop_units) %>%
  mutate(`g[s]` = ifelse(g_sc == 2, "low", "high"))

ggplot(ph, aes(T_air, T_leaf, color = `g[s]`)) +
  geom_line(size = 2, lineend = "round") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_color_discrete(name = expression(g[s])) +
  xlab(expression(paste(T[air], " [K]"))) +
  ylab(expression(paste(T[leaf], " [K]"))) +
  theme_bw() +
  NULL
```

<img src="man/figures/README-use-tealeaves-example-1.png" width="100%" />

``` r

ggplot(ph, aes(T_air, A, color = `g[s]`)) +
  geom_line(size = 2, lineend = "round") +
  scale_color_discrete(name = expression(g[s])) +
  xlab(expression(paste(T[leaf], " [K]"))) +
  ylab(expression(paste("A [", mu, "mol ", m^-2~s^-1, "]"))) +
  theme_bw() +
  NULL
```

<img src="man/figures/README-use-tealeaves-example-2.png" width="100%" />

## Contributors

  - [Chris Muir](https://github.com/cdmuir)

## Comments and contributions

I welcome comments, criticisms, and especially contributions\! GitHub
issues are the preferred way to report bugs, ask questions, or request
new features. You can submit issues here:

<https://github.com/cdmuir/photosynthesis/issues>

## Meta

  - Please [report any issues or
    bugs](https://github.com/cdmuir/photosynthesis/issues).
  - License: MIT
  - Get citation information for **photosynthesis** in R doing
    `citation(package = 'photosynthesis')`
  - Please note that this project is released with a [Contributor Code
    of Conduct](CONDUCT.md). By participating in this project you agree
    to abide by its terms.

<!-- end list -->

1.  Since optimization is somewhat time-consuming, be careful about
    crossing too many combinations. Use `progress = TRUE` to show
    progress bar with estimated time remaining.
