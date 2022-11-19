## Summary of new changes

* Added Bayesian options to fit light-response and light respiration models via `fit_photosynthesis(..., .method = "brms")`
* Preferred method for fitting data to models is `fit_photosynthesis()` which performs all checks and manipulations prior to passing arguments to various `fit_` functions.
* Deprecated `fit_r_light_kok()`, `fit_r_light_WalkerOrt()`, `fit_r_light_yin()` in favor of `fit_r_light2()`. The new function uses non-standard evaluation to replace variable names as in `dplyr::rename()`. It will also extend functionality to enable Bayesian fitting using **brms** and does not output a plot.
* Added Bayesian fitting method to `fit_aq_response2()` using **brms** package.
* Deprecated `fit_aq_response()` in favor of `fit_aq_response2()`. The new function uses non-standard evaluation to replace variable names as in `dplyr::rename()`. It will also extend functionality to enable Bayesian fitting using **brms** and does not output a plot.
* Fixed bug with setting upper bound for search in `find_A()`
* Addressed warnings about deprecated arguments in **tidyselect** and **ggplot2**
* Added `C_i` (intercellular CO2 concentration) to output from `photo()` and `photosynthesis()`

## Test environments
* local R installation, R 4.2.2
* ubuntu 20.04.1 (on Github actions), R 4.2.2
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)
* R-hub builder: Windows Server 2022, R-devel, 64 bit
* R-hub builder: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* R-hub builder: Fedora Linux, R-devel, clang, gfortran

## R CMD check results

❯ checking installed package size ... NOTE
    installed size is  7.1Mb
    sub-directories of 1Mb or more:
      doc   6.1Mb
