## Summary of new changes

* Updated CITATION using `bibentry()` instead of `citEntry()`
* Resolved **purrr** deprecations
* removed `expect_no_condition()` from unit tests to resolved issue #12
* Replaced `dplyr::progress_estimated()` with `progress::progress_bar()`
* Fixed error in `photo(..., use_tealeaves = TRUE)`. User-defined changes in stomatal conductance ratio were not being passed to **tealeaves**.
* Added evaporation (E) to `photo()` and `photosynthesis()` output when `use_tealeaves = TRUE`
* Fixed issue with **lifecycle** badges
* Added new function `simulate_error()` to simulate measurement error in gas exchange measurements.

## Test environments
* local R installation, R 4.2.3
* ubuntu 22.04.2 (on Github actions), R 4.2.3
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)
* R-hub builder: Windows Server 2022, R-devel, 64 bit
* R-hub builder: Fedora Linux, R-devel, clang, gfortran

## R CMD check results

❯ checking installed package size ... NOTE
    installed size is  7.1Mb
    sub-directories of 1Mb or more:
      doc   6.1Mb
