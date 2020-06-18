## New submission
  
* Package was archived on CRAN

## Changes

* Fixed bug with crossing parameters in `photosynthesis()` that was introduced when `use_tealeaves = TRUE` because of changes in the **tealeaves** package. This led to crossing all parameter values with all unique values of calculated `T_sky`, which was incorrect. Added unit tests to ensuring that crossing is done correctly under `tests/test-photosynthesis-crossing.R`
* Fixed bug in `photosynthesis()` caused by new version of **dplyr**.
* In `enviro_par()`, "sky" temperature (`T_sky`) can now be provided directly as a values (in K) or as a function (the default).
* If `parallel = TRUE` in `photosynthesis()`, **future** uses `plan("multisession")` rather than `plan("multiprocess")`.
* Added full URL for `CONDUCT.md` in README

## Test environments
* local R installation, R 4.1.0
* local R installation, R 4.0.1
* ubuntu 16.04 (on travis-ci), R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.0.1
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
