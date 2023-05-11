## Summary of new changes

* Added example LI6800 data set (inst/extdata/li6800_example) for unit testing `read_licor()`
* Soft-deprecated `read_li6800()` in favor of `read_licor()`
* Soft-deprecated `fit_many()` in favor of generic methods like `purrr::map()`

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
    installed size is  7.2Mb
    sub-directories of 1Mb or more:
      doc   6.1Mb
