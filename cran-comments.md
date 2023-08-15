## Summary of new changes

* Removed imports of unexported **tealeaves** functions using `:::` operator
* `read_licor()` removes lines where parameter settings are changed between logging

## Test environments
* local R installation, R 4.3.1
* ubuntu 22.04.3 (on Github actions), R 4.3.1
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)
* R-hub builder: Windows Server 2022, R-devel, 64 bit
* R-hub builder: Fedora Linux, R-devel, clang, gfortran

## R CMD check results

❯ checking installed package size ... NOTE
    installed size is  7.3Mb
    sub-directories of 1Mb or more:
      doc   6.1Mb
