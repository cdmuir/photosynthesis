## Summary of new changes

* Added `photoinhibition()` to light response models. This allows users to estimate photoinhibition at high light.

## Test environments
* local R installation, R 4.4.2
* ubuntu 22.04.5 (on Github actions), R 4.4.2
* rhub (version 2): 
  - Ubuntu 22.04.5, R-devel, gcc
  - Microsoft Windows Server 2022 10.0.20348, R-devel
  - macOS 13.7.1, R-release, clang

## R CMD check results

❯ checking installed package size ... NOTE
    installed size is  7.4Mb
    sub-directories of 1Mb or more:
      doc   6.2Mb

## rhub results
Tested using `rhub` version 2. All checks passed successfully on the above platforms.

## Downstream dependencies
No known issues with downstream dependencies.
