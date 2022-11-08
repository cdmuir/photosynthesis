## Summary of new changes

* There is a new vignette on C3 photosynthesis modeling recommendations (modeling-recommendations)
* Under the hood, many changes to `photosynthesis()`, but performance should be the same
* Changed default `C_air` from 41 Pa to 420 umol/mol
* Changed default `O` from 21.27565 kPa to 0.21 mol/mol
* Added optional feature to calculate mesophyll conductance to CO2 (g_mc) as sum of internal airspace (`g_iasc`) and liquid-phase (`g_liqc`) conductances.
* To avoid redundancy, `photo_parameters` is single source of truth for all input parameters to `photo()` and `photosynthesis()`.
* Fixed error in `gc2gw()` and `gw2gc()` and migrated to **gunit** version 1.0.2. Legacy version used version for still air in boundary layer conductance conversions. The corrected version includes modification for laminar flow in the boundary layer. Legacy version can be obtained with option `use_legacy_version = TRUE`.
* Changed default conductance units from `[umol / m ^ 2 / s / Pa]` to `[mol / m ^ 2 / s]`
* Changed `<-` to `=` in many instances
* Changed `%>%` to `|>` in many instances

## Test environments
* local R installation, R 4.2.2
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)
* R_hub builder: Windows Server 2022, R-devel, 64 bit
* R-hub builder: Ubuntu Linux 20.04.1 LTS, R-release, GCC

## R CMD check results

0 errors | 0 warnings | 1 note

❯ checking installed package size ... NOTE
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      doc    3.2Mb
      help   1.1Mb
