## Summary of new changes

* Commented out examples that took a long time to run
* Added `progress` option to `fit_many()` to toggle progress bar
* We removed large files from help subdirectory
* Divided large vignette into smaller vignettes and removed figures to reduce file size
* Updated CITATION
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
* Removed `check_dependencies()` function

## Test environments
* local R installation, R 4.2.2
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)
* R-hub builder: Windows Server 2022, R-devel, 64 bit

## R CMD check results

❯ checking installed package size ... NOTE
    installed size is  6.9Mb
    sub-directories of 1Mb or more:
      doc   6.0Mb
