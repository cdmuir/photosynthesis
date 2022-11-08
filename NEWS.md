# photosynthesis 2.1.0

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

# photosynthesis 2.0.3

* In the DESCRIPTION file, rewrote references in the form authors (year) <doi:...>
* In the DESCRIPTION file, added () behind all function names
* Added \value to .Rd files regarding exported methods for bake.Rd, bake_par.Rd, constants.Rd, enviro_par.Rd, leaf_par.Rd, parameter_names.Rd
* Changed print() to stop() or message() in R/compile_data.R; R/fit_gs_model.R; R/fit_t_response.R; R/print_graphs.R
* In R/print_graphs.R, added code to restore users' option for par()$mfrow
* Removed "2020" from the field COPYRIGHT HOLDER in the LICENCE file
* Updated link to Prometheus protocols in vignette
* Stopped evaluating parallel example in vignette
* Fixed tests that failed because of update to dependency **units** 0.8-0. (#7)

# photosynthesis 2.0.1

* for `temp_resp1` and `temp_resp2`, corrected reference. (#6)

# photosynthesis 2.0.0

* Added analytical tools for plant ecophysiology, including fitting stomatal
conductance models, photosynthetic responses to light, CO2, and temperature,
light respiration, as well as tools for performing sensitivity analyses.

* Added tests for new functions.

* Added new vignette to include examples of new analytical functions.

# photosynthesis 1.0.2

* Fixed bug with crossing parameters in `photosynthesis()` that was introduced when `use_tealeaves = TRUE` because of changes in the **tealeaves** package. This led to crossing all parameter values with all unique values of calculated `T_sky`, which was incorrect. Added unit tests to ensuring that crossing is done correctly under `tests/test-photosynthesis-crossing.R`
* Fixed bug in `photosynthesis()` caused by new version of **dplyr**.
* In `enviro_par()`, "sky" temperature (`T_sky`) can now be provided directly as a values (in K) or as a function (the default).
* If `parallel = TRUE` in `photosynthesis()`, **future** uses `plan("multisession")` rather than `plan("multiprocess")`.
* Added full URL for `CONDUCT.md` in README
* Fixed cross-references in .Rd files

# photosynthesis 1.0.1

Release to be archived with revision of "Is amphistomy an adaptation to high light? Optimality models of stomatal traits along light gradients."

[Blog post.](https://cdmuir.netlify.app/post/2019-05-21-phyteclub/)

# photosynthesis 1.0.0

Description: Simulate C$_3$ photosynthesis using the Farquhar, von Caemmerer, Berry (1980) model as described in Buckley and Diaz-Espejo (2015). It uses units to ensure that parameters are properly specified and transformed before calculations. Temperature response functions get automatically "baked" into all parameters based on leaf temperature following Bernacchi et al. (2002). The package includes boundary layer, cuticular, stomatal, and mesophyll conductances to CO$_2$, which each can vary on the upper and lower portions of the leaf. Use straightforward functions to simulate photosynthesis over environmental gradients such as Photosynthetic Photon Flux Density (PPFD) and leaf temperature, or over trait gradients such as CO$_2$ conductance or photochemistry. 

* Added a `NEWS.md` file to track changes to the package.
