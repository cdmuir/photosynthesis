# photosynthesis (development version)

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
