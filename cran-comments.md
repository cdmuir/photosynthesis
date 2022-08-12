## Summary of new changes

* In the DESCRIPTION file, rewrote references in the form authors (year) <doi:...>
* In the DESCRIPTION file, added () behind all function names
* Added \value to .Rd files regarding exported methods for bake.Rd, bake_par.Rd, constants.Rd, enviro_par.Rd, leaf_par.Rd, parameter_names.Rd
* Changed print() to stop() or message() in R/compile_data.R; R/fit_gs_model.R; R/fit_t_response.R; R/print_graphs.R
* In R/print_graphs.R, added code to restore users' option for par()$mfrow
* Removed "2020" from the field COPYRIGHT HOLDER in the LICENCE file
* Updated link to Prometheus protocols in vignette
* Stopped evaluating parallel example in vignette
* Fixed tests that failed because of update to dependency

## Test environments
* local R installation, R 4.2.1
* win-builder (release)
* win-builder (devel)
* win-builder (oldrelease)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
    installed size is  5.2Mb
    sub-directories of 1Mb or more:
      doc    3.2Mb
      help   1.1Mb
