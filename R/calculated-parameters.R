#' Get default functions for calculated parameters in \link{photosynthesis}
#' 
#' @name calculated-parameters
#' @param .f_name character string of function
#' @encoding UTF-8
get_f_parameter = function(.f_name) {
  
  .f_name |>
    match.arg(c("f_nu", "f_sh", "T_sky")) |>
    switch(
      
      f_nu = function(Re, type, T_air, T_leaf, surface, unitless) {
        if (!unitless) {
          stopifnot(units(T_air)$numerator == "K" &
                      length(units(T_air)$denominator) == 0L)
          stopifnot(units(T_leaf)$numerator == "K" &
                      length(units(T_leaf)$denominator) == 0L)
        }
        
        type %<>% match.arg(c("free", "forced"))
        
        if (identical(type, "forced")) {
          if (unitless) {
            if (Re <= 4000) ret = list(a = 0.6, b = 0.5)
            if (Re > 4000) ret = list(a = 0.032, b = 0.8)
          } else {
            if (Re <= set_units(4000)) ret = list(a = 0.6, b = 0.5)
            if (Re > set_units(4000)) ret = list(a = 0.032, b = 0.8)
          }
          return(ret)
        }
        
        if (identical(type, "free")) {
          surface %<>% match.arg(c("lower", "upper"))
          if ((surface == "upper" & T_leaf > T_air) |
              (surface == "lower" & T_leaf < T_air)) {
            ret = list(a = 0.5, b = 0.25)
          } else {
            ret = list(a = 0.23, b = 0.25)
          }
          return(ret)
        }
      },
      
      f_sh = function(type, unitless) {
        type |>
          match.arg(c("free", "forced")) |>
          switch(forced = 0.33, free = 0.25)
      },
      
      T_sky = function(pars) {
        set_units(pars$T_air, K) - set_units(20, K) * 
          set_units(pars$S_sw, W / m^2) / set_units(1000, W / m^2)
      }
      
    )
  
}
