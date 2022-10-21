## code to prepare `photo-parameters` data set goes here
photo_parameters = readr::read_csv("inst/extdata/photo-parameters.csv")
            
usethis::use_data(photo_parameters, overwrite = TRUE)
