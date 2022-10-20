## code to prepare `photo-parameters` dataset goes here
photo_parameters = readr::read_csv(system.file("extdata", "photo-parameters.csv", package = "photosynthesis"))
            
usethis::use_data(photo_parameters, overwrite = TRUE)
