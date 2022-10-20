## code to prepare `photo-parameters` dataset goes here
photo_parameters = data.frame(x =1 , y = 2)
photo_parameters = read.csv(system.file("extdata", "photo-parameters.csv", package = "photosynthesis"))
            
usethis::use_data(photo_parameters, overwrite = TRUE)
