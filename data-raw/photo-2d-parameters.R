## code to prepare `photo-2d-parameters` data set goes here

# Import shared parameters from photo_parameters
photo_parameters = readr::read_csv("inst/extdata/photo-parameters.csv")
photo_2d_parameters = readr::read_csv("inst/extdata/photo-2d-parameters.csv")

usethis::use_data(photo_parameters, overwrite = TRUE)
