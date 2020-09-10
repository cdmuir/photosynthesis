library(readr)
library(usethis)

baked_input <- read_csv("data-raw/baked-input.csv")

usethis::use_data(baked_input, overwrite = TRUE)