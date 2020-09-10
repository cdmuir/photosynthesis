library(readr)
library(usethis)

input <- read_csv("data-raw/input.csv")

usethis::use_data(input, overwrite = TRUE)