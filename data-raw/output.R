library(readr)
library(usethis)

output <- read_csv("data-raw/output.csv")

usethis::use_data(output, overwrite = TRUE)