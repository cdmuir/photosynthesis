## code to prepare `li6800_example` dataset goes here

usethis::use_data(li6800_example, overwrite = TRUE)

file.copy(
  "/Users/cdmuir/Library/CloudStorage/GoogleDrive-cdmuir@hawaii.edu/Shared drives/muir-lab/adaptive-amphistomy/raw-data/licor/2023-04-23-0804_logdata",
  "inst/extdata/li6800_example"
)
