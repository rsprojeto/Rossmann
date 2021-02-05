## code to prepare `new_data` dataset goes here


new_data <- read.csv("/home/renato/repos/Rossmann/inst/Data/test.csv")

usethis::use_data(new_data, overwrite = TRUE)