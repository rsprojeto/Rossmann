## code to prepare `stores` dataset goes here

stores <- data.table::fread("/home/renato/repos/Rossmann/inst/Data/store.csv", stringsAsFactors = T,  na.strings=c("","'","NA"))

usethis::use_data(stores, overwrite = TRUE)
