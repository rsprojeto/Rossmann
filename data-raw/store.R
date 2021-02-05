## code to prepare `store` dataset goes here

store <- data.table::fread("/home/renato/repos/Rossmann/inst/Data/store.csv", stringsAsFactors = T,  na.strings=c("","'","NA"))

usethis::use_data(store, overwrite = TRUE)
