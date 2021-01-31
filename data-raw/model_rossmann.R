## code to prepare `model_rossmann` dataset goes here

model_rossmann <- readRDS("/home/renato/repos/Rossmann/inst/Resultados_Modelos/xg_fit_final.rds")

usethis::use_data(model_rossmann, overwrite = TRUE)
