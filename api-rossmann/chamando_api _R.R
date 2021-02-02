library(httr)
library(magrittr)
library(tidyverse)
library(tidymodels)
library(httr)
library(lubridate)

url <- parse_url("http://127.0.0.1:5000")
url$path <- "/rossmann"

url$query <- list(
 
  "Id" = 14,
  "Store" = 20,
  "DayOfWeek" = 4,
  "Date" = "2015-09-17",
  "Open" = 1,
  "Promo" = 1,
  "StateHoliday" = "0",
  "SchoolHoliday" = 0
)

response <- POST(
  url = build_url(url),
  add_headers(
    #"Content-Type" = "json",
    "Accept" = "application/json"
  )
)



df <- as.data.frame(httr::content(response))







