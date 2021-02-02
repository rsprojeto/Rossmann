library(plumber)
library(tidyverse)

# 'plumber.R' is the location of the file shown above

server <- plumber::plumb('plumber.R')
server$run(host = '0.0.0.0',port = 5000,swagger = T)
