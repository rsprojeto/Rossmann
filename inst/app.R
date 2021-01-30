library(plumber)
library(Rossmann)

#r = plumb("plumber_api.R")
#r$run(port = 5000, host = "0.0.0.0")

# server <- plumb(system.file('plumber.R',package = "Rossmann"))
server <- plumb('plumber.R')
server$run(host = '0.0.0.0',port = 5000,swagger = T)


