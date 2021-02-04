#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny tidyverse ggplot2
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  # Loading new data
  observeEvent(input$predict,{
    # Loading new data
    new_data <- read.csv("/home/renato/repos/Rossmann/inst/Data/test.csv") 
    
    # Loading store data set
    stores <- Rossmann::store
    
    # Joining a new data set with a data set from Stores.
    new_data <- merge(new_data , stores , by = "Store")
    
    # Filter of Store
    new_data <- new_data  %>% dplyr::filter(Store == input$store)
    
    # Convert from json
    news_json <- jsonlite::toJSON(new_data)
    
    # Making request
    resp = httr::POST(url = "http://ec2-18-231-153-70.sa-east-1.compute.amazonaws.com:5000/rossmann",
                      body = news_json, encode = "json")
    
    # Result request
    result <- httr::content(resp, "text")
    
    
    # Convert in data frame
    df <- jsonlite::fromJSON(result)
    
    # Grouping and total revenue per store
    df1 <- df %>% 
      dplyr::group_by(store) %>% 
      dplyr::summarise(total_predictions = sum(predictions), .groups = "drop")
    
    
    
    
    dt <- paste("Store Number", df1$store, "Will sell R$", formattable::currency(df1$total_predictions, big.mark = ","))
    
    output$test <- renderPrint(dt)
    
  })
  
  
  

}




