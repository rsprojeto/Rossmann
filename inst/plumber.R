library(tidyverse)
library(magrittr)
library(Rossmann)



# Read model data.
final_model <- readRDS(system.file("Resultados_Modelos/xg_fit_final.rds", package = "Rossmann"))

#final_model <- readRDS("xg_fit_final.rds")

#* @parse json
#* @param Id            
#* @param Store         
#* @param DayOfWeek     
#* @param Date         
#* @param Open          
#* @param Promo         
#* @param StateHoliday  
#* @param SchoolHoliday  
#* @post /predict_rossmann


# Creating the function that will be the route of the API
get_prediction <- function(req, Id , Store ,DayOfWeek , Date , Open , Promo , StateHoliday ,SchoolHoliday ) {
  
  # Assigning types New dataset
  Id                                   %<>% as.integer 
  Store                                %<>% as.integer
  DayOfWeek                            %<>% as.integer 
  Date                                 %<>% as.character
  Open                                 %<>% as.integer
  Promo                                %<>% as.integer 
  StateHoliday                         %<>% as.character
  SchoolHoliday                        %<>% as.integer
  
  # data frame with new data
  new_data <- tibble(Id = Id, Store = Store , DayOfWeek = DayOfWeek , Date = Date , Open = Open , Promo = Promo , 
                     StateHoliday = StateHoliday ,SchoolHoliday = SchoolHoliday  )
  
  # Loading Dataset Store
  stores <- Rossmann::store
  
  # Merge Datasets New_Data and Stores
  new_data <- merge(new_data , stores , by= "Store")
  
  # Applying the function to clear / feature engineering and data preparation the data
  new_data <- Rossmann::data_cleaning(new_data)
  
  new_data <- Rossmann::feature_engineering(new_data)
  
  new_data <- Rossmann::data_preparation(new_data)
  
  # Making the prediction
  new_data$predictions<- predict(final_model, new_data) %>% 
    rename(predictions = .pred)
  
  #return(new_data)#pred # Returning predictions
  new_data
}







  