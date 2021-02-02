library(plumber)
library(tidyverse)

# Read model data.
final_model <- Rossmann::model_rossmann

#* @parser json
#* @param Id
#* @param Store
#* @param DayOfWeek
#* @param Date
#* @param Open
#* @param Promo
#* @param StateHoliday
#* @param SchoolHoliday
#* @post /rossmann

predictions <- function(req,Id , Store ,DayOfWeek , Date , Open , Promo , StateHoliday ,SchoolHoliday){
  
  
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
  
  # Id <- as.integer(Id)
  # Store <- as.integer(Store)
  # DayOfWeek <- as.integer(DayOfWeek)
  # Date <- as.character(Date)
  # Open <- as.integer(Open)
  # Promo <- as.integer(Promo)
  # StateHoliday <- as.character(StateHoliday )
  # SchoolHoliday <- as.integer(SchoolHoliday)
  # 
  # new_data <- dplyr::tibble(Id = Id, Store = Store, DayOfWeek = DayOfWeek,
  #                         Date = Date , Open = Open , Promo = Promo,
  #                         StateHoliday = StateHoliday, SchoolHoliday = SchoolHoliday)
  
  # Loading Dataset Store
  stores <- Rossmann::store
  
  # Merge Datasets New_Data and Stores
  new_data <- merge(new_data , stores , by= "Store")
  
  new_data <- Rossmann::data_cleaning(new_data)
  
  new_data <- Rossmann::feature_engineering(new_data)
  
  new_data <- Rossmann::data_preparation(new_data)
  
  # Making the prediction
  new_data$predictions<- predict(final_model, new_data) %>% 
    rename(predictions = .pred)
  
  return(new_data)
  
  
  
}