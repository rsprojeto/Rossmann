
# 1) Data Cleaning --------------------------------------------------------



#' Data Cleaning
#' 
#' @param df data.frame data.frame with new data.


#' @export

data_cleaning <- function(df){

  # snacke_case
  df <- df %>% janitor::clean_names()
  
  # convert in Date
  df$date <- lubridate::ymd(df$date)
  
  
  
  # Removing Missing Data
  df <- df %>% 
    dplyr::mutate(
      
      # replace the missing values with the value of 200000
      competition_distance = ifelse(is.na(competition_distance), 200000, competition_distance),
      
      # replace the missing values with the month in the date column     
      competition_open_since_month = ifelse(is.na(competition_open_since_month), lubridate::month(date), competition_open_since_month),
      
      # replace the missing values with the year in the date column      
      competition_open_since_year = ifelse(is.na(competition_open_since_year), lubridate::year(date), competition_open_since_year),
      
      # replace the missing values with the week in the date column      
      promo2since_week = ifelse(is.na(promo2since_week ), lubridate::week(date), promo2since_week ),
      
      # replace the missing values with the year in the date column      
      promo2since_year = ifelse(is.na(promo2since_year), lubridate::year(date), promo2since_year),
      
      month_map = lubridate::month(date),
      
      month_map = month.abb[month_map])
  
  
  # removing the blanks 
  df$promo_interval <- stringr::str_squish(df$promo_interval)
  
  # replacing missing values with 0
  df$promo_interval[df$promo_interval==""] = "0"
  
  # creating a column with the months that the promotion is active with value 1 and not active with value 0
  df <- df %>% 
    dplyr::mutate(is_promo = ifelse(promo_interval == "0",0,ifelse(stringr::str_detect(promo_interval, month_map),1,0)))
  
  return(df)
}



# 2) Feature Enginnering --------------------------------------------------



#' Feature Engineering
#' 
#' @param df clean data set.
#'

#' @export

feature_engineering <- function(df) {
  
  
  
  df <- df %>% 
    dplyr::mutate(
      
      # Extracting year
      year = base::as.integer(lubridate::year(date)),
      
      # Extracting month
      month = lubridate::month(date),
      
      # Extracting day
      day = lubridate::day(date),
      
      #Extracting week of the year
      week_of_year = lubridate::week(date),
      
      # Extracting year and week
      year_week = base::strftime(date, format = "%Y-%W"),
      
      # Extracting first day of the month
      first_day_of_month = "01",
      
      # Turning into Date
      competition_since = lubridate::make_date(competition_open_since_year, competition_open_since_month,first_day_of_month),
      
      # Getting the difference in days
      competition_time_month = base::as.integer(difftime(date, competition_since, units = "days")/30)
      
    )
  
  
  # this is function to convert year-week to year-month-day
  data_da_semana <- function(ano, semana, diadasemana){
    w <- base::paste0(ano, "-W", base::sprintf("%02d", semana), "-", diadasemana)
    ISOweek::ISOweek2date(w)-1
  }
  
  df <- df %>% 
    dplyr::mutate(
      
      # convert year-week to year-month-day
      promo_since = data_da_semana(promo2since_year, promo2since_week, 1),
      
      # Getting the difference in days
      promo_time_week = base::difftime(date, promo_since, units = "days")/7,
      
      # converting to integer
      promo_time_week = base::as.integer(promo_time_week),
      
      assortment = dplyr::case_when(
        # changing from a to basic  
        assortment == "a" ~ "basic",
        
        # changing from b to extra 
        assortment == "b" ~ "extra",
        
        # everything else for extended
        T ~ "extended"),
      
      state_holiday = dplyr::case_when(
        # changing from a to public_holiday  
        state_holiday == "a" ~ "public_holiday",
        
        # changing from b to easter_holiday
        state_holiday == "b" ~ "easter_holiday",
        
        # changing from b to christmas
        state_holiday == "c" ~ "christmas",
        
        # everything else for regular_day     
        T ~ "regular_day"))
  
  return(df)
}
# 3) Data Preparation -----------------------------------------------------



#' Data Preparation
#' 
#' @param df clean and feature engineered data set.
#'

#' @export

data_preparation <- function(df){
  
  
  minmax_scaler <- function(x) {
    
    
    return( ( x - min( x ) )  / ( max(x) - min(x) ) ) 
  }
  
  robust_scaler <- function(x){
    
    return( ( x - quantile( x , 0.5) )  / ( quantile(x ,0.75) - quantile(x, 0.25) ) )
    
  }
  
  
  # Variable filtering
  # Removing the records of the days the stores are closed and thus obtaining only sales with values> 0
  #df3 <- 
    #df3 %>% 
    #dplyr::filter(open == 1)
  
  # Removing features that were not available at the time of production and will not be needed.
  #df3 <- df3 %>% 
    #dplyr::select( -open, -promo_interval, -month_map)
  
  
  # Rescaling
  
  df <- df %>% 
    dplyr::mutate(competition_distance = robust_scaler(competition_distance),
                  competition_time_month = robust_scaler(competition_time_month),
                  promo_time_week = minmax_scaler(promo_time_week),
                  year= log(year))
  
  ## Transformation
  
  ### Encoding
  
  # state_holiday -  One Hot Encoding
  one_hot_state_holiday <- df %>% 
    dplyr::select(state_holiday) %>% 
    fastDummies::dummy_cols()
  
  # store_type - Label Encoding
  df <- df %>%
    dplyr::mutate(store_type= dplyr::recode_factor(store_type,"a"="1",
                                                   "b"="2",
                                                   "c"="3",
                                                   "d"= "4",.ordered = TRUE))
  
  # assortment - Ordinal Encoding
  df <- df %>%
    dplyr::mutate(assortment= dplyr::recode_factor(assortment,"basic"="1",
                                                   "extra"="2",
                                                   "extended"="3",.ordered = TRUE))
  
  df <- base::cbind(df, one_hot_state_holiday)
  
  df <- df %>% 
    dplyr::select( - state_holiday)
  
  
  ### Nature Transformation
  
  df <- df %>% 
    dplyr::mutate(day_of_week_sin = round(sin(day_of_week*(2. * pi/7)),2),
                  day_of_week_cos = round(cos(day_of_week*(2. * pi/7)),2),
                  month_sin = round(sin(month*(2. * pi/12)),2),
                  month_cos = round(cos(month*(2. * pi/12)),2),
                  day_sin = round(sin(day*(2. * pi/30)),2),
                  day_cos = round(cos(day*(2. * pi/30)),2),
                  week_of_year_sin = round(sin(week_of_year*(2. * pi/52)),2),
                  week_of_year_cos = round(cos(week_of_year*(2. * pi/52)),2),
    )
  
  
  df <- df %>% dplyr::mutate(store_type = base::as.double(store_type), 
                        assortment = base::as.double(assortment))
  df <- df %>% dplyr::select("store","store_type","promo", "assortment", "competition_distance", "competition_open_since_month",
                                 "competition_open_since_year","promo2","promo2since_week","promo2since_year", "competition_time_month",
                                 "promo_time_week", "day_of_week_sin", "day_of_week_cos", "month_sin","month_cos", "day_sin", "day_cos",
                                 "week_of_year_sin", "week_of_year_cos", )
  
  
}


#' run_api
#' 
#' @param "" clean and feature engineered data set.
#'

#' @export
run_api <- function(){
  server <- plumber::plumb("plumber.R")
  server$run(host = '0.0.0.0',port = 5000,swagger = T)
}