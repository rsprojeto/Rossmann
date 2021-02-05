#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard tidyverse
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
    ui <- dashboardPage(
      dashboardHeader(title = "Sales Forecast Rossmann"),
      dashboardSidebar(
        sidebarMenu(numericInput("store",label = "Number of Store", value = ""),
                    actionButton("predict","Predict", icon = icon("crystal ball"), color="blue",
                                 class = "btn-success")
                    
          
         
        )
      ),
      dashboardBody(
        verbatimTextOutput("test")
      )
    )
    
    )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Rossmann'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

