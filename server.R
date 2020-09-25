# Server
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(googleAuthR)
library(shinyjs)


# Load data
usersfilepath <- file.path("data","user_credentials.csv")
gauth_detailsfilepath <- file.path("data","google_authentication.csv")


df_users <- read.csv(file = usersfilepath, stringsAsFactors = FALSE)
df_auth <- read.csv(file = gauth_detailsfilepath, stringsAsFactors = FALSE)


# guidance og google authentication is availabe here https://lmyint.github.io/post/shiny-app-with-google-login/
# ------------------------------------------

# GOOGLE AUTHENTICATION Steps
# 


#Important details for GOOGle authenation

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = df_auth[1,'client_id'])
options("googleAuthR.webapp.client_secret" = df_auth[1,'client_secret'])



# user infor for google authentication
user_info <- function(){
  f <- gar_api_generator("https://www.googleapis.com/oauth2/v1/userinfo",
                         "GET",
                         data_parse_function = function(x) x
  )
  f()
}


server <- function(input, output, session) {
  # For this search form, the corresponding values in the server-side code would be input$searchText and input$searchButton.
  
  # This section of the code is borrowed from James Peng's authentication and slightly 
  # modified to fit my application
  
  # -------------------------------------------
  # AUTHENTICATION
  # -------------------------------------------
  
  # this part of the code creates a modal dialog login in page
  
  dataModal <- function(failed = FALSE) {
    if (failed == FALSE){
      modalDialog(
        tags$head(tags$style(".modal-dialog{min-width:1200px;}")),
        tags$head(tags$style(".modal-body{min-height:700px}")),
        useShinyjs(),googleAuthUI("gauth_login"),
        footer = tagList()
      )
    }
    else{
      modalDialog(
        tags$head(tags$style(".modal-dialog{min-width:50px;}")),
        tags$head(tags$style(".modal-body{min-height:50px}")),
        useShinyjs(),googleAuthUI("gauth_login"),
        footer = tagList()
      )
    }
  }
  
  dataModal2 = function(failed = FALSE) {
    modalDialog(
      HTML('User is not authorized to view this app. If you think this is a mistake, please email wafulaer@gmail.com'),
      useShinyjs(),googleAuthUI("gauth_login"),
      footer = tagList()
    )
  }
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  ## Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-primary",
                            logout_class = "btn btn-primary")
  
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    with_shiny(user_info, shiny_access_token = accessToken())
  })
  
  obs2 <- observe({
    
    validate(
      need(userDetails(), "getting user details"))
    
    if(userDetails()$email %in% df_users$email){
      # close the log in dialog if authenticated
      obs1$suspend()
      
      # hacky way to deal with modal sizes
      showModal(dataModal(TRUE))
      removeModal()
    }
    else{
      showModal(dataModal2())
    }
  })
  
  user_name = reactive({
    userDetails()$name
  })
  
  ## Display user's Google display name after successful login
  output$logininfo <- renderUI({
    x <- user_name()
    HTML('<p>Logged in as <strong>', x,  '</p>')
  })
  
}