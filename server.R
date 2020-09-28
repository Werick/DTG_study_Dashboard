# Server
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(googleAuthR)
library(shinyjs)
library(plotly)
library(DT)

# load helper function
helperfilepath <- file.path("script","helper_functions.R")
source(helperfilepath)

# Load data
usersfilepath <- file.path("data","user_credentials.csv")
gauth_detailsfilepath <- file.path("data","google_authentication.csv")
enrollment_filepath <- file.path("data", "enrollment.csv")
track_filepath <- file.path("data", "track.csv")
withdraw_filepath <- file.path("data", "withdrawal.csv")
screening_filepath <- file.path("data", "screening.csv")
followup_filepath <- file.path("data", "followup.csv")



df_users <- read.csv(file = usersfilepath, stringsAsFactors = FALSE)
df_auth <- read.csv(file = gauth_detailsfilepath, stringsAsFactors = FALSE)
df_enr <- read.csv(file = enrollment_filepath, stringsAsFactors = FALSE)
df_scr <- read.csv(file = screening_filepath, stringsAsFactors = FALSE)

df_all_tables <- list(
  "enrollment" = df_enr,
  "screening"  = df_scr
)

# Format columns in the data set
df_enr$gender <- factor(df_enr$gender, levels = c(0,1), labels = c("Female","Male"))

df_enr <- set_age_group(df_enr, df_scr)


# guidance og google authentication is availabe here https://lmyint.github.io/post/shiny-app-with-google-login/
# and https://gist.github.com/explodecomputer/ef4341872582719d6b73
# ------------------------------------------

# GOOGLE AUTHENTICATION Steps
# 


#Important details for GOOGle authenation

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = df_auth[1,'client_id'])
options("googleAuthR.webapp.client_secret" = df_auth[1,'client_secret'])



# user info after google authentication
get_user_info <- function(){
  f <- gar_api_generator("https://www.googleapis.com/oauth2/v1/userinfo",
                         "GET",
                         data_parse_function = function(x) x
  )
  f()
}


server <- function(input, output, session) {
  # For this search form, the corresponding values in the server-side code would be input$searchText and input$searchButton.
  
  # This section of the code is adopted from James Peng's authentication and slightly 
  # modified to fit this application
  
  # -------------------------------------------
  # AUTHENTICATION
  # -------------------------------------------
  
  ## this part of the code creates a modal dialog login in page
  
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

  # obs1 <- observe({
  #   showModal(dataModal())
  # })
  
  ## Authentication
  # accessToken <- callModule(googleAuth, "gauth_login",
  #                           login_class = "btn btn-primary",
  #                           logout_class = "btn btn-primary")
  
  # userDetails <- reactive({
  #   validate(
  #     need(accessToken(), "not logged in")
  #   )
  #   with_shiny(get_user_info, shiny_access_token = accessToken())
  # })
  # 
  # obs2 <- observe({
  #   
  #   validate(
  #     need(userDetails(), "getting user details"))
  #   
  #   if(userDetails()$email %in% df_users$email){
  #     # close the log in dialog if authenticated
  #     obs1$suspend()
  #     
  #     # hacky way to deal with modal sizes
  #     showModal(dataModal(TRUE))
  #     removeModal()
  #   }
  #   else{
  #     showModal(dataModal2())
  #   }
  # })
  # 
  
  # user_name = reactive({
  #   userDetails()$name
  # })
  
  ## Display user's Google display name after successful login
  
  # output$logininfo <- renderUI({
  #   x <- user_name()
  #   HTML('<p>Logged in as <strong>', x,  '</p>')
  # })
  
  
  # -------------------------------------------
  # SCREENING AND ENROLLMENT Details
  # -------------------------------------------
  
  ## Get enrollment and screening data
  enrollment_data <- reactive({
    ret_value <- df_enr
  })
  
  screening_data <- reactive({
    ret_value <- df_scr
  })
  
  gender_summary <- reactive({
    ret_value <- df_enr %>%
      group_by(gender) %>%
      summarise(N=n()) %>%
      mutate(p = N/sum(N))
  })
  
  output$total_screened <- renderUI({
    scr_count <- length(screening_data()$studyid)
    
    HTML(paste("<font size='4'>", sprintf("There are %s Participants Screened so far.",
                                          scr_count),"</font><br><br>"))
  })
  
  output$enroll_progress <- renderUI({
    enr_count = length(enrollment_data()$studyid)
    scr_count = length(screening_data()$studyid)
    
    if (scr_count != 0){
      #progressGroup('Enrollment and Screening', enr_count, 0, scr_count)
      p <- 100*enr_count/scr_count
      HTML(paste("<font size='4'>", sprintf("%.2f",p),"%",sprintf("(%s/%s) Participants Have been Enrolled",
                                             enr_count,scr_count),"</font><br><br>"))
    }
  })
  
  ## Check the user selection of breakdown input and update the subcategory
  observeEvent(input$breakdown, {
    pref_choices <- c("Male", "Female")
    selected_option <- input$breakdown
    if(selected_option == "Gender") {
      updateSelectInput(session, "subcategory",
                        label = "Select Gender",
                        choices = pref_choices,
                        selected = head(pref_choices, 1))
    } else if (selected_option == "Age-group") {
      pref_choices <- c("25-34", "35-44", "45-54", "55-64", "65+")
      updateSelectInput(session, "subcategory",
                        label = "Select Age Group",
                        choices = pref_choices,
                        selected = head(pref_choices, 1))
    }
  })
  
  ## Enrollment graph by Gender/sex
  output$plot1_enrollment <- renderPlotly({
    p<-ggplot(df_enr, aes(x=gender,fill=gender)) +
      xlab("") +
      geom_bar(stat = "count") +
      theme_minimal()
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Add downloadable list
  df_temp <- reactive({
    ret_value <- enrollment_data() %>%
      select(studyid, clinicid,pinitials,gender,enrdate,dtg_start_date, year_diagnosed,year_start_art, age,height,weight,age_group)
    
  })
  output$enrollment_list <- DT::renderDataTable({
   dt <- df_temp()
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#download1'));"),
              extensions = 'Buttons')
  })
  
  # Enrollment List
  output$download1 <- downloadHandler(
    filename = function() {
      paste('enrollment_list1-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_temp, con)
    }
  )
  
  
  output$enroll_text_summary <- renderUI({
    m <-gender_summary()[gender_summary()$gender=='Male','N']
    f <-gender_summary()[gender_summary()$gender=='Female','N']
    m_percent <- 100*m/(f+m)
    f_percent <- 100*f/(f+m)
    HTML(paste("<font size='4'>",
               sprintf("Male %.1f",m_percent),"%", 
               sprintf("(%s/%s) and ",m,f+m),
               sprintf("Female %.1f",f_percent),"%",
               sprintf("(%s/%s) ",f,f+m),
               "</font><br><br>"))
  })
  # -------------------------------------------
  # DOWNLOAD FOR VARIOUS RAW DATASETS
  # -------------------------------------------
  
  # guidance on how to work with buttons
  # https://shiny.rstudio.com/reference/shiny/1.0.4/downloadButton.html
  
  # Screening
  output$download_screening <- downloadHandler(
    filename = function() {
      paste('screening-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_all_tables$screening, con)
    }
  )
  
  # enrollent
  output$download_enrollment <- downloadHandler(
    filename = function() {
      paste('enrollment-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_all_tables$enrollment, con)
    }
  )
  
  
}