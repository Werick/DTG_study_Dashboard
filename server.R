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
library(timevis) # An R package for creating timeline visualizations
library(lubridate)

# load helper function
helperfilepath <- file.path("script","helper_functions.R")
source(helperfilepath)

# Load data
usersfilepath <- file.path("data","user_credentials.csv")
gauth_detailsfilepath <- file.path("data","google_authentication.csv")
enrollment_filepath <- file.path("data", "enrollment.csv")
track_filepath <- file.path("data", "tracking.csv")
withdraw_filepath <- file.path("data", "withdrawal.csv")
screening_filepath <- file.path("data", "screening.csv")
followup_filepath <- file.path("data", "followup.csv")
lab_filepath <- file.path("data", "lab.csv")



df_users <- read.csv(file = usersfilepath, stringsAsFactors = FALSE)
df_auth <- read.csv(file = gauth_detailsfilepath, stringsAsFactors = FALSE)
df_enr <- read.csv(file = enrollment_filepath, stringsAsFactors = FALSE)
df_scr <- read.csv(file = screening_filepath, stringsAsFactors = FALSE)
df_trk <- read.csv(file = track_filepath, stringsAsFactors = FALSE)
df_withdraw <- read.csv(file = withdraw_filepath, stringsAsFactors = FALSE)
df_followup <- read.csv(file = followup_filepath, stringsAsFactors = FALSE)
df_lab <- read.csv(file = lab_filepath, stringsAsFactors = FALSE)

df_all_tables <- list(
  "enrollment" = df_enr,
  "screening"  = df_scr,
  "tracking" = df_trk,
  "withdrawal" = df_withdraw,
  "followup" = df_followup,
  "lab" = df_lab
)

# Format columns in the data set
df_enr$gender <- factor(df_enr$gender, levels = c(0,1), labels = c("Female","Male"))
df_followup$gender <- factor(df_followup$gender, levels = c(0,1), labels = c("Female","Male"))
df_enr$enrdate_0 <- as.Date(as.character(df_enr$enrdate))
df_withdraw$tdate_0 <- as.Date(as.character(df_withdraw$tdate))
df_trk$tdate_0 <- as.Date(as.character(df_trk$tdate))
df_followup$vdate_0 <- as.Date(as.character(df_followup$vdate))
df_scr$screendate_0 <- as.Date(as.character(df_scr$screendate))
df_enr$vmonth <- paste(year(as.Date(as.character(df_enr$enrdate))),month(as.Date(as.character(df_enr$enrdate)), label = TRUE), sep ="-")


df_enr <- set_age_group(df_enr, df_scr)

df_enr$bmi <- apply(df_enr, 1, calculate_bmi)
df_enr$hyp_status_0 <- apply(df_enr, 1, get_hyptension_status_baseline)
df_enr$chol_status_0 <- apply(df_enr, 1, get_cholesterol_status_baseline)
df_enr$diab_status_0 <- apply(df_enr, 1, get_diabetic_status_baseline)

df_enr_temp <- df_enr %>%
  select(studyid, height, weight)

df_followup$bmi <- apply(df_followup, 1, calculate_bmi_fu, df_en = df_enr_temp)
df_followup$weight_change <- apply(df_followup, 1, calculate_weight_change, df_en = df_enr_temp)
df_followup$hyp_status_1 <- apply(df_followup, 1, get_hyptension_status_baseline)
df_followup$chol_status_1 <- apply(df_followup, 1, get_cholesterol_status_baseline)
df_followup$diab_status_1 <- apply(df_followup, 1, get_diabetic_status_baseline)

df_retention <- create_retention_data(df_enr, df_followup)

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
  
  # get the time when the file was last modified
  date_last_updates <- reactive({
    #print(as.character(file.info(followup_filepath)$mtime))
    as.character(file.info(followup_filepath)$mtime) # Get time when the file was last modified
  })
  
  output$last_updated <- renderUI({
       HTML('<p>Date Last Updated: <strong>', date_last_updates(),  '</p>')
     })
  
  # For this search form, the corresponding values in the server-side code would be input$searchText and input$searchButton.
  
  ## Initialization of variables
  studyid_select = reactiveVal(NULL)
  
  
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
  # VALUE BOXES ON THE MAIN/ENROLLMENT TAB
  # -------------------------------------------   
   output$screened <- renderValueBox({
     valueBox(
       paste0(length(screening_data()$studyid)), "Number Screened", icon = icon("list"),
       color = "aqua"
     )
   })
   
   output$enrolled <- renderValueBox({
     e <- length(enrollment_data()$studyid)
     s <- length(screening_data()$studyid)
     result <- round(100*e/s,1)
     valueBox(
       paste0(result,"%","(",e,"/",s,")"), "Proportion Enrolled", icon = icon("thumbs-up", lib = "glyphicon"),
       color = "green"
     )
   })
   
  
   output$male <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$gender=="Male",]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- round(100*m/t,1)
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion of Male Enrolled", icon = icon("male", lib = "glyphicon"),
       color = "aqua"
     )
   })
   
   output$hypertensive <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$hyp_status_0=="Hypertensive",]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- round(100*m/t,1)
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion Hypertensive", 
       color = "green"
     )
   })
   
   output$diabetic <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$diab_status_0=='Diabetic',]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- round(100*m/t,1)
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion Diabetic", 
       color = "aqua"
     )
   })
   
   output$cholesterol <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$chol_status_0=='High Cholesterol',]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- round(100*m/t,1)
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion with High Cholesterol", icon = icon("male", lib = "glyphicon"),
       color = "green"
     )
   })
   
   output$bmi <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$bmi>=30.0,]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- round(100*m/t,1)
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion with Obesity",
       color = "green"
     )
   })
   
   
   # -------------------------------------------
   # VALUE BOXES ON THE Follow-up TAB
   # -------------------------------------------
   follow_up_data <- reactive({
     ret_val <- df_followup
   })
   
   output$weight_1 <- renderValueBox({
     t<-length(enrollment_data()$studyid)
     
     n <- follow_up_data() %>%
       filter(weight_change >= 5.0 | weight_change >= -5.0) %>%
       filter(!duplicated(studyid)) %>%
       count()
       
     #length(follow_up_data()[follow_up_data()$weight_change>=5.0,]$studyid)
     
     result <- round(100 * n/t,1)
     valueBox(
       
       paste0(result,"%","(",n,"/",t,")"), "At least -5% or 5% Weight Change ",
       color = "green"
     )
   })
   
   output$month_3 <- renderValueBox({
     t<-length(enrollment_data()$studyid)
     n <- length(follow_up_data()[follow_up_data()$fuvnumber==3,]$studyid)
     result <- round(100 * n/t,1)
     valueBox(
       
       paste0(result,"%","(",n,"/",t,")"), "Proportion seen for Month 3 follow-up Visit",
       color = "green"
     )
   })
   
   output$month_6 <- renderValueBox({
     t<-length(enrollment_data()$studyid)
     n <- length(follow_up_data()[follow_up_data()$fuvnumber==6,]$studyid)
     result <- round(100 * n/t,1)
     valueBox(
       
       paste0(result,"%","(",n,"/",t,")"), "Proportion seen for Month 6 follow-up Visit",
       color = "green"
     )
   })
   
   output$new_hypertenstion <- renderValueBox({
     # get all non hypertensive at baseline or enrollment
     t <- enrollment_data() %>%
       filter(hyp_status_0 == "Not Hypertensive") %>%
       count()
     
     known_hyp_df <- enrollment_data() %>%
       filter(hyp_status_0 == "Hypertensive")
     
     n <- follow_up_data() %>%
       filter((hyp_status_1 == "Hypertensive"), !(studyid %in% known_hyp_df$studyid)) %>%
       arrange(desc(studyid)) %>%
       filter(!duplicated(studyid)) %>%
       count()
     result <- round(100 * n/t,1)
     
     valueBox(
       
       paste0(result,"%","(",n,"/",t,")"), "New Hypertensive",
       color = "yellow"
     )
   })
   
   output$new_diabetic <- renderValueBox({
     
     t<-enrollment_data() %>%
       filter(diab_status_0 %in% c("Not Diabetic", "Unknown")) %>%
       count()
     
     known_diab_df <- enrollment_data() %>%
       filter(diab_status_0 == "Diabetic")
     
     n <- follow_up_data() %>%
       filter(diab_status_1 == "Diabetic", !(studyid %in% known_diab_df$studyid)) %>%
       arrange(desc(studyid)) %>%
       filter(!duplicated(studyid)) %>%
       count()
     result <- round(100 * n/t,1)
     valueBox(
       
       paste0(result,"%","(",n,"/",t,")"), "New Diabetic",
       color = "yellow"
     )
     
   })
   
   
   output$new_cholesterol <- renderValueBox({
     t <- enrollment_data() %>%
       filter(chol_status_0 %in% c("Not High Cholesterol")) %>%
       count()
     
     known_chol_df <- enrollment_data() %>%
       filter(chol_status_0 == "High Cholesterol")
     
     n <- follow_up_data() %>%
       filter(chol_status_1 == "High Cholesterol", !(studyid %in% known_chol_df$studyid)) %>%
       arrange(desc(studyid)) %>%
       filter(!duplicated(studyid)) %>%
       count()
     
     result <- round(100 * n/t,1)
     valueBox(
       
       paste0(result,"%","(",n,"/",t,")"), "New High Cholesterol",
       color = "yellow"
     )
   })
   # -------------------------------------------
   # DATA TABLES FOR FOLLOW_UP TAB
   # -------------------------------------------   
   # Add downloadable list
   df_temp_fu <- reactive({
     ret_value <- df_followup %>%
       select(studyid, clinicid,gender,vdate,fuvnumber, weight_change,weight, hyp_status_1,diab_status_1,next_visit_date,
              chol_status_1, bmi 
              )
     
   })
   
   output$fu_visits <- DT::renderDataTable({
     dt <- df_temp_fu()
     
     if( input$fu_visit_selection == 'Month 1') {
       dt <- df_temp_fu() %>%
         filter(fuvnumber == 1)
     } else if( input$fu_visit_selection == 'Month 3') {
       dt <- df_temp_fu() %>%
         filter(fuvnumber == 3)
     } else if( input$fu_visit_selection == 'Month 6') {
       dt <- df_temp_fu() %>%
         filter(fuvnumber == 6)
     } else {
       dt <- df_temp_fu()
     }
       
     
     #add functionality to the download button
     datatable(dt,
               escape=F, selection = 'none', filter = 'top', rownames = FALSE,
               callback = JS("$('div.dwnld').append($('#fu_download'));"),
               extensions = 'Buttons')
   })
   
   output$fu_visits_incidence <- DT::renderDataTable({
     dt <- df_temp_fu()
     
     if (input$fu_incidence_selection == "Hypertensive") {
       
       dt <- df_temp_fu() %>%
         filter(hyp_status_1 == "Hypertensive")
       
     } else if (input$fu_incidence_selection == "Diabetic") {
       
       dt <- df_temp_fu() %>%
         filter(diab_status_1 == "Diabetic")
       
     } else if (input$fu_incidence_selection == "High Cholesterol") {
       
       dt <- df_temp_fu() %>%
         filter(chol_status_1 == "High Cholesterol")
       
     } else if (input$fu_incidence_selection == "Obesity") {
       
       dt <- df_temp_fu() %>%
         filter(bmi >=30)
       
     } else if (input$fu_incidence_selection == "Overweight") {
       
       dt <- df_temp_fu() %>%
         filter(bmi>=25)
       
     }  else if (input$fu_incidence_selection == "Weight Change") {
       
       dt <- df_temp_fu() %>%
         filter(weight_change>=5.0 | weight_change <= -5.0)
       
     } else {
       dt <- df_temp_fu()
     }
     
     #add functionality to the download button
     datatable(dt,
               rownames = FALSE,
               callback = JS("$('div.dwnld').append($('#fu_incidence_download'));"),
               extensions = 'Buttons')
   })
  
   
   # Code to manage download for fu incidence
   output$fu_incidence_download <- downloadHandler(
     filename = function() {
       paste('fu_list1-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       write.csv(df_temp_fu(), con)
     }
   )
   
   # Code to manage download for fu visits
   output$fu_download <- downloadHandler(
     filename = function() {
       paste('fu_list2-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       write.csv(df_temp_fu(), con)
     }
   )
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
    pref_choices <- c()
    select_label <- ""
    selected_option <- input$breakdown
    if(selected_option == "Gender") {
      pref_choices <- c("All","Male", "Female")
      select_label <- "Select Gender"
    } else if (selected_option == "Age-group") {
      pref_choices <- c("All","25-34", "35-44", "45-54", "55-64", "65+")
      select_label <- "Select Age Group"
    } else if (selected_option == "Pre-conditions"){
      pref_choices <- c("All","Hypertensive", "Diabetic", "Cholesterol", "Obesity")
      select_label <- "Select Pre-Existing Conditions"
    }else {
      pref_choices <- c("All")
      select_label <- ""
    }
    
    updateSelectInput(session, "subcategory",
                      label = select_label,
                      choices = pref_choices,
                      selected = head(pref_choices, 1))
  })
  
  
  ## Enrollment graph by Gender/sex
  
  output$plot1_enrollment <- renderPlotly({
    df <- df_enr #get_dashboard_data(df_enr, input$breakdown,input$subcategory)
    
    if( input$breakdown == "Gender") {
      if(input$subcategory != "All") {
        df <- df_enr %>%
          filter(gender == input$subcategory)
      }
      
    } else if (input$breakdown == "Age-group") {
      if( input$subcategory != "All") {
        df <- df_enr %>%
          filter(age_group == input$subcategory)
      }
      
    } else if (input$breakdown == "Pre-conditions") {
      if(input$subcategory == "Hypertensive") {
        df <- df_enr %>%
          filter(hyp_status_0 == "Hypertensive")
      } else if(input$subcategory == "Diabetic") {
        df <- df_enr %>%
          filter(diab_status_0 == "Diabetic")
      } else if(input$subcategory == "Cholesterol") {
        df <- df_enr %>%
          filter(chol_status_0 == "High Cholesterol")
      } else if(input$subcategory == "Obesity") {
        df <- df_enr %>%
          filter(bmi >= 30.0)
      } 
    }
    
      
      
    
    p<-ggplot(df, aes(x=gender,fill=gender)) +
      xlab("") +
      geom_bar(stat = "count") +
      theme_minimal()
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  output$plot2_enrollment <- renderPlotly({
    df <- df_enr #get_dashboard_data(df_enr, input$breakdown,input$subcategory)
    
    if( input$breakdown == "Gender") {
      if(input$subcategory != "All") {
        df <- df_enr %>%
          filter(gender == input$subcategory)
      }
      
    } else if (input$breakdown == "Age-group") {
      if( input$subcategory != "All") {
        df <- df_enr %>%
          filter(age_group == input$subcategory)
      }
      
    } else if (input$breakdown == "Pre-conditions") {
      if(input$subcategory == "Hypertensive") {
        df <- df_enr %>%
          filter(hyp_status_0 == "Hypertensive")
      } else if(input$subcategory == "Diabetic") {
        df <- df_enr %>%
          filter(diab_status_0 == "Diabetic")
      } else if(input$subcategory == "Cholesterol") {
        df <- df_enr %>%
          filter(chol_status_0 == "High Cholesterol")
      } else if(input$subcategory == "Obesity") {
        df <- df_enr %>%
          filter(bmi >= 30.0)
      } 
    }
    
    
    
    
    p <- ggplot(df, aes(x=vmonth,fill=gender)) +
      geom_bar(stat = "count") +
      xlab("Visit Month") +
      ylab("Number Enrolled") +
      ggtitle("Monthly enrollment summary") +
      theme_minimal()
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  # Add downloadable list
  df_temp <- reactive({
    ret_value <- enrollment_data() %>%
      select(studyid, clinicid,pinitials,gender,enrdate,dtg_start_date, year_diagnosed,year_start_art, age,height,weight,age_group,
             bmi, hyp_status_0,diab_status_0,chol_status_0)
    
  })
  
  output$enrollment_list <- DT::renderDataTable({
   dt <- df_temp() #get_dashboard_data(df_temp(), input$breakdown,input$subcategory)
   
   if( input$breakdown == "Gender") {
     if(input$subcategory != "All") {
       dt <- df_temp() %>%
         filter(gender == input$subcategory)
     }
     
   } else if (input$breakdown == "Age-group") {
     if( input$subcategory != "All") {
       dt <- df_temp() %>%
         filter(age_group == input$subcategory)
     }
     
   } else if (input$breakdown == "Pre-conditions") {
     if(input$subcategory == "Hypertensive") {
       dt <- df_temp() %>%
         filter(hyp_status_0 == "Hypertensive")
     } else if(input$subcategory == "Diabetic") {
       dt <- df_temp() %>%
         filter(diab_status_0 == "Diabetic")
     } else if(input$subcategory == "Cholesterol") {
       dt <- df_temp() %>%
         filter(chol_status_0 == "High Cholesterol")
     } else if(input$subcategory == "Obesity") {
       dt <- df_temp() %>%
         filter(bmi >= 30.0)
     } 
   }
   
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
    
    m <- ifelse(is.na(as.integer(m)),0,as.integer(m))
    f <- ifelse(is.na(as.integer(f)),0,as.integer(f))
    #print(paste(m,f))
    t <- f + m
    
    m_percent <- 100*m/t
    f_percent <- 100*f/t
    HTML(paste("<font size='4'>",
               sprintf("Male %.1f",m_percent),"%", 
               sprintf("(%s/%s) and ",m,t),
               sprintf("Female %.1f",f_percent),"%",
               sprintf("(%s/%s) ",f,t),
               "</font><br><br>"))
  })
  
  
  # ------------------------------------------
  # SHOW MODAL FROM SEARCH BOX
  # -------------------------------------------
  
  observeEvent(input$searchButton,{
    studyid_select(input$searchText)
    showModal(patient_details_modal())
  })
  
  
  # -------------------------------------------
  # PATIENT DETAILS MODAL DIALOG
  # -------------------------------------------
  
  patient_details_modal <- reactive({
    if (length(df_enr[which(tolower(df_enr$studyid)  == tolower(studyid_select())),]$studyid) == 0){
      # Display this page if no matching record
      modalDialog(
        # title = "Participant Not Found",
        fluidPage(
        
        HTML(sprintf("<h4>Study ID %s Not Found</h4>",toupper(studyid_select())))
      ), footer = tagList(actionButton("dismiss_modal",label = "Close")), easyClose = TRUE)
    } else {
      # Display the page for displaying details
      modalDialog(
        fluidPage(
          uiOutput("patient_header"),
          htmlOutput('demographics'),
          timevisOutput("timeline"),
          htmlOutput("select_visit"),
          htmlOutput('visit_details')
          
        ), footer = tagList(actionButton("dismiss_modal",label = "Close")) , size = 'l', easyClose = TRUE
      )
      
    }
    
  })
  
  # hide the modal page with the patient details
  observeEvent(input$dismiss_modal, {
    removeModal()
  })
  
  ## ------------------------------------------
  ## TIME VISUALIZATION DATA FRAME
  ##-------------------------------------------
  df_timeline_viz_data <- reactive({
    data <- create_timeline_df(df_scr, df_enr, df_followup, df_withdraw, df_trk, studyid_select())
    print(data)
  })
  
  ## ------------------------------------------
  ## ADD DETAILS TO THE MODAL PAGE
  ## ------------------------------------------
  ## Patient Header
  output$patient_header <- renderUI({
    h3(strong(sprintf("Patient: %s", toupper(studyid_select()))),align="center")
  })
  
  # Demographic details
  output$demographics = renderUI({
    demographics = get_demographic_data(df_enr, studyid_select())
    #print(demographics)
    fluidPage(fluidRow(
      HTML('<br>'),
      column(6, HTML(demographics[[1]])),
      column(6, HTML(demographics[[2]]))
    ))
  })
  
  # Time visualization
  # create timeline, see helper function below
  output$timeline = renderTimevis({
    #print(df_timeline_viz_data())
    timevis(df_timeline_viz_data())
  })
  
  # VISITS LIST
  output$select_visit <- renderUI({
    tvisit_list <- create_visit_list(df_timeline_viz_data())
    selectInput('visit_list', label = "Select Visit", choices = tvisit_list$choices)
  })
  
  observe({
    setSelection('timeline', input$visit_list)
  }) 
  
  observe({
    updateSelectInput(session, 'visit_list', selected= as.numeric(input$timeline_selected))
  })
  
  # select date on timeline
  selected_date = reactive({
    as.Date(df_timeline_viz_data()[df_timeline_viz_data()$id ==input$visit_list,]$start)
  })
  
  # -----------------------------------------------------------------
  # GENERATES VISIT DETAILS BASED ON DATE SELECTED AND TYPE OF VISIT
  # -----------------------------------------------------------------
  output$visit_details <- renderUI({
    row_id = as.integer(input$visit_list)
    # print(paste("This is what has been selected", row_id))
    df_selected_item <- df_timeline_viz_data()[df_timeline_viz_data()$id == row_id,]
      #filter()
    # print(df_selected_item)
    if(nrow(df_selected_item)>0) {
      if(grepl("Enrollment", df_selected_item$content)) {
        #print(paste("Enrollment selected", row_id))
        
        details_header <- sprintf("<h4>Enrollment Visit Details: %s </h4>",format(selected_date(), format = '%Y-%B-%d'))
        
        fluidRow(
          column(12, HTML(details_header),
                 tabsetPanel(id = 'tabs', type = 'tabs',
                             tabPanel('Hiv History and Symptoms', htmlOutput('hiv_history')),
                             tabPanel('ART Adherence and Medical History', htmlOutput('art_meds')),
                             tabPanel('Food Security', htmlOutput('food_security')),
                             tabPanel('Reproductive History', htmlOutput('rep_history')),
                             tabPanel('Baseline Data', htmlOutput('baseline')),
                             selected = input$tabs
                             )
                 )
        )
      } else if(grepl("Screening", df_selected_item$content)) {
        
        details_header <- sprintf("<h4>Screening Details: %s </h4>",format(selected_date(), format = '%Y-%B-%d'))        
        fluidRow(column(12, HTML(details_header),
                        tabsetPanel(id = 'tabs', type = 'tabs',
                                    tabPanel('Basic Information', htmlOutput('basic_scr_info')),
                                    selected = input$tabs
                                    )))
        
      } else if(grepl("Followup", df_selected_item$content)) {
        
        details_header <- sprintf("<h4>Followup Visit Details: %s </h4>",format(selected_date(), format = '%Y-%B-%d'))
        
        fluidRow(
          column(12, HTML(details_header),
                 tabsetPanel(id = 'tabs', type = 'tabs',
                             tabPanel('Symptoms and ART Adherence', htmlOutput('fu_art_sym')),
                             tabPanel('Food Security', htmlOutput('fu_food_security')),
                             tabPanel('Reproductive History', htmlOutput('rep_history_fu')),
                             tabPanel('Follow-up Biometric Data', htmlOutput('fu_info')),
                             selected = input$tabs
                 )
          )
        )
        
       
        
      } else if(grepl("Withdrawal", df_selected_item$content)) {
        print(paste("Withdrawal selected", row_id))
        
        fluidRow(column(12, HTML(sprintf('<h4>Withdrawal: %s</h4>', format(selected_date(), format = '%Y-%B-%d')))),
                 column(12, htmlOutput('withdrawal_details')))
        
      } else if(grepl("Tracking", df_selected_item$content)) {
        print(paste("Tracking selected", row_id))
        fluidRow(column(12, HTML(sprintf('<h4>Tracking: %s</h4>', format(selected_date(), format = '%Y-%B-%d')))),
                 column(12, htmlOutput('tracking_details')))
        
      } else if(grepl("Moved", df_selected_item$content)) {
        #print(paste("Move out selected", row_id))
        
        fluidRow(column(12, HTML(sprintf('<h4>Participant Moved: %s</h4>', format(selected_date(), format = '%Y-%B-%d')))),
                 column(12, htmlOutput('move_out_details')))
      }
      
    }
    
  })
  
  # ------------------------------------------------------------------
  # CREATE HTML OUTPUTS FOR VARIOUS ENROLLMENT VISIT DETAILS OR TABS
  # ------------------------------------------------------------------
  output$hiv_history <- renderUI({
  hivhistory <- get_hiv_history(df_enr, studyid_select())
  HTML(hivhistory)
  })
  
  output$art_meds <- renderUI({
    meds_history <- get_medication_history(df_enr, studyid_select())
    fluidPage(fluidRow(
      HTML('<br>'),
      column(4, HTML(meds_history[[1]])),
      column(4, HTML(meds_history[[2]])),
      column(4, HTML(meds_history[[3]]))
      ))
  })
  
  
  output$food_security <- renderUI({
    food_details <- get_food_security(df_enr, studyid_select())
    
    fluidPage(fluidRow(
      HTML('<br>'),
      column(6, HTML(food_details[[1]])),
      column(6, HTML(food_details[[2]]))
    ))
  })
  
  output$rep_history <- renderUI({
    reproductive_history <- get_reproductive_history(df_enr, studyid_select())
    HTML(reproductive_history)
  })
  
  output$baseline <- renderUI({
    baseline_info <- get_enrollment_details(df_enr, studyid_select())
    
    fluidPage(fluidRow(
      HTML('<br>'),
      column(6, HTML(baseline_info[[1]])),
      column(6, HTML(baseline_info[[2]]))
    ))
  })
  
  
  # ------------------------------------------------------------------
  # CREATE HTML OUTPUTS FOR VARIOUS FOLLOW-UP VISIT DETAILS OR TABS
  # ------------------------------------------------------------------
  
  output$fu_food_security <- renderUI({
    food_details <- get_food_security(df_followup,studyid_select(),'f', selected_date())
    
    fluidPage(fluidRow(
      HTML('<br>'),
      column(6, HTML(food_details[[1]])),
      column(6, HTML(food_details[[2]]))
    ))
  })
  
  
  output$rep_history_fu <- renderUI({
    reproductive_history <- get_reproductive_history_fu(df_followup, studyid_select(), selected_date())
    HTML(reproductive_history)
  })
  
  
  output$fu_info <- renderUI({
    baseline_info <- get_followup_details(df_followup, studyid_select(), selected_date())
    
    fluidPage(fluidRow(
      HTML('<br>'),
      column(6, HTML(baseline_info[[1]])),
      column(6, HTML(baseline_info[[2]]))
    ))
  })
  
  output$fu_art_sym <- renderUI({
    sym_art_adherence <- get_fu_sysmptom_n_art_adherence(df_followup, studyid_select(), selected_date())
    HTML(sym_art_adherence)
  })
  
  
  # ---------------------------------------
  # CREATE HTML OUTPUTS FOR SCREENING INFO
  # ---------------------------------------
  
  output$basic_scr_info <- renderUI({
    basic_data <- get_screening_info(df_scr, studyid_select())
    fluidPage(fluidRow(
      HTML('<br>'),
      column(6, HTML(basic_data[[1]])),
      column(6, HTML(basic_data[[2]]))
    ))
  })
  
  
  # -------------------------------------------
  # WITHDRAWAL/MOVE OUT DETAILS
  # -------------------------------------------
  output$move_out_details <- renderUI({
    move <- get_move_out_info(df_withdraw, studyid_select())
    HTML(move)
  })
  
  
  # -------------------------------------------
  # TRACKING DETAILS
  # -------------------------------------------
  output$tracking_details <- renderUI({
    track_info <- get_tracking_info(df_trk, studyid_select(), selected_date())
    HTML(track_info)
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
  
  # followup
  output$download_fup <- downloadHandler(
    filename = function() {
      paste('followup-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_all_tables$followup, con)
    }
  )
  
  # Tracking
  output$download_tracking <- downloadHandler(
    filename = function() {
      paste('tracking-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_all_tables$tracking, con)
    }
  )
  
  # enrollent
  output$download_withdrawal <- downloadHandler(
    filename = function() {
      paste('withdraw_move-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_all_tables$withdrawal, con)
    }
  )
  
  
  #----------------------------------------------------------
  # RETENTION UI
  #----------------------------------------------------------
  
  # -------------------------------------------
  # VALUE BOXES ON THE RETENTION TAB
  # -------------------------------------------   
  output$retention_gender <- renderValueBox({
    dt <- df_retention %>%
      filter(!duplicated(studyid))
    
    df_retained <- dt %>%
      filter(gender == 'Male')
    
    proption <- round(100 * length(df_retained$studyid)/ length(dt$studyid),1)
    
    valueBox(
      paste0(proption,"%(",length(df_retained$studyid),"/",length(dt$studyid),")"), "Male Proportion Retained",
      color = "aqua"
    )
  })
  
  output$retention_hyp <- renderValueBox({
    dt <- df_retention %>%
      filter(!duplicated(studyid))
    
    df_retained <- dt %>%
      filter(hyp_status_1 == 'Hypertensive')
    
    proption <- round(100 * length(df_retained$studyid)/ length(dt$studyid),1)
    
    valueBox(
      paste0(proption,"%(",length(df_retained$studyid),"/",length(dt$studyid),")"), "Hypertensive Proportion Retained",
      color = "aqua"
    )
  })
  
  output$retention_Summary_prop <- renderValueBox({
    dt <- df_retention %>%
      filter(!duplicated(studyid))
    
    df_retained <- dt %>%
      filter(retention_status == 'Retained')
    
    proption <- round(100 * length(df_retained$studyid)/ length(dt$studyid),1)
    valueBox(
      paste0(proption,"%(",length(df_retained$studyid),"/",length(dt$studyid),")"), "Proportion Retained",
      color = "aqua"
    )
  })
  
  ## Get Retention Plot
  
  output$plot3_retention <- renderPlotly({
    df <- df_retention
    
    
    p<-ggplot(df, aes(x=retention_status,fill=gender)) +
      xlab("") +
      geom_bar(stat = "count") +
      theme_minimal()
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  
  ## Get Retention Plot
  
  output$plot4_retention <- renderPlotly({
    df <- df_retention
    
    
    p<-ggplot(df, aes(x=retention_status,fill=hyp_status_1)) +
      xlab("") +
      geom_bar(stat = "count") +
      theme_minimal()
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Retention table and download code
  output$retention_list <- DT::renderDataTable({
    dt <- df_retention #get_dashboard_data(df_temp(), input$breakdown,input$subcategory)
    
    
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#download1_ret'));"),
              extensions = 'Buttons')
  })
  
  # retention List
  output$download1_ret <- downloadHandler(
    filename = function() {
      paste('retention_list1-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df_retention, con)
    }
  )
  
  #----------------------------------------------------------
  # SCHEDULED/MISSED VISITS UI
  #----------------------------------------------------------
  
  df_scheduled <- reactive({
    dt <- createSchedule(df_enr, df_trk, df_followup)
  })
  
  # Scheduled visits table and download code
  output$sch_visits <- DT::renderDataTable({
    dt <- df_scheduled()
    dt <- dt %>%
      filter(between(as.Date(as.character(nextvisit)), as.Date(as.character(input$svisit_since)),
                     as.Date(as.character(input$svisit_to))))
    
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#sch_download'));"),
              extensions = 'Buttons')
  })
  
  # retention List
  output$sch_download <- downloadHandler(
    
    filename = function() {
      paste('scheduled-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      dt <- createSchedule(df_enr, df_trk, df_followup)
      dt <- dt %>%
        filter(between(as.Date(as.character(nextvisit)), as.Date(as.character(input$svisit_since)),
                       as.Date(as.character(input$svisit_to))))
      write.csv(dt, con)
    }
  )
  
  # Scheduled visits table and download code
  output$m_visits <- DT::renderDataTable({
    df_scheduled <- df_scheduled()
    
    dt <- getMissedVisits(df_followup, df_trk, df_scheduled)
    dt <- dt %>%
      filter(between(as.Date(as.character(missedvisitdate)), as.Date(as.character(input$mvisit_since)),
                     as.Date(as.character(input$mvisit_to))))
    
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#sch_download'));"),
              extensions = 'Buttons')
  })
  
  #----------------------------------------------------------------------------
  # LAB QC REPORTS
  #----------------------------------------------------------------------------
  lab_qc <- reactive({
    ret_val <- baseline_lab_qc(df_enr, df_lab)
  })
  
  # Baseline hemoglobin 1AC
  output$hemoglobin <- DT::renderDataTable({
    col_order <- c("studyid","gender", "pinitials", "enrdate","daterequested","hemoglobinA1C_clinic", "hemoglobinA1C_lab")
    
    dt <- lab_qc()%>%
      filter(as.double(hemoglobinA1C_clinic) != as.double(hemoglobinA1C_lab))
    
    dt <- dt[, col_order]
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#download_hgb'));"),
              extensions = 'Buttons')
    
  })
  
  # Baseline Fasting blood sugar
  output$fbs <- DT::renderDataTable({
    col_order <- c("studyid","gender", "pinitials", "enrdate","daterequested","fasting_bloodsugar_clinc", "fasting_bloodsugar_lab")
    
    dt <- lab_qc()%>%
      filter(as.double(fasting_bloodsugar_clinc) != as.double(fasting_bloodsugar_lab))
    
    dt <- dt[, col_order]
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#download_fbs'));"),
              extensions = 'Buttons')
    
  })
  
  # Baseline Fasting total cholesterol
  output$tchol <- DT::renderDataTable({
    col_order <- c("studyid","gender", "pinitials", "enrdate","daterequested","total_cholesterol_clinic",
                   "total_cholesterol_lab")
    
    dt <- lab_qc()%>%
      filter(as.double(total_cholesterol_clinic) != as.double(total_cholesterol_lab))
    
    dt <- dt[, col_order]
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#download_ftchol'));"),
              extensions = 'Buttons')
    
  })
  
  # Baseline Fasting HDL cholesterol
  output$hdlchol <- DT::renderDataTable({
    col_order <- c("studyid","gender", "pinitials", "enrdate","daterequested","HDL_cholesterol_clinic", "HDL_cholesterol_lab")
    
    dt <- lab_qc()%>%
      filter(as.double(HDL_cholesterol_clinic) != as.double(HDL_cholesterol_lab))
    
    dt <- dt[, col_order]
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#download_fhdlchol'));"),
              extensions = 'Buttons')
    
  })
  
  # Baseline Fasting Triglycerides
  output$ftrig <- DT::renderDataTable({
    col_order <- c("studyid","gender", "pinitials", "enrdate","daterequested","Triglycerides_clinic",
                   "Triglycerides_lab")
    
    dt <- lab_qc()%>%
      filter(as.double(Triglycerides_clinic) != as.double(Triglycerides_lab))
    
    dt <- dt[, col_order]
    #add functionality to the download button
    datatable(dt,
              callback = JS("$('div.dwnld').append($('#download_trigchol'));"),
              extensions = 'Buttons')
    
  })
  
  
}