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



df_users <- read.csv(file = usersfilepath, stringsAsFactors = FALSE)
df_auth <- read.csv(file = gauth_detailsfilepath, stringsAsFactors = FALSE)
df_enr <- read.csv(file = enrollment_filepath, stringsAsFactors = FALSE)
df_scr <- read.csv(file = screening_filepath, stringsAsFactors = FALSE)
df_trk <- read.csv(file = track_filepath, stringsAsFactors = FALSE)
df_withdraw <- read.csv(file = withdraw_filepath, stringsAsFactors = FALSE)
df_followup <- read.csv(file = followup_filepath, stringsAsFactors = FALSE)

df_all_tables <- list(
  "enrollment" = df_enr,
  "screening"  = df_scr,
  "tracking" = df_trk,
  "withdrawal" = df_withdraw,
  "followup" = df_followup
)

# Format columns in the data set
df_enr$gender <- factor(df_enr$gender, levels = c(0,1), labels = c("Female","Male"))
df_enr$enrdate_0 <- as.Date(as.character(df_enr$enrdate), format = "%d/%m/%Y")
df_withdraw$tdate_0 <- as.Date(as.character(df_withdraw$tdate), format = "%d/%m/%Y")
df_trk$tdate_0 <- as.Date(as.character(df_trk$tdate), format = "%d/%m/%Y")
df_followup$vdate_0 <- as.Date(as.character(df_followup$vdate), format = "%d/%m/%Y")
df_scr$screendate_0 <- as.Date(as.character(df_scr$screendate), format = "%d/%m/%Y")

df_enr <- set_age_group(df_enr, df_scr)

df_enr$bmi <- apply(df_enr, 1, calculate_bmi)


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
     result <- 100*m/t
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion of Male Enrolled", icon = icon("male", lib = "glyphicon"),
       color = "aqua"
     )
   })
   
   output$hypertensive <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$ever_had_hyp==1,]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- 100*m/t
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion Hypertensive", icon = icon("male", lib = "glyphicon"),
       color = "green"
     )
   })
   
   output$diabetic <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$ever_had_diab==1,]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- 100*m/t
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion Diabetic", icon = icon("male", lib = "glyphicon"),
       color = "aqua"
     )
   })
   
   output$cholestrol <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$ever_had_chol==1,]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- 100*m/t
     valueBox(
       
       paste0(result,"%","(",m,"/",t,")"), "Proportion with High Cholestrol", icon = icon("male", lib = "glyphicon"),
       color = "green"
     )
   })
   
   output$bmi <- renderValueBox({
     m <- length(enrollment_data()[enrollment_data()$bmi>=30.0,]$studyid)
     t<-length(enrollment_data()$studyid)
     result <- 100*m/t
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
   
   output$month_1 <- renderValueBox({
     t<-length(enrollment_data()$studyid)
     n <- length(follow_up_data()[follow_up_data()$fuvnumber==1,]$studyid)
     result <- round(100 * n/t,1)
     valueBox(
       
       paste0(result,"%","(",n,"/",t,")"), "Proportion seen for Month 1 follow-up Visit",
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
      pref_choices <- c("All","Hypertensive", "Diabetic", "Cholestrol", "Obesity")
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
          filter(ever_had_hyp == 1)
      } else if(input$subcategory == "Diabetic") {
        df <- df_enr %>%
          filter(ever_had_diab == 1)
      } else if(input$subcategory == "Cholestrol") {
        df <- df_enr %>%
          filter(ever_had_chol == 1)
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
  
  # Add downloadable list
  df_temp <- reactive({
    ret_value <- enrollment_data() %>%
      select(studyid, clinicid,pinitials,gender,enrdate,dtg_start_date, year_diagnosed,year_start_art, age,height,weight,age_group,
             bmi, ever_had_hyp,ever_had_diab,ever_had_chol)
    
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
         filter(ever_had_hyp == 1)
     } else if(input$subcategory == "Diabetic") {
       dt <- df_temp() %>%
         filter(ever_had_diab == 1)
     } else if(input$subcategory == "Cholestrol") {
       dt <- df_temp() %>%
         filter(ever_had_chol == 1)
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
    m_percent <- 100*m/(f+m)
    f_percent <- 100*f/(f+m)
    HTML(paste("<font size='4'>",
               sprintf("Male %.1f",m_percent),"%", 
               sprintf("(%s/%s) and ",m,f+m),
               sprintf("Female %.1f",f_percent),"%",
               sprintf("(%s/%s) ",f,f+m),
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
    #print(data)
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
        print(paste("Screening selected", row_id))
      } else if(grepl("Followup", df_selected_item$content)) {
        print(paste("Followup selected", row_id))
        
       
        
      } else if(grepl("Withdrawal", df_selected_item$content)) {
        print(paste("Withdrawal selected", row_id))
        
        fluidRow(column(12, HTML(sprintf('<h4>Withdrawal: %s</h4>', format(selected_date(), format = '%Y-%B-%d')))),
                 column(12, htmlOutput('withdrawal_details')))
        
      } else if(grepl("Tracking", df_selected_item$content)) {
        print(paste("Tracking selected", row_id))
        fluidRow(column(12, HTML(sprintf('<h4>Tracking: %s</h4>', format(selected_date(), format = '%Y-%B-%d')))),
                 column(12, htmlOutput('tracking_details')))
        
      } else if(grepl("Moved", df_selected_item$content)) {
        print(paste("Move out selected", row_id))
        
        fluidRow(column(12, HTML(sprintf('<h4>Participant Moved: %s</h4>', format(selected_date(), format = '%Y-%B-%d')))),
                 column(12, htmlOutput('move_out_details')))
      }
      
    }
    
  })
  
  # -----------------------------------------------
  # CREATE HTML OUTPUTS FOR VARIOUS VISIT DETAILS OR TABS
  # -----------------------------------------------
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
  
  
}