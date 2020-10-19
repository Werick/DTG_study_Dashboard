# UI
# DTG STUDY UI for the Dashboard
# Created on 24/09/2020
# Developed by Wafula Erick
# This dashboard has been adopted from what we currently have from Search Youth Thanks to James Peng

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(lubridate)
library(googleAuthR)
library(plotly)



ui <- dashboardPage(
  dashboardHeader(title = "DTG Study Dashboard", titleWidth = 250),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "first", icon = icon("dashboard"),
               menuSubItem("Enrollment", tabName = "enrollment"),
               menuSubItem("Follow-up", tabName = "follow_up")
               ),
      menuItem("Data QC", tabName = "data_qc",icon = icon("database"),
               menuSubItem("QC Reports", tabName = "qc_report")),
      menuItem("Reports", tabName = "reports", icon = icon("th"),
               menuSubItem("Missed Visits", tabName = "missed_visit"),
               menuSubItem("Withdrawals/Move", tabName = "withdrawal")),
      menuItem("Downlaod", tabName = "raw_data_download", icon = icon("download")),
      hr(),
      " Find Study Id",
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Enter Study Id..."),
      uiOutput("logininfo"),
      uiOutput("last_updated")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "enrollment",
              h2("Enrollment Summary"),
              # Screening and Enrollment Numbers
              fluidRow(
                # Dynamic valueBoxes
                #valueBoxOutput("screened"),
                valueBoxOutput("enrolled"),
                valueBoxOutput("male"),
                valueBoxOutput("bmi")
              ),
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("hypertensive"),
                
                valueBoxOutput("diabetic"),
                valueBoxOutput("cholestrol")
              ),
              fluidRow(
                
                box(status = 'primary', solidHeader = TRUE,title = 'Breakdowns',
                    selectInput("breakdown", label = "Breakdown: ",
                                choices = c('All','Gender', 'Age-group','Pre-conditions'))
                ),
                box(status = 'primary', solidHeader = TRUE,title = 'Sub-Breakdowns',
                    selectInput("subcategory", label = "Sub Category: ",
                                choices = c('All'))
                    
                )
                
              ),
              fluidRow(
                box(title="Enrollment Summary by Gender", 
                    htmlOutput("enroll_text_summary"),
                    plotlyOutput("plot1_enrollment")
                    ),
                
                box(title="Enrollment by All/Sub Categories", 
                    htmlOutput("enroll_sub"),
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('enrollment_list')),
                    downloadButton("download1","Download csv")
                )
              )
              ),
      tabItem(tabName = "follow_up", 
              # Add variours value boxes to show follow-up visit status
              h2("Follow-up Visits Summary"),
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("month_1"),
                
                valueBoxOutput("month_3"),
                valueBoxOutput("month_6")
              ),
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("new_hypertenstion"),
                
                valueBoxOutput("new_diabetic"),
                valueBoxOutput("new_cholestrol")
              ),
              fluidRow(
                
                box(status = 'primary', solidHeader = TRUE,title = 'Follow-up Visits',
                    selectInput("fu_visits", label = "Breakdown: ",
                                choices = c('All','Month 1', 'Month 3','Month 6'))
                ),
                box(status = 'primary', solidHeader = TRUE,title = 'Incidence',
                    selectInput("fu_incidence", label = "Sub Category: ",
                                choices = c('All','Diabetic','Hypertensive','High Cholestrol','Obesity'))
                    
                )
                
              )
              ),
      tabItem(tabName = "raw_data_download",
              # Add various buttons to download the raw data
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Download raw Screening Data",
                    downloadButton("download_screening", label = "Download Screening csv")
                    ),
                box(status = "primary", solidHeader = TRUE, title = "Download raw Enrollment Data",
                    downloadButton("download_enrollment", label = "Download Enrollment csv")
                ),
                box(status = "primary", solidHeader = TRUE, title = "Download raw Tracking Data",
                    downloadButton("download_tracking", label = "Download Tracking csv")
                )
              ),
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Download raw Follow-up Data",
                    downloadButton("download_fup", label = "Download follow-up csv")
                ),
                box(status = "primary", solidHeader = TRUE, title = "Download raw Withdrawal Data",
                    downloadButton("download_withdrawal", label = "Download Withdrawal csv")
                )
              )
              )
      
    )
  )
)
