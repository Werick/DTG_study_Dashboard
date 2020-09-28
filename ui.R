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
      menuItem("Data QC", tabName = "data_qc",icon = icon("th"),
               menuSubItem("QC Reports", tabName = "qc_report")),
      menuItem("Reports", tabName = "reports", icon = icon("th"),
               menuSubItem("Missed Visits", tabName = "missed_visit"),
               menuSubItem("Withdrawals/Move", tabName = "withdrawal")),
      menuItem("Downlaod", tabName = "raw_data_download", icon = icon("downlaod")),
      hr(),
      " Find Study Id",
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Enter Study Id..."),
      uiOutput("logininfo")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "enrollment",
              # Screening and Enrollment Numbers
              fluidRow(
                box(status = 'primary', solidHeader = TRUE, title = 'Screening and Enrolment',
                    htmlOutput("total_screened"),
                    #uiOutput('screen_progress'),
                    htmlOutput("enroll_progress")
                    ),
                box(status = 'primary', solidHeader = TRUE,title = 'Breakdowns',
                    selectInput("breakdown", label = "Breakdown: ",
                                choices = c('None','Gender', 'Age-group')),
                    selectInput("subcategory", label = "Sub Category: ",
                                choices = c('None'))
                    #htmlOutput("enroll_progress"))
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
      tabItem(tabName = "raw_data_download",
              # Add various buttons to download the raw data
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Download raw Screening Data",
                    downloadButton("download_screening", label = "Download Screening csv")
                    ),
                box(status = "primary", solidHeader = TRUE, title = "Download raw Enrollment Data",
                    downloadButton("download_enrollment", label = "Download Enrollment csv")
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
