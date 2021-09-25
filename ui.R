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
      menuItem("Endpoint", tabName = "endpoint", icon = icon("hourglass-end"),
               menuSubItem("Status Overview", tabName = "ep_overview"),
               menuSubItem("Preliminary Results", tabName = "ep_results")),
      menuItem("Data QC", tabName = "data_qc",icon = icon("database"),
               menuSubItem("QC Reports Baseline", tabName = "qc_report"),
               menuSubItem("QC Reports Followup 1", tabName = "qc_report1"),
               menuSubItem("QC Reports Followup 3", tabName = "qc_report3"),
               menuSubItem("QC Reports Followup 6", tabName = "qc_report6")),
      menuItem("Reports", tabName = "reports", icon = icon("th"),
               menuSubItem("Scheduled/Missed Visits", tabName = "missed_visit"),
               menuSubItem("Withdrawals/Move", tabName = "withdrawal"),
               menuSubItem("Retention", tabName = "retention")),
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
                valueBoxOutput("cholesterol")
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
                
                box(title="Monthly Enrollment Progress", 
                    htmlOutput("enroll_sub"),
                    plotlyOutput("plot2_enrollment"),
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('enrollment_list')),
                    downloadButton("download1","Download csv")
                )
              )
              ),
      tabItem(tabName = "missed_visit",
              fluidRow(
                box(status = 'primary', solidHeader = TRUE,title = 'Scheduled Visits',
                    dateInput('svisit_since', 'From:', value = Sys.Date()), 
                    dateInput('svisit_to', 'To:', value = Sys.Date()), 
                    HTML('<br><br>'),
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('sch_visits')),
                    downloadButton("sch_download","Download csv")
                ),
                box(status = 'primary', solidHeader = TRUE,title = 'Missed Visits',
                    dateInput('mvisit_since', 'From:', value = '2019-03-01'), 
                    dateInput('mvisit_to', 'To:', value = Sys.Date() - 1), 
                    HTML('<br><br>'),
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('m_visits')),
                    downloadButton("m_download","Download csv")
                )
              )
              ),
      tabItem(tabName = "follow_up", 
              # Add variours value boxes to show follow-up visit status
              h2("Follow-up Visits Summary"),
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("weight_1"),
                
                valueBoxOutput("month_3"),
                valueBoxOutput("month_6")
              ),
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("new_hypertenstion"),
                
                valueBoxOutput("new_diabetic"),
                valueBoxOutput("new_cholesterol")
              ),
              fluidRow(
                
                box(status = 'primary', solidHeader = TRUE,title = 'Follow-up Visits',
                    selectInput("fu_visit_selection", label = "Breakdown: ",
                                choices = c('All','Month 1', 'Month 3','Month 6'))
                ),
                box(status = 'primary', solidHeader = TRUE,title = 'Incidence',
                    selectInput("fu_incidence_selection", label = "Sub Category: ",
                                choices = c('All','Diabetic','Hypertensive','High Cholesterol','Overweight','Obesity', 'Weight Change'))
                    
                )
              ),
              fluidRow(
                
                box(
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('fu_visits')),
                  downloadButton("fu_download","Download csv")
                
                ),
                box(
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('fu_visits_incidence')),
                  downloadButton("fu_incidence_download","Download csv")
                )
              )
              ),
      tabItem(tabName = "retention",
              h2("Retention Summary"),
              fluidRow(
                # Dynamic valueBoxes
                valueBoxOutput("retention_Summary_prop"),
                valueBoxOutput("retention_gender"),
                valueBoxOutput("retention_hyp")
              ),
              fluidRow(
                box(title="Retention Summary", 
                    plotlyOutput("plot3_retention"),
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('retention_list')),
                    downloadButton("download1_ret","Download csv")
                ),
                box(title="Retention Summary", 
                    plotlyOutput("plot4_retention")
                    #div(style = 'overflow-x: scroll', DT::dataTableOutput('enrollment_list')),
                    #downloadButton("download1","Download csv")
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
              ),
      tabItem(tabName = 'qc_report',
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in hemoglobin A1C",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('hemoglobin')),
                    downloadButton("download_hgb", label = "hemoglobinA1C csv")
                ),
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting Blood Sugar",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('fbs')),
                    downloadButton("download_fbs", label = "fasting blood sugar csv")
                )
              ),
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting total cholesterol ",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('tchol')),
                    downloadButton("download_ftchol", label = "Fasting total cholesterol csv")
                ),
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting HDL cholesterol ",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('hdlchol')),
                    downloadButton("download_fhdlchol", label = "Fasting HDL cholesterol csv")
                )
              ),
              fluidRow(
               box(status = "primary", solidHeader = TRUE, title = "Error in Fasting Triglycerides ",
                   div(style = 'overflow-x: scroll', DT::dataTableOutput('ftrig')),
                    downloadButton("download_trigchol", label = "Fasting Triglycerides csv")
                )
              )
              ),
      tabItem(tabName = 'qc_report1',
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in hemoglobin A1C",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('hemoglobin1')),
                    downloadButton("download_hgb1", label = "hemoglobinA1C csv")
                ),
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting Blood Sugar",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('fbs1')),
                    downloadButton("download_fbs1", label = "fasting blood sugar csv")
                )
              ),
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting total cholesterol ",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('tchol1')),
                    downloadButton("download_ftchol1", label = "Fasting total cholesterol csv")
                ),
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting HDL cholesterol ",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('hdlchol1')),
                    downloadButton("download_fhdlchol1", label = "Fasting HDL cholesterol csv")
                )
              ),
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting Triglycerides ",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('ftrig1')),
                    downloadButton("download_trigchol1", label = "Fasting Triglycerides csv")
                )
              )
      ),
      tabItem(tabName = 'qc_report3',
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting Blood Sugar",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('fbs3')),
                    downloadButton("download_fbs3", label = "fasting blood sugar csv")
                )
              )
      ),
      tabItem(tabName = 'qc_report6',
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in hemoglobin A1C",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('hemoglobin6')),
                    downloadButton("download_hgb6", label = "hemoglobinA1C csv")
                ),
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting Blood Sugar",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('fbs6')),
                    downloadButton("download_fbs6", label = "fasting blood sugar csv")
                )
              ),
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting total cholesterol ",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('tchol6')),
                    downloadButton("download_ftchol6", label = "Fasting total cholesterol csv")
                ),
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting HDL cholesterol ",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('hdlchol6')),
                    downloadButton("download_fhdlchol6", label = "Fasting HDL cholesterol csv")
                )
              ),
              fluidRow(
                box(status = "primary", solidHeader = TRUE, title = "Error in Fasting Triglycerides ",
                    div(style = 'overflow-x: scroll', DT::dataTableOutput('ftrig6')),
                    downloadButton("download_trigchol6", label = "Fasting Triglycerides csv")
                )
              )
      ),
      tabItem(tabName = 'withdrawal',
              fluidRow(
                h2("Withdrawn Participants"),
                div(style = 'overflow-x: scroll', DT::dataTableOutput('withraw_list')),
                downloadButton("download_withdrawn","Download csv")
              )
              ),
      tabItem(tabName = 'ep_overview',
              fluidRow(
                box(status = 'primary', solidHeader = TRUE, title = "DTG Month 6 Endpoint Overview",
                    htmlOutput("in_window"),
                    radioGroupButtons(
                      inputId = "in_window_type",
                      choices = c("Completed" = "endpoint_complete",
                                  "started" = "endpoint_started")
                    ),
                    htmlOutput('in_window_progress')),
                box(status = "primary", solidHeader = TRUE, title = "Summary Statistics at Month 6 (Started or Completed)",
                    htmlOutput("in_window_weight_change"),
                    htmlOutput("in_window_diabetic"),
                    htmlOutput("in_window_hypertensive"),
                    htmlOutput("in_window_high_chol")
                    )
              ),
              fluidRow(box(
                HTML("This line list only includes those who are in their endpoint window.<br><br>"),
                checkboxGroupInput(inputId="ep_vars_to_include", label="Variables to include", 
                                   choices=c("Endpoint Ascertained", "Endpoint Started", "Withdrawn"),
                                              selected = c("Endpoint Ascertained"), inline = TRUE),
                div(style = 'overflow-x: scroll', DT::dataTableOutput("ep_ind_line_list")),
                downloadButton('download_ep_line_list', 'Download CSV'),
                tags$script("$(document).on('click', '#ep_ind_line_list button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                              Shiny.onInputChange('lastClick', Math.random())
                              });"),
                title = 'Individual Line List', width = 12))
      ),
      tabItem('ep_results', 
              fluidRow(
                box(
                  status = 'primary', 
                  title = 'Summary Spreadsheet', 
                  div(style = 'overflow-x: scroll', tableOutput('ep_summary_table')),
                  downloadButton('download_ep_summary_ss', 'Download CSV'),
                  width = 12
                )
              )
      )
      
    )
  )
)
