# UI
# DTG STUDY UI for the Dashboard
# Created on 24/09/2020
# Developed by Wafula Erick
# This dashboard has been adopted from what we currently have from Search Youth Thanks to James Peng

library(shiny)
library(shinydashboard)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = "DTG Study Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "first", icon = icon("dashboard"),
               menuSubItem("Screening", tabName = "screening"),
               menuSubItem("Enrollment", tabName = "enrollment")
               ),
      menuItem("Data QC", tabName = "data_qc",
               menuSubItem("QC Reports", tabName = "qc_report")),
      menuItem("Reports", tabName = "reports",
               menuSubItem("Missed Visits", tabName = "missed_visit"),
               menuSubItem("Withdrawals/Move", tabName = "withdrawal"))
    )
  ),
  dashboardBody()
)
