#   
#   CS 424 Spring 2018 UIC
#   Project 3 - You Spin Me Round
#
#   Aaron Struck    - Undergraduate
#   Chris Janowski  - Undergraduate
#   Steve Stranczek - Undergraduate
#

# Import libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)
library(lubridate)
library(dplyr)

# Read in dataset with relevant columns and rename them
data <- read.csv(file = "revised.1950-2016_all_tornadoes.csv", header = TRUE)
headerNames <- c("Date", "Time", "State", "Magnitude", "Injuries", "Fatalities", "Loss","Start Lat","Start Lon","End Lat","End Lon","Length","Width","F1","F2","F3","F4","FC")
colnames(data) <- headerNames

#
# TODO: This following line is not formatting the dates correctly... 
# e.g. of what it is doing: 1/3/50 --> 2050-01-03
# Should be 1950-01-03
#
# Ensure date column is in date format
data$Date <- as.Date(data$Date, "%m/%d/%y")




# Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader("Project 3"),
  
  dashboardSidebar(
    sidebarMenu(
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  dashboardBody(
    
  ) # end dashboardBody
) # end dashBoardPage

server <- function(input, output) { 
  
}

shinyApp(ui, server)

