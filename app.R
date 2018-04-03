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


# Ensure date column is in date format
data$Date <- as.Date(data$Date, "%m/%d/%y")

# Some dates were converting to dates that haven't happened...
# Solution is if date is past today, it should be 19XX instead of 20XX
data$Date <- as.Date(ifelse(data$Date > "2017-12-31", format(data$Date, "19%y-%m-%d"), format(data$Date)))

# Save data in RData file
saveRDS(data, 'data.rds')

# Example - Load RData file
# tempData <- readRDS('data.rds')


# Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Project 3"),
  
  dashboardSidebar(
    sidebarMenu(
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  dashboardBody(
    box(
      title = "Fart", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("fatalitiesByYear")
    )
  ) # end dashboardBody
) # end dashBoardPage

server <- function(input, output) { 
  output$fatalitiesByYear <- DT::renderDataTable({
    fatYear <- data
    fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
    fatYear <- group_by(fatYear, Year)
    fatYear <- mutate(fatYear, Fatalities = sum(Fatalities))
    fatYear <- mutate(fatYear, Injuries = sum(Injuries))
    fatYear <- mutate(fatYear, Loss = sum(Loss))
    fatYear <- select(fatYear, Year, Fatalities, Injuries, Loss)
    fatYear1 <- distinct(fatYear)
    
    DT::datatable(fatYear1, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  })
}

shinyApp(ui, server)

