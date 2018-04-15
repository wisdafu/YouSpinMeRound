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
    fluidRow(
      box(
        title = "Year", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("fatalitiesByYear")
      ),
      box(
        title = "Year", solidHeader = TRUE, status = "primary", wdith = 6, plotlyOutput("fatalitiesByYearChart")
      )
    ),
    fluidRow(
      box(
        title = "Month", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("fatalitiesByMonth")
      ),
      box(
        title = "Month", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("fatalitiesByMonthChart")
      )
    ),
    fluidRow(
      box(
        title = "Hour", solidHeader = TRUE, status = "primary", width = 6, dataTableOutput("fatalitiesByHour")
      ),
      box(
        title = "Hour", solidHeader = TRUE, status = "primary", width = 6, plotlyOutput("fatalitiesByHourChart")
      )
    )
  ) # end dashboardBody
) # end dashBoardPage

server <- function(input, output) { 
  
  #table and chart showing the injuries, fatalities, loss for each year in the records
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
  
  output$fatalitiesByYearChart <- renderPlotly({
    fatYear <- data
    fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
    fatYear <- group_by(fatYear, Year)
    fatYear <- mutate(fatYear, Fatalities = sum(Fatalities))
    fatYear <- mutate(fatYear, Injuries = sum(Injuries))
    fatYear <- mutate(fatYear, Loss = sum(Loss))
    fatYear <- select(fatYear, Year, Fatalities, Injuries, Loss)
    fatYear1 <- distinct(fatYear)
    
    dat <-data.frame(fatYear1$Year, fatYear1$Fatalities, fatYear1$Injuries, fatYear1$Loss)
    
    plot_ly(dat,x = ~dat$fatYear1.Year, y = ~dat$fatYear1.Fatalities, name = "Fatalities", type = "scatter", mode = "lines") %>%
           add_trace(y = ~dat$fatYear1.Injuries, name = "Injuries", mode = "lines") %>%
           add_trace(y = ~dat$fatYear1.Loss, name = "Loss", mode = "lines") %>%
           layout(xaxis = list(title = "Year",
                          tickangle = 45),
             yaxis = list (title = "Fatalities, Injuries, and Loss"))
  })
  
  
  #table and chart showing the injuries, fatalities, loss per month summed over all years
  output$fatalitiesByMonth <- DT::renderDataTable({
    fatMonth <- data
    fatMonth$Month <- format(as.POSIXct(fatMonth$Date, format="%Y-%m-%d"),"%b")
    fatMonth <- group_by(fatMonth, Month)
    fatMonth <- mutate(fatMonth, Fatalities = sum(Fatalities))
    fatMonth <- mutate(fatMonth, Injuries = sum(Injuries))
    fatMonth <- mutate(fatMonth, Loss = sum(Loss))
    fatMonth <- select(fatMonth, Month, Fatalities, Injuries, Loss)
    fatMonth1 <- distinct(fatMonth)
    
    DT::datatable(fatMonth1, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  })
  
  output$fatalitiesByMonthChart <- renderPlotly({
    fatMonth <- data
    fatMonth$Month <- format(as.POSIXct(fatMonth$Date, format="%Y-%m-%d"),"%b")
    fatMonth <- group_by(fatMonth, Month)
    fatMonth <- mutate(fatMonth, Fatalities = sum(Fatalities))
    fatMonth <- mutate(fatMonth, Injuries = sum(Injuries))
    fatMonth <- mutate(fatMonth, Loss = sum(Loss))
    fatMonth <- select(fatMonth, Month, Fatalities, Injuries, Loss)
    fatMonth1 <- distinct(fatMonth)
    
    fatMonth1$Month1 <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    dat <- data.frame(fatMonth1$Month1, fatMonth1$Fatalities, fatMonth1$Injuries, fatMonth1$Loss)
    
    plot_ly(dat, x = ~dat$fatMonth1.Month1, y = ~dat$fatMonth1.Fatalities, name = "Fatalities", type = "scatter", mode = "lines") %>%
      add_trace(y = ~dat$fatMonth1.Injuries, name = "Injuries", mode = "lines") %>%
      add_trace(y = ~dat$fatMonth1.Loss, name = "Loss", mode = "lines") %>%
      layout(xaxis = list(title = "Month",
                          tickangle = 45,
                          categoryorder = "array", 
                          categoryarray = c(fatMonth1$Month1)),
             yaxis = list (title = "Fatalities, Injuries, and Loss"))
  })
  
  #table and chart showing the injuries, fatalities, loss per hour of the day summed over all years
  output$fatalitiesByHour <- DT::renderDataTable({
    fatHour <- data
    fatHour$Hour <- factor(fatHour$Time)
    fatHour$Hour <- format(strptime(fatHour$Hour,"%H:%M:%S"),'%H')
    fatHour <- group_by(fatHour, Hour)
    fatHour <- mutate(fatHour, Fatalities = sum(Fatalities))
    fatHour <- mutate(fatHour, Injuries = sum(Injuries))
    fatHour <- mutate(fatHour, Loss = sum(Loss))
    fatHour <- select(fatHour, Hour, Fatalities, Injuries, Loss)
    fatHour1 <- distinct(fatHour)
    
    
    DT::datatable(fatHour1, options = list(pageLength = 8, lengtChange = FALSE, searching = FALSE))
  })
  
  output$fatalitiesByHourChart <- renderPlotly({
    fatHour <- data
    fatHour$Hour <- factor(fatHour$Time)
    fatHour$Hour <- format(strptime(fatHour$Hour,"%H:%M:%S"),'%H')
    fatHour <- group_by(fatHour, Hour)
    fatHour <- mutate(fatHour, Fatalities = sum(Fatalities))
    fatHour <- mutate(fatHour, Injuries = sum(Injuries))
    fatHour <- mutate(fatHour, Loss = sum(Loss))
    fatHour <- select(fatHour, Hour, Fatalities, Injuries, Loss)
    fatHour1 <- distinct(fatHour)
    newdt <- fatHour1[order(fatHour1$Hour),]
    
    dat <- data.frame(newdt$Hour, newdt$Fatalities, newdt$Injuries, newdt$Loss)
    
    plot_ly(dat, x=~dat$newdt.Hour, y =~dat$newdt.Fatalities, name = "Fatalities", type = "scatter", mode = "lines") %>%
           add_trace(y = ~dat$newdt.Injuries, name ="Injuries", mode = "lines") %>%
           add_trace(y= ~dat$newdt.Loss, name = "Loss", mode = "lines") %>%
           layout(xaxis= list(title = "Hour",
                                                      tickangle = 45,
                                                      categoryorder = "array",
                                                      categoryarray = c(newdt$Hour)),
                               yaxis = list (title = "Fatalities, Injuries, and Loss"))
    
  })
}

shinyApp(ui, server)

