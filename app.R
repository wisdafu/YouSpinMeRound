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
library(leaflet)

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
saveRDS(data, "data.rds")

# Example - Load RData file
# tempData <- readRDS("data.rds")


# Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Project 3"),
  
  dashboardSidebar(
    sidebarMenu(
      # Create radio buttons to switch between O'Hare and Midway
      radioButtons("ymhOption", "View:",
                   c("Yearly" = "Yearly",
                     "Monthly" = "Monthly",
                     "Hourly" = "Hourly")
      )
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  dashboardBody(
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 fluidRow(
                   box(title = "Map", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("map"))
                 )
        ),
        tabPanel("Charts", 
                 fluidRow(
                   box(title = "Chart", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("fatalitiesLineChart"))
                 )
        ),
        tabPanel("Tables", 
                 fluidRow(
                   box(title = "Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("fatalitiesTable"))
                 )
        )
      )
    )
  ) # end dashboardBody
) # end dashBoardPage

server <- function(input, output) { 
  
  # Focus on hourly/monthly/yearly data
  ymhChoice <- reactive ({
    input$ymhOption
  })
  
  #table and chart showing the injuries, fatalities, loss for each year in the records
  output$fatalitiesTable <- DT::renderDataTable({
    fatYear <- data
    fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
    fatYear <- group_by(fatYear, Year)
    fatYear <- mutate(fatYear, Fatalities = sum(Fatalities))
    fatYear <- mutate(fatYear, Injuries = sum(Injuries))
    fatYear <- mutate(fatYear, Loss = sum(Loss))
    fatYear <- select(fatYear, Year, Fatalities, Injuries, Loss)
    fatYear1 <- distinct(fatYear)
    
    DT::datatable(fatYear1, options = list(pageLength = 8, lengthChange = FALSE, searching = FALSE))
  })
  
  output$fatalitiesLineChart <- renderPlotly({
    
    # Check which option is chosen
    if(ymhChoice() == "Yearly") {
      fatYear <- data
      fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
      fatYear <- group_by(fatYear, Year)
      fatYear <- mutate(fatYear, Fatalities = sum(Fatalities))
      #fatYear <- mutate(fatYear, Injuries = sum(Injuries))
      #fatYear <- mutate(fatYear, Loss = sum(Loss))
      fatYear <- select(fatYear, Year, Fatalities)
      fatYear <- distinct(fatYear)
      
      fatYear <- data.frame(fatYear$Year, fatYear$Fatalities)
      
      finalChart <- plot_ly(fatYear, x = ~fatYear.Year, y = ~fatYear.Fatalities, name = "Fatalities", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Year", tickangle = 45), yaxis = list (title = "Count"))
      
    }
    
    if(ymhChoice() == "Monthly") {
      fatMonth <- data
      fatMonth$Month <- format(as.POSIXct(fatMonth$Date, format="%Y-%m-%d"),"%b")
      fatMonth <- group_by(fatMonth, Month)
      fatMonth <- mutate(fatMonth, Fatalities = sum(Fatalities))
      fatMonth <- select(fatMonth, Month, Fatalities)
      fatMonth <- distinct(fatMonth)
      
      fatMonth$Month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
      dat <- data.frame(fatMonth$Month, fatMonth$Fatalities)
      
      
      
      finalChart <- plot_ly(dat, x = ~dat$fatMonth.Month, y = ~dat$fatMonth.Fatalities, name = "Fatalities", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Month", 
                            tickangle = 45, 
                            categoryorder = "array", 
                            categoryarray = c(fatMonth$Month)),
                            yaxis=list(title="Fatalities"))
    }
    
    if(ymhChoice() == "Hourly") {
      
    }
    
    finalChart
    
  })
  
  output$lossLineChart <- renderPlotly({
    fatYear <- data
    fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
    fatYear <- group_by(fatYear, Year)
    fatYear <- mutate(fatYear, Fatalities = sum(Fatalities))
    fatYear <- mutate(fatYear, Injuries = sum(Injuries))
    fatYear <- mutate(fatYear, Loss = sum(Loss))
    fatYear <- select(fatYear, Year, Fatalities, Injuries, Loss)
    fatYear1 <- distinct(fatYear)
    
    DT::datatable(fatYear1, options = list(pageLength = 8, lengthChange = FALSE, searching = FALSE))
  })
  
  output$fatalitiesByYearChart <- renderPlotly({
    
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
    
    DT::datatable(fatMonth1, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
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
    
    
    DT::datatable(fatHour1, options = list(pageLength = 8, lengthChange = FALSE, searching = FALSE))
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
  
  # Leaflet map for all tornadoes
  # Need to add init markers
  output$map <- renderLeaflet({
    map1 <- dplyr::filter(data, data$State == 'IL')
    map2 <- map1
    map1 <- dplyr::filter(map1, map1$`End Lon` != 0)
    m <- leaflet::leaflet()
    m <- leaflet::addTiles(m)
    for(i in 1:nrow(map1)){
      m <- leaflet::addPolylines(m, lat = as.numeric(map1[i, c(8, 10)]), lng = as.numeric(map1[i, c(9, 11)]))
    }
    # Maps the tornado touch down in hopes to counteract
    # Long and Lat values of 0 which end up in Africa
    #m <- addMarkers(m, lat = map2$`Start Lat`, lng = map2$`Start Lon`)
    m
  })
}

shinyApp(ui, server)
