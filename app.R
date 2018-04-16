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
                 ),
                 fluidRow(
                   box(title = "Chart", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("lossLineChart"))
                 )
        ),
        tabPanel("Tables", 
                 fluidRow(
                   box(title = "Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("fatalitiesInjuriesLossTable"))
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
  
  # table showing the injuries, fatalities, loss for each year/month/hour in the records
  output$fatalitiesInjuriesLossTable <- DT::renderDataTable({
    if(ymhChoice() == "Yearly") {
      fatYear <- data
      fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
      fatYear <- group_by(fatYear, Year)
      fatYear <- mutate(fatYear, Fatalities = sum(Fatalities))
      fatYear <- mutate(fatYear, Injuries = sum(Injuries))
      fatYear <- mutate(fatYear, Loss = sum(Loss))
      fatYear <- select(fatYear, Year, Fatalities, Injuries, Loss)
      
      finalTable <- distinct(fatYear)
    }
    
    if(ymhChoice() == "Monthly") {
      fatMonth <- data
      fatMonth$Month <- format(as.POSIXct(fatMonth$Date, format="%Y-%m-%d"),"%b")
      fatMonth <- group_by(fatMonth, Month)
      fatMonth <- mutate(fatMonth, Fatalities = sum(Fatalities))
      fatMonth <- mutate(fatMonth, Injuries = sum(Injuries))
      fatMonth <- mutate(fatMonth, Loss = sum(Loss))
      fatMonth <- select(fatMonth, Month, Fatalities, Injuries, Loss)
      finalTable <- distinct(fatMonth)
    }
    
    if(ymhChoice() == "Hourly") {
      fatHour <- data
      fatHour$Hour <- factor(fatHour$Time)
      fatHour$Hour <- format(strptime(fatHour$Hour,"%H:%M:%S"),'%H')
      fatHour <- group_by(fatHour, Hour)
      fatHour <- mutate(fatHour, Fatalities = sum(Fatalities))
      fatHour <- mutate(fatHour, Injuries = sum(Injuries))
      fatHour <- mutate(fatHour, Loss = sum(Loss))
      fatHour <- select(fatHour, Hour, Fatalities, Injuries, Loss)
      finalTable <- distinct(fatHour)
    }
    
    DT::datatable(finalTable, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  })
  
  # chart showing the fatalities for each year/month/hour
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
      
      dat <- data.frame(fatMonth$Month, fatMonth$Fatalities)
      
      finalChart <- plot_ly(dat, x = ~dat$fatMonth.Month, y = ~dat$fatMonth.Fatalities, name = "Fatalities", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Month", 
                            tickangle = 45, 
                            categoryorder = "array", 
                            categoryarray = c(fatMonth$Month)),
                            yaxis=list(title="Fatalities"))
    }
    
    if(ymhChoice() == "Hourly") {
      fatHour <- data
      fatHour$Hour <- factor(fatHour$Time)
      fatHour$Hour <- format(strptime(fatHour$Hour,"%H:%M:%S"),'%H')
      fatHour <- group_by(fatHour, Hour)
      fatHour <- mutate(fatHour, Fatalities = sum(Fatalities))
      #fatHour <- mutate(fatHour, Injuries = sum(Injuries))
      #fatHour <- mutate(fatHour, Loss = sum(Loss))
      fatHour <- select(fatHour, Hour, Fatalities)
      fatHour <- distinct(fatHour)
      newdt <- fatHour[order(fatHour$Hour),]
      
      dat <- data.frame(newdt$Hour, newdt$Fatalities) #add different parameter here
      
      finalChart <- plot_ly(dat, x=~dat$newdt.Hour, y =~dat$newdt.Fatalities, name = "Fatalities", type = "scatter", mode = "lines") %>%
        layout(xaxis= list(title = "Hour",
                           tickangle = 45, 
                           categoryorder = "array", 
                           categoryarray = c(newdt$Hour)), 
               yaxis = list (title = "Fatalities"))
    }
    
    finalChart
    
  })
  
  # chart showing the loss for each year/month/hour 
  output$lossLineChart <- renderPlotly({
    if(ymhChoice() == "Hourly") {
      fatHour <- data
      fatHour$Hour <- factor(fatHour$Time)
      fatHour$Hour <- format(strptime(fatHour$Hour,"%H:%M:%S"),'%H')
      fatHour <- group_by(fatHour, Hour)
      #fatHour <- mutate(fatHour, Injuries = sum(Injuries))
      fatHour <- mutate(fatHour, Loss = sum(Loss))
      fatHour <- select(fatHour, Hour, Loss)
      fatHour <- distinct(fatHour)
      newdt <- fatHour[order(fatHour$Hour),]
      
      dat <- data.frame(newdt$Hour, newdt$Loss) #add different parameter here
      
      finalChart <- plot_ly(dat, x=~dat$newdt.Hour, y =~dat$newdt.Loss, name = "Loss", type = "scatter", mode = "lines") %>%
        layout(xaxis= list(title = "Hour",
                           tickangle = 45, 
                           categoryorder = "array", 
                           categoryarray = c(newdt$Hour)), 
               yaxis = list (title = "Loss"))
    }
    
    if(ymhChoice() == "Monthly") {
      fatMonth <- data
      fatMonth$Month <- format(as.POSIXct(fatMonth$Date, format="%Y-%m-%d"),"%b")
      fatMonth <- group_by(fatMonth, Month)
      fatMonth <- mutate(fatMonth, Loss = sum(Loss))
      fatMonth <- select(fatMonth, Month, Loss)
      fatMonth <- distinct(fatMonth)
      
      dat <- data.frame(fatMonth$Month, fatMonth$Loss)
      
      finalChart <- plot_ly(dat, x = ~dat$fatMonth.Month, y = ~dat$fatMonth.Loss, name = "Loss", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Month", 
                            tickangle = 45, 
                            categoryorder = "array", 
                            categoryarray = c(fatMonth$Month)),
               yaxis=list(title="Loss"))
    }
    
    if(ymhChoice() == "Yearly") {
      fatYear <- data
      fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
      fatYear <- group_by(fatYear, Year)
      #fatYear <- mutate(fatYear, Injuries = sum(Injuries))
      fatYear <- mutate(fatYear, Loss = sum(Loss))
      fatYear <- select(fatYear, Year, Loss)
      fatYear <- distinct(fatYear)
      
      fatYear <- data.frame(fatYear$Year, fatYear$Loss)
      
      finalChart <- plot_ly(fatYear, x = ~fatYear.Year, y = ~fatYear.Loss, name = "Loss", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Year", tickangle = 45), yaxis = list (title = "Count"))
    }
    
    finalChart
  })
  

  
  # Leaflet map for all tornadoes
  # Need to add init markers
  output$map <- renderLeaflet({
    map1 <- dplyr::filter(data, data$State == "IL")
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