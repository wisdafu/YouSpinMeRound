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
fips <- read.csv(file = 'fips.csv', header = TRUE)
headerNames <- c("Date", "Time", "State", "Magnitude", "Injuries", "Fatalities", "Loss","Start Lat","Start Lon","End Lat","End Lon","Length","Width","F1","F2","F3","F4","FC")
colnames(data) <- headerNames

# Ensure date column is in date format
data$Date <- as.Date(data$Date, "%m/%d/%y")

# Some dates were converting to dates that haven't happened...
# Solution is if date is past today, it should be 19XX instead of 20XX
data$Date <- as.Date(ifelse(data$Date > "2017-12-31", format(data$Date, "19%y-%m-%d"), format(data$Date)))
data <- dplyr::filter(data, data$State == "IL")

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
      ),
      radioButtons('hours', 'Hours:',
                   c('12 Hr' = 24,
                     '24 Hr' = 12)             
      ),
      menuItem("About", icon = icon("question-circle"), href = "http://cjanow3.people.uic.edu/project3.html")
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  dashboardBody(
    tabsetPanel(
      tabPanel("Map", 
               fluidRow(
                 column(width = 12, 
                        box(title = "Map", width = NULL, solidHeader = TRUE, status = "primary",
                            leafletOutput("map", height = 500))
               )),
               fluidRow(
                 h1("Filters", align = "center"),
                  
                 column(width = 4, align = "center",
                        
                        h4("Magnitude"),
                               selectInput("magnitudeLvl", "",
                                c("All" = -1 ,"0" = 0, "1" = 1, "2" = 2, "3" = 3, "4"= 4, "5" = 5)),
                        h4("Width (yd)"),
                               splitLayout(numericInput("minWidth", label = "min", value = 0, min = 0, max = 2630),
                                           numericInput("maxWidth", label = "max", value = 2630, min = 0, max = 2630)),
                        h4("Length (mi)"),
                               splitLayout(numericInput("minLength", label = "min", value = 0, min = 0, max = 157),
                                           numericInput("maxLength", label = "max", value = 157, min = 0, max = 157))
                               ),
                        
                 column(width = 4, align = "center",
                               h4("Loss"),
                               selectInput("loss", "",
                                           c("Lorem" ,"Ipsum")),
                               h4("Injuries"),
                               sliderInput("injuries", "",step = 5,
                                           min = 0, max = 500,
                                           value = c(0,500)),
                               h4("Fatalities"),
                               sliderInput("fatalities", "",step = 1,
                                           min = 0, max = 33,
                                           value = c(0,33))
                        ),
                        
                 column(width = 4, align = "center",
                        
                        h4("Stats:"),
                        tableOutput("statsTable")
                        ))
      ),
      tabPanel("Charts", 
               fluidRow(
                 box(title = "Chart", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("fatalitiesLineChart"))
               ),
               fluidRow(
                 box(title = "Chart", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("lossLineChart"))
               ),
               fluidRow(
                 box(title = "Chart", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("injuryLineChart"))
               ),
               fluidRow(
                 box(title = "Number of Tornadoes", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("numTornadoLineChart"))
               )
      ),
      tabPanel("Tables", 
               fluidRow(
                 box(title = "Table", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("fatalitiesInjuriesLossTable")),
                 box(title = "Magnitude", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("magnitudeTable")),
                 box(title = "Number of Tornadoes", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("numTornadoTable"))
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
  
  # Values for min/max for Length
  minWidth <- reactive ({
    as.numeric(input$minWidth)
  })
  
  maxWidth <- reactive ({
    as.numeric(input$maxWidth)
  })
  
  # Values for min/max for Length
  minLength <- reactive ({
    as.numeric(input$minLength)
  })
  
  maxLength <- reactive ({
    as.numeric(input$maxLength)
  })
  
  minFatal <- reactive ({
    input$injuries[1]
  })
  
  maxFatal <- reactive ({
    input$injuries[2]
  })
  
  minInjury <- reactive ({
    input$fatalities[1]
  })
  
  maxInjury <- reactive ({
    input$fatalities[2]
  })
  
  magnitudeChoice <- reactive ({
    input$magnitudeLvl
  })
  
  hourSetting <- reactive ({
    input$hours
  })
  
  # Renders table output that shows some key stats
  statsValuesForTable <- reactive({
    data.frame(
      Variable = c("Min Width",
                   "Max Width",
                   "Min Length",
                   "Max Length",
                   "Min Injuries",
                   "Max Injuries",
                   "Min Fatalities",
                   "Max Fatalities"
      ),
      
      Value = as.character(c(
        input$minWidth,
        input$maxWidth,
        input$minLength,
        input$maxLength,
        input$injuries[1],
        input$injuries[2],
        input$fatalities[1],
        input$fatalities[2]
      )), stringsAsFactors = FALSE)
    
  })
  
  output$statsTable <- renderTable({
    statsValuesForTable()
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
      factor(fatMonth$Month, month.abb, ordered=TRUE)
      finalTable <- distinct(fatMonth)
    }
    
    if(ymhChoice() == "Hourly") {
      fatHour <- data
      fatHour$Hour <- factor(fatHour$Time)
      if(hourSetting() == 12){
        fatHour$Hour <- format(strptime(fatHour$Hour, "%H:%M:%S"),'%H')
      }else{
        fatHour$Hour24 <- format(strptime(fatHour$Hour, "%H:%M:%S"),'%H')
        fatHour$Hour <- format(strptime(fatHour$Time,"%H:%M:%S"), '%I %p')
      }
      fatHour <- group_by(fatHour, Hour)
      fatHour <- mutate(fatHour, Fatalities = sum(Fatalities))
      fatHour <- mutate(fatHour, Injuries = sum(Injuries))
      fatHour <- mutate(fatHour, Loss = sum(Loss))
      if(hourSetting() == 12){
        fatHour <- fatHour[order(fatHour$Hour),]
      }else{
        fatHour <- fatHour[order(fatHour$Hour24),]
      }
      fatHour <- select(fatHour, Hour, Fatalities, Injuries, Loss)
      fatHour <- distinct(fatHour)
      finalTable <- distinct(fatHour)
    }
    
    DT::datatable(finalTable, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  })
  
  #Magnitude Code
  #TODO Finish up
  output$magnitudeTable <- DT::renderDataTable({
    mag <- data
    
    if(ymhChoice() == "Yearly"){
      mag$Year <- format(as.POSIXct(mag$Date, format="%Y-%m-%d"),"%Y")
      mag <- group_by(mag, Year)
      mag <- summarise(mag, M1 = sum(Magnitude == 1), M2 = sum(Magnitude == 2), M3 = sum(Magnitude == 3), M4 = sum(Magnitude == 4), M5 = sum(Magnitude == 5), M0 = sum(Magnitude == 0))
      magTab <- distinct(mag)
    }
    
    if(ymhChoice() == "Monthly"){
      mag$Month <- format(as.POSIXct(mag$Date, format="%Y-%m-%d"),"%b")
      mag <- group_by(mag, Month)
      mag <- summarise(mag, M1 = sum(Magnitude == 1), M2 = sum(Magnitude == 2), M3 = sum(Magnitude == 3), M4 = sum(Magnitude == 4), M5 = sum(Magnitude == 5), M0 = sum(Magnitude == 0))
      magTab <- distinct(mag)
    }
    
    if(ymhChoice() == "Hourly"){
      mag <- data
      mag$Hour <- factor(mag$Time)
      
      if(hourSetting() == 12){
        mag$Hour <- format(strptime(mag$Hour, "%H:%M:%S"),'%H')
        mag <- group_by(mag, Hour)
        mag <- summarise(mag, M1 = sum(Magnitude == 1), M2 = sum(Magnitude == 2), M3 = sum(Magnitude == 3), M4 = sum(Magnitude == 4), M5 = sum(Magnitude == 5), M0 = sum(Magnitude == 0))
        mag <- mag[order(mag$Hour),]
      }else{
        mag$Hour <- format(strptime(mag$Hour, "%H:%M:%S"),'%H')
        mag$Hour12 <- format(strptime(mag$Time,"%H:%M:%S"), '%I %p')
        mag <- group_by(mag, Hour)
        mag <- summarise(mag, M1 = sum(Magnitude == 1), M2 = sum(Magnitude == 2), M3 = sum(Magnitude == 3), M4 = sum(Magnitude == 4), M5 = sum(Magnitude == 5), M0 = sum(Magnitude == 0))
        mag <- mag[order(mag$Hour),]
        mag$Hour <- format(strptime(mag$Hour, "%H"), '%I %p')
      }
      
      magTab <- distinct(mag)
    }
    
    DT::datatable(magTab, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  })
  
  output$numTornadoTable <- DT::renderDataTable({
    if(ymhChoice() == "Hourly") {
      numTor <- data
      numTor$Hour <- factor(numTor$Time)
      numTor$Hour24 <- format(strptime(numTor$Hour, "%H:%M:%S"),'%H')
      if(hourSetting() == 12){
        numTor$Hour <- format(strptime(numTor$Hour, "%H:%M:%S"),'%H')
      }else{
        numTor$Hour <- format(strptime(numTor$Time,"%H:%M:%S"), '%I %p')
      }
      numTor <- group_by(numTor, Hour)
      numTor <- as.data.frame(table(numTor$Hour24))
      if(hourSetting() == 12){
        numTor <- numTor[order(numTor$Var1),]
      }else{
        numTor$Var1 <- format(strptime(num$Var1, '%H'), '%I %p')
      }
      colnames(numTor) <- c("Hour","Number of Tornadoes")
      
      numTornadoTable <- distinct(numTor)
    }
    
    if(ymhChoice() == "Monthly") {
      numTor <- data
      numTor$Month <- format(as.POSIXct(numTor$Date, format="%Y-%m-%d"),"%b")
      numTor$Month <- factor(numTor$Month)
      numTor <- as.data.frame(table(numTor$Month))
      colnames(numTor) <- c("Month","Number of Tornadoes")
      
      numTornadoTable <- distinct(numTor)
    }
    
    if(ymhChoice() == "Yearly") {
      
      numTor <- data
      numTor$Year <- format(as.POSIXct(numTor$Date, format="%Y-%m-%d"),"%Y")
      numTor$Month <- factor(numTor$Year)
      numTor <- as.data.frame(table(numTor$Year))
      colnames(numTor) <- c("Year","Number of Tornadoes")
      
      numTornadoTable <- distinct(numTor)
    }
    
    DT::datatable(numTornadoTable, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  })
  
  
  # chart showing the fatalities for each year/month/hour
  output$fatalitiesLineChart <- renderPlotly({
    # Check which option is chosen
    if(ymhChoice() == "Yearly") {
      fatYear <- data
      fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
      fatYear <- group_by(fatYear, Year)
      fatYear <- mutate(fatYear, Fatalities = sum(Fatalities))
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
      if(hourSetting() == 12){
        fatHour$Hour <- format(strptime(fatHour$Hour, "%H:%M:%S"),'%H')
      }else{
        fatHour$Hour24 <- format(strptime(fatHour$Hour, "%H:%M:%S"),'%H')
        fatHour$Hour <- format(strptime(fatHour$Time,"%H:%M:%S"), '%I %p')
      }
      fatHour <- group_by(fatHour, Hour)
      fatHour <- mutate(fatHour, Fatalities = sum(Fatalities))
      #fatHour <- mutate(fatHour, Injuries = sum(Injuries))
      #fatHour <- mutate(fatHour, Loss = sum(Loss))
      if(hourSetting() == 12){
        fatHour <- fatHour[order(fatHour$Hour),]
      }else{
        fatHour <- fatHour[order(fatHour$Hour24),]
      }
      fatHour <- select(fatHour, Hour, Fatalities)
      newdt <- distinct(fatHour)
      
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
      if(hourSetting() == 12){
        fatHour$Hour <- format(strptime(fatHour$Hour, "%H:%M:%S"),'%H')
      }else{
        fatHour$Hour24 <- format(strptime(fatHour$Hour, "%H:%M:%S"),'%H')
        fatHour$Hour <- format(strptime(fatHour$Time,"%H:%M:%S"), '%I %p')
      }
      fatHour <- group_by(fatHour, Hour)
      fatHour <- mutate(fatHour, Loss = sum(Loss))
      if(hourSetting() == 12){
        fatHour <- fatHour[order(fatHour$Hour),]
      }else{
        fatHour <- fatHour[order(fatHour$Hour24),]
      }
      fatHour <- select(fatHour, Hour, Loss)
      newdt <- distinct(fatHour)
      
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
      fatYear <- mutate(fatYear, Loss = sum(Loss))
      fatYear <- select(fatYear, Year, Loss)
      fatYear <- distinct(fatYear)
      
      fatYear <- data.frame(fatYear$Year, fatYear$Loss)
      
      finalChart <- plot_ly(fatYear, x = ~fatYear.Year, y = ~fatYear.Loss, name = "Loss", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Year", tickangle = 45), yaxis = list (title = "Count"))
    }
    
    finalChart
  })
  
  # chart showing the injury for each year/month/hour 
  output$injuryLineChart <- renderPlotly({
    if(ymhChoice() == "Hourly") {
      fatHour <- data
      fatHour$Hour <- factor(fatHour$Time)
      if(hourSetting() == 12){
        fatHour$Hour <- format(strptime(fatHour$Hour, "%H:%M:%S"),'%H')
      }else{
        fatHour$Hour24 <- format(strptime(fatHour$Hour, "%H:%M:%S"),'%H')
        fatHour$Hour <- format(strptime(fatHour$Time,"%H:%M:%S"), '%I %p')
      }
      fatHour <- group_by(fatHour, Hour)
      fatHour <- mutate(fatHour, Injuries = sum(Injuries))
      if(hourSetting() == 12){
        fatHour <- fatHour[order(fatHour$Hour),]
      }else{
        fatHour <- fatHour[order(fatHour$Hour24),]
      }
      newdt <- distinct(fatHour)
      fatHour <- select(fatHour, Hour, Injuries)
      newdt <- distinct(fatHour)
      
      dat <- data.frame(newdt$Hour, newdt$Injuries)
      
      finalChart <- plot_ly(dat, x=~dat$newdt.Hour, y =~dat$newdt.Injuries, name = "Injury", type = "scatter", mode = "lines") %>%
        layout(xaxis= list(title = "Hour",
                           tickangle = 45, 
                           categoryorder = "array", 
                           categoryarray = c(newdt$Hour)), 
               yaxis = list (title = "Count"))
    }
    
    if(ymhChoice() == "Monthly") {
      fatMonth <- data
      fatMonth$Month <- format(as.POSIXct(fatMonth$Date, format="%Y-%m-%d"),"%b")
      fatMonth <- group_by(fatMonth, Month)
      fatMonth <- mutate(fatMonth, Injuries = sum(Injuries))
      fatMonth <- select(fatMonth, Month, Injuries)
      fatMonth <- distinct(fatMonth)
      
      dat <- data.frame(fatMonth$Month, fatMonth$Injuries)
      
      finalChart <- plot_ly(dat, x = ~dat$fatMonth.Month, y = ~dat$fatMonth.Injuries, name = "Injury", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Month", 
                            tickangle = 45, 
                            categoryorder = "array", 
                            categoryarray = c(fatMonth$Month)),
               yaxis=list(title="Count"))
    }
    
    if(ymhChoice() == "Yearly") {
      fatYear <- data
      fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
      fatYear <- group_by(fatYear, Year)
      fatYear <- mutate(fatYear, Injuries = sum(Injuries))
      fatYear <- select(fatYear, Year, Injuries)
      fatYear <- distinct(fatYear)
      
      fatYear <- data.frame(fatYear$Year, fatYear$Injuries)
      
      finalChart <- plot_ly(fatYear, x = ~fatYear.Year, y = ~fatYear.Injuries, name = "Loss", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Year", tickangle = 45), yaxis = list (title = "Count"))
    }
    
    finalChart
  })
  
  output$numTornadoLineChart <- renderPlotly({
    if(ymhChoice() == "Hourly") {
      numTor <- data
      numTor$Hour <- factor(numTor$Time)
      numTor$Hour24 <- format(strptime(numTor$Hour, "%H:%M:%S"),'%H')
      if(hourSetting() == 12){
        numTor$Hour <- format(strptime(numTor$Hour, "%H:%M:%S"),'%H')
      }else{
        numTor$Hour <- format(strptime(numTor$Time,"%H:%M:%S"), '%I %p')
      }
      numTor <- group_by(numTor, Hour)
      numTor <- as.data.frame(table(numTor$Hour24))
      if(hourSetting() == 12){
        numTor <- numTor[order(numTor$Var1),]
      }else{
        numTor$Var1 <- format(strptime(num$Var1, '%H'), '%I %p')
      }
      
      dat <- data.frame(numTor)
      
      if(hourSetting()==12)
      {
        finalChart <- plot_ly(dat, x=~dat$Var1, y =~dat$Freq, name = "Number of Tornadoes", type = "scatter", mode = "lines") %>%
          layout(xaxis= list(title = "Hour", tickangle = 45), 
                 yaxis = list (title = "Count"))
        
      }else{
        finalChart <- plot_ly(dat, x=~dat$Var1, y =~dat$Freq, name = "Number of Tornadoes", type = "scatter", mode = "lines") %>%
          layout(xaxis= list(title = "Hour",
                             tickangle = 45, 
                             categoryorder = "array", 
                             categoryarray = c(numTor$Var1)), 
                 yaxis = list (title = "Count"))
      }
    }
    
    if(ymhChoice() == "Monthly") {
      numTor <- data
      numTor$Month <- format(as.POSIXct(numTor$Date, format="%Y-%m-%d"),"%b")
      numTor$Month <- factor(numTor$Month)
      numTor <- as.data.frame(table(numTor$Month))
      
      dat <- data.frame(numTor)
      
      finalChart <- plot_ly(dat, x = ~dat$Var1, y = ~dat$Freq, name = "Number of Tornadoes", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Month", 
                            tickangle = 45, 
                            categoryorder = "array", 
                            categoryarray = c(numTor$Month)),
               yaxis=list(title="Count"))
    }
    
    if(ymhChoice() == "Yearly") {
      
      numTor <- data
      numTor$Year <- format(as.POSIXct(numTor$Date, format="%Y-%m-%d"),"%Y")
      numTor$Month <- factor(numTor$Year)
      numTor <- as.data.frame(table(numTor$Year))
      
      dat <- data.frame(numTor)
      
      finalChart <- plot_ly(dat, x = ~dat$Var1, y = ~dat$Freq, name = "Number of Tornadoes", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Year", tickangle = 45), yaxis = list (title = "Count"))
    }
    
    finalChart
  })
  
  
  # Leaflet map for all tornadoes
  # Need to add init markers
  output$map <- renderLeaflet({
    
    # -1 means show all tornadoes
    if (magnitudeChoice() == -1){
      map1 <- dplyr::filter(data, data$"End Lon" != 0 &
                              data$Length >= minLength() & data$Length <= maxLength() &
                              data$Width >= minWidth() & data$Width <= maxWidth() &
                              data$Injuries >= minInjury() & data$Injuries <= maxInjury() &
                              data$Fatalities >= minFatal() & data$Fatalities <= maxFatal())
      m <- leaflet::leaflet()
      m <- leaflet::addTiles(m)
      for(i in 1:nrow(map1)){
        m <- leaflet::addPolylines(m, lat = as.numeric(map1[i, c(8, 10)]), lng = as.numeric(map1[i, c(9, 11)]))
      }
    } else {
      map1 <- dplyr::filter(data, data$"End Lon" != 0 & 
                              data$Magnitude == magnitudeChoice() &
                              data$Length >= minLength() & data$Length <= maxLength() &
                              data$Width >= minWidth() & data$Width <= maxWidth() & 
                              data$Injuries >= minInjury() & data$Injuries <= maxInjury() &
                              data$Fatalities >= minFatal() & data$Fatalities <= maxFatal())
      m <- leaflet::leaflet()
      m <- leaflet::addTiles(m)
      for(i in 1:nrow(map1)){
        m <- leaflet::addPolylines(m, lat = as.numeric(map1[i, c(8, 10)]), lng = as.numeric(map1[i, c(9, 11)]))
      }
    }
    
    
    
    # Maps the tornado touch down in hopes to counteract
    # Long and Lat values of 0 which end up in Africa
    #m <- addMarkers(m, lat = map2$`Start Lat`, lng = map2$`Start Lon`)
    m
  })
}

shinyApp(ui, server)