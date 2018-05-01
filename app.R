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
library(DT)
library(plotly)
library(dplyr)
library(leaflet)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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
stateData <- data
data <- dplyr::filter(data, data$State == "IL")

data2<- data
data2$Year <- format(as.POSIXct(data2$Date, format="%Y-%m-%d"),"%Y")
tmp <- c()
for(row in 1:nrow(data2)){
  year <- data2[row, "Year"]
  if(year < 1996){
    loss <- data2[row, "Loss"]
    if(loss == 0)
      data2[row, "Loss"] <- 0
    
    else if(loss == 1)
      data2[row,"Loss"] <- 50
    
    else if(loss == 2)
      data2[row,"Loss"] <- 250
    
    else if(loss == 3)
      data2[row,"Loss"] <- 3000
    
    else if(loss == 4)
      data2[row,"Loss"] <- 30000
    
    else if(loss == 5)
      data2[row,"Loss"] <- 250000
    
    else if(loss == 6)
      data2[row,"Loss"] <- 25000000
    
    else if(loss == 7)
      data2[row, "Loss"] <- 25000000
    
    else if(loss == 8)
      data2[row,"Loss"] <- 250000000
    
    else
      data2[row,"Loss"] <- 2500000000
    
    tmp <- rbind(tmp, data2[row,])
    
  }
  else if(year >= 1996 & year < 2016)
  {
    val <- data2[row, c(7)]
    data2[row, "Loss"] <- (val*1000000)
    tmp <- rbind(tmp, data2[row,])
  }
  else{
    data2[row, "Loss"] <- data2[row,"Loss"]
    tmp <- rbind(tmp, data2[row,])
  }
  
}

data$Loss <- tmp$Loss
rm(data2)
rm(tmp)


# Save data in RData file
saveRDS(data, "data.rds")

# Example - Load RData file
# tempData <- readRDS("data.rds")

# create top 10 data
top10 <- data[ which(
  (data$Loss == 25000000 & data$Width == 200 & data$Length == 15 & data$Injuries == 500 & data$Fatalities == 33)   | 
    (data$Loss == 250000 & data$Width == 1200 & data$Length == 25.5 & data$Injuries == 450 & data$Fatalities == 24)   |
    (data$Loss == 935225000 & data$Width == 880 & data$Length == 46.36 & data$Injuries == 125 & data$Fatalities == 3) |
    (data$Loss == 250000 & data$Width == 600 & data$Length == 16.4 & data$Injuries == 350 & data$Fatalities == 29)    |
    (data$Loss == 25000000 & data$Width == 300 & data$Length == 28.3 & data$Injuries == 200 & data$Fatalities == 11) |
    (data$Loss == 250000000 & data$Width == 400 & data$Length == 17 & data$Injuries == 181 & data$Fatalities == 10)   |
    (data$Loss == 25000000 & data$Width == 2630 & data$Length == 14 & data$Injuries == 69 & data$Fatalities == 2)    |
    (data$Loss == 2000000 & data$Width == 325 & data$Length == 26.09 & data$Injuries == 108 & data$Fatalities == 8)   |
    (data$Loss == 25000000 & data$Width == 120 & data$Length == 35.9 & data$Injuries == 90 & data$Fatalities == 2)    |
    (data$Loss == 25000000 & data$Width == 150 & data$Length == 8.80 & data$Injuries == 100 & data$Fatalities == 1)
) , ]


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
      radioButtons('measurement', 'Measurements:',
                   c('Imperial' = 1,
                     'Metric' = 0)             
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
                        h4("Loss"),
                        splitLayout(numericInput("minLoss", label = "min", value = 0, min = 0, max = 1000000000),
                                    numericInput("maxLoss", label = "max", value = 1000000000, min = 0, max = 1000000000)),
                        h4("Width (yd)"),
                        splitLayout(numericInput("minWidth", label = "min", value = 0, min = 0, max = 2630),
                                    numericInput("maxWidth", label = "max", value = 2630, min = 0, max = 2630)),
                        h4("Length (mi)"),
                        splitLayout(numericInput("minLength", label = "min", value = 0, min = 0, max = 157),
                                    numericInput("maxLength", label = "max", value = 157, min = 0, max = 157))
                 ),
                 
                 
                 column(width = 4, align = "center",
                        h4("Magnitude/Top 10 destructive"),
                        selectInput("magnitudeLvl", "",
                                    c("All" = -1 , "Top 10 destructive" = -2, "0" = 0, "1" = 1, "2" = 2, "3" = 3, "4"= 4, "5" = 5)),       
                        
                        h4("Injuries"),
                        sliderInput("injuries", "",step = 5,
                                    min = 0, max = 500,
                                    value = c(0,500)),
                        h4("Fatalities"),
                        sliderInput("fatalities", "",step = 1,
                                    min = 0, max = 33,
                                    value = c(0,33))
                        
                 ),
                 #Playback slider for map
                 column(width = 4, align = "center",
                        checkboxInput('playback', 'Yearly Playback', value = FALSE),
                        sliderInput("yearlyPlayback", "Year:",
                                    min = 1950, max = 2016,
                                    value = 1950, step = 1,
                                    sep = ",",
                                    animate = TRUE),
                        h4("Stats:"),
                        textOutput("numberDataPoints"),
                        tableOutput("statsTable"),
                        h4("County", align = "center"),
                        selectInput("county","",c("All"= -1, "Adams" = 1, "Alexander" = 3, "Bond" = 5, "Boone" = 7, "Brown" = 9, "Bureau" = 11, "Calhoun" = 13,
                                                  "Carroll" = 15, "Cass" = 17, "Champaign" = 19, "Christian" = 21, "Clark"= 23, "Clay" = 25, "Clinton" = 27,
                                                  "Coles" = 29, "Cook" = 31, "Crawford" = 33, "Cumberland" = 35,  "DeKalb" = 37, "DeWitt" = 39, "Douglas" = 41,
                                                  "DuPage" = 43, "Edgar" = 45, "Edwards" = 47, "Effingham" = 49, "Fayette" = 51, "Ford" = 53, "Franklin" = 55,
                                                  "Fulton" = 57, "Gallatin" = 59, "Greene" = 61, "Grundy" = 63, "Hamilton" = 65, "Hancock" = 67, "Hardin" = 69,
                                                  "Henderson" = 71, "Henry" = 73, "Iroquois" = 75, "Jackson" = 77, "Jasper" = 79, "Jefferson" = 81, "Jersey" = 83,
                                                  "Jo Daviess" = 85, "Johnson" = 87, "Kane" = 89, "Kankakee" = 91, "Kendall" = 93, "Knox" = 95, "Lake" = 97,
                                                  "Lasalle" = 99, "Lawrence" = 101, "Lee" = 103, "Livingston" = 105, "Logan" = 107, "Macon" = 115, "Macoupin" = 117,
                                                  "Madison" = 119, "Marion" = 121, "Marshall" = 123, "Mason" = 125, "Massac" = 127, "McDonough" = 109, "McHenry" = 111,
                                                  "McLean" = 113, "Menard" = 129, "Mercer" = 131, "Monroe" = 133, "Montgomery" = 135, "Morgan" = 137, "Moultrie" = 139,
                                                  "Ogle" = 141, "Peoria" = 143, "Perry" = 145, "Piatt" = 147, "Pike" = 149, "Pope" = 151, "Pulaski" = 153, "Putnam" = 155,
                                                  "Randolph" = 157, "Richland" = 159, "Rock Island" = 161, "Saline" = 165, "Sangamon" = 167, "Schuyler" = 169, "Scott" = 171,
                                                  "Shelby" = 173, "St. Clair" = 163, "Stark" = 175, "Stephenson" = 177, "Tazewell" = 179, "Union" = 181, "Vermilion" = 183,
                                                  "Wabash" = 185, "Warren" = 187, "Washingtom" = 189, "Wayne" = 191, "White" = 193, "Whiteside" = 195, "Will" = 197,
                                                  "Williamson" = 199, "Winnebago" = 201, "Woodford" = 203)
                 )))
      ),
      tabPanel("Charts", 
               fluidRow(
                 splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                             box(title = "Number of Tornadoes", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("numTornadoLineChart")),
                             box(title = "Number of Tornadoes with Magnitude", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("magTornadoLineChart")),
                             box(title = "Distance From Chicago", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("distanceLineChart")),
                             box(title = "15 Worst Counties for Tornadoes", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("CountiesLineChart"))
                 ) 
               ),
               fluidRow(
                 splitLayout(cellWidths = c("33%", "33%", "33%"),
                             box(title = "Number of Fatalities", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("fatalitiesLineChart")),
                             box(title = "Loss in US Dollar", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("lossLineChart")),
                             box(title = "Number of Injuries", solidHeader = TRUE, status = "primary", width = 12, plotlyOutput("injuryLineChart"))
                 ) 
               )
      ),
      tabPanel("Tables", 
               fluidRow(
                 splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                             box(title = "Fatalities, Loss in USD, and Injuries", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("fatalitiesInjuriesLossTable")),
                             box(title = "Magnitude", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("magnitudeTable")),
                             box(title = "Number of Tornadoes", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("numTornadoTable")),
                             box(title = "Distance From Chicago", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("distanceTable")),
                             box(title = "County Data", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("countiesTable"))
                 )
               )
      ),
      
      tabPanel("State Compare", 
               fluidRow(
                 column(width = 4, align = "center",
                        h4("Illinois:"),
                        selectInput("Illinois", "Illinois:",c("Illinois")),
                        box(title = "IL Fatalities, Loss in USD, and Injuries", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("ILFatTable")),
                        box(title = "IL Magnitude", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("ILMagTable")),
                        box(title = "IL Number of Tornadoes", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("ILTornadoTable"))
                 ),
                 column(width = 4, alight = "center",
                        h4("Choose a State:"),
                        selectInput("stateChoice", "Choose a State:", c("Alaska" = 'AK', "Alabama" = "AL", "Arkansas" = "AR", "Arizona" = "AZ", "California" = "CA",
                                                                        "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "Florida" = "FL", "Georgia" = "GA",
                                                                        "Hawaii" = "HI", "Iowa" = "IA", "Idaho" = "ID", "Indiana" = "IN", "Kansas" = "KA",
                                                                        "Kentucky" = "KY", "Lousiana" = "LA", "Massachusettes" ="MA", "Maryland" = "MD",
                                                                        "Maine" = "ME", "Michigan" = "MI", "Minnesota" = "MN", "Missouri" = "MI", "Mississippi" = "MS",
                                                                        "Montana" = "MT", "North Carolina" = "NC", "North Dakota" = "ND", "Nebraska" = "NE",
                                                                        "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "Nevada" = "NV", 
                                                                        "New York" = "NY", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA",
                                                                        "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD", "Tennessee" = "TN",
                                                                        "Texas" = "TX", "Utah" = "UT", "Virginia" = "VA", "Vermont" = "VT", "Washington" = "WA", 
                                                                        "Wisconson" = "WI", "West Virginia" = "WV", "Wyoming" = "WY")),
                        box(title = "Fatalities, Loss in USD, and Injuries", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("stateFatTable")),
                        box(title = "Magnitude", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("stateMagTable")),
                        box(title = "Number of Tornadoes", solidHeader = TRUE, status = "primary", width = 8, dataTableOutput("stateTornadoTable"))
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
  
  getPlayback <- reactive ({
    input$playback
  })
  
  getState <- reactive ({
    input$stateChoice
  })
  
  getYearPlay <- reactive ({
    input$yearlyPlayback
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
  
  # Loss
  minLoss <- reactive ({
    as.numeric(input$minLoss)
  })
  
  maxLoss <- reactive ({
    as.numeric(input$maxLoss)
  })
  
  # Fatalities
  minFatal <- reactive ({
    input$fatalities[1]
  })
  
  maxFatal <- reactive ({
    input$fatalities[2]
  })
  
  # Injuries
  minInjury <- reactive ({
    input$injuries[1]
  })
  
  maxInjury <- reactive ({
    input$injuries[2]
  })
  
  magnitudeChoice <- reactive ({
    input$magnitudeLvl
  })
  
  hourSetting <- reactive ({
    input$hours
  })
  
  measurementSetting <- reactive ({
    input$measurement
  })
  
  countyChoice <- reactive ({
    input$county
  })
  
  yearChoice <- reactive ({
    input$year
  })
  
  filteredData <- reactive({
    
    if (magnitudeChoice() == -1) {
      tempData <- dplyr::filter(data, data$"End Lon" != 0 &
                                  data$Length >= minLength() & data$Length <= maxLength() &
                                  data$Width >= minWidth() & data$Width <= maxWidth() &
                                  data$Injuries >= minInjury() & data$Injuries <= maxInjury() &
                                  data$Fatalities >= minFatal() & data$Fatalities <= maxFatal() &
                                  data$Loss >= minLoss() & data$Loss <= maxLoss())
      if(countyChoice() != -1){
        tempData <- dplyr::filter(tempData, tempData$F1 == countyChoice())
      }
      if(measurementSetting() == 0){
        tempData$Width <- tempData$Width * 0.914
        tempData$Length <- tempData$Length * 1.60934
      }
      
    } else if (magnitudeChoice() == -2) {
      tempData <- top10
      if(measurementSetting() == 0){
        tempData$Width <- tempData$Width * 0.914
        tempData$Length <- tempData$Length * 1.60934
      }
    } else {
      tempData <- dplyr::filter(data, data$"End Lon" != 0 & 
                                  data$Magnitude == magnitudeChoice() &
                                  data$Length >= minLength() & data$Length <= maxLength() &
                                  data$Width >= minWidth() & data$Width <= maxWidth() & 
                                  data$Injuries >= minInjury() & data$Injuries <= maxInjury() &
                                  data$Fatalities >= minFatal() & data$Fatalities <= maxFatal() &
                                  data$Loss >= minLoss() & data$Loss <= maxLoss())
      if(countyChoice() != -1){
        tempData <- dplyr::filter(tempData, tempData$F1 == countyChoice())
      }
      if(measurementSetting() == 0){
        tempData$Width <- tempData$Width * 0.914
        tempData$Length <- tempData$Length * 1.60934
      }
    }
    
    
    tempData
  })
  
  # Renders table output that shows some key stats
  statsValuesForTable <- reactive({
    
    tempData <- filteredData()
    
    # width
    meanWidth = format(round(mean(tempData$Width),2), nsmall = 2, big.mark = ",")
    medianWidth = format(round(median(tempData$Width),2), nsmall = 2, big.mark = ",")
    modeWidth <- format(round(Mode(tempData$Width),2), nsmall = 2, big.mark = ",")
    totalWidth <- format(round(sum(tempData$Width),2), nsmall = 2, big.mark = ",")
    
    # length
    meanLength = format(round(mean(tempData$Length),2), nsmall = 2, big.mark = ",")
    medianLength = format(round(median(tempData$Length),2), nsmall = 2, big.mark = ",")
    modeLength <- format(round(Mode(tempData$Length),2), nsmall = 2, big.mark = ",")
    totalLength <- format(round(sum(tempData$Length),2), nsmall = 2, big.mark = ",")
    
    # injuries
    meanInjuries = format(round(mean(tempData$Injuries),2), nsmall = 2, big.mark = ",")
    medianInjuries = format(round(median(tempData$Injuries),2), nsmall = 2, big.mark = ",")
    modeInjuries <- format(round(Mode(tempData$Injuries),2), nsmall = 2, big.mark = ",")
    totalInjuries <- format(round(sum(tempData$Injuries),2), nsmall = 2, big.mark = ",")
    
    # fatalities
    meanFatalities = format(round(mean(tempData$Fatalities),2), nsmall = 2, big.mark = ",")
    medianFatalities = format(round(median(tempData$Fatalities),2), nsmall = 2, big.mark = ",")
    modeFatalities <- format(round(Mode(tempData$Fatalities),2), nsmall = 2, big.mark = ",")
    totalFatalities <- format(round(sum(tempData$Fatalities),2), nsmall = 2, big.mark = ",")
    
    # loss
    meanLoss = format(round(mean(tempData$Loss),2), nsmall = 2, big.mark = ",", scientific = FALSE)
    medianLoss = format(round(median(tempData$Loss),2), nsmall = 2, big.mark = ",", scientific = FALSE)
    modeLoss <- format(round(Mode(tempData$Loss),2), nsmall = 2, big.mark = ",", scientific = FALSE)
    totalLoss <- format(round(sum(tempData$Loss),2), nsmall = 2, big.mark = ",", scientific = FALSE)
    
    data.frame(
      Variable = c("Loss",
                   "Width",
                   "Length",
                   "Injuries",
                   "Fatalities"
      ),
      
      Mean = as.character(c(
        meanLoss,
        meanWidth,
        meanLength,
        meanInjuries,
        meanFatalities
      )),
      
      Median = as.character(c(
        medianLoss,
        medianWidth,
        medianLength,
        medianInjuries,
        medianFatalities
      )),
      
      Mode = as.character(c(
        modeLoss,
        modeWidth,
        modeLength,
        modeInjuries,
        modeFatalities
      )),
      Total = as.character(c(
        totalLoss,
        totalWidth,
        totalLength,
        totalInjuries,
        totalFatalities
      )),
      stringsAsFactors = FALSE)
    
  })
  
  numDataPoints <- reactive({
    
    tempData <- filteredData()
    
    numDP = nrow(tempData)
    format(numDP,big.mark = ",")
  })
  
  output$numberDataPoints <- renderText({
    paste("# Data Points: ", as.character(numDataPoints()))
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
      numTorArray <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
      mag <- mag[match(numTorArray, mag$Month),]
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
  
  output$ILFatTable <- DT::renderDataTable({
    
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
  
  output$ILMagTable <- DT::renderDataTable({
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
      numTorArray <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
      mag <- mag[match(numTorArray, mag$Month),]
    
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
  
  output$ILTornadoTable <- DT::renderDataTable({
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
        numTor$Var1 <- format(strptime(numTor$Var1, '%H'), '%I %p')
      }
      colnames(numTor) <- c("Hour","Number of Tornadoes")
      
      numTornadoTable <- distinct(numTor)
    }
    
    if(ymhChoice() == "Monthly") {
      numTor <- data
      numTor$Month <- format(as.POSIXct(numTor$Date, format="%Y-%m-%d"),"%b")
      numTor$Month <- factor(numTor$Month)
      numTor <- as.data.frame(table(numTor$Month))
      numTorArray <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
      numTor <- numTor[match(numTorArray, numTor$Var1),]
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
  
  output$stateFatTable <- DT::renderDataTable({
    
    if(ymhChoice() == "Yearly") {
      fatYear <- dplyr::filter(stateData, stateData$State == getState())
      fatYear$Year <- format(as.POSIXct(fatYear$Date, format="%Y-%m-%d"),"%Y")
      fatYear <- group_by(fatYear, Year)
      fatYear <- mutate(fatYear, Fatalities = sum(Fatalities))
      fatYear <- mutate(fatYear, Injuries = sum(Injuries))
      fatYear <- mutate(fatYear, Loss = sum(Loss))
      fatYear <- select(fatYear, Year, Fatalities, Injuries, Loss)
      
      finalTable <- distinct(fatYear)
    }
    
    if(ymhChoice() == "Monthly") {
      fatMonth <- dplyr::filter(stateData, stateData$State == getState())
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
      fatHour <- dplyr::filter(stateData, stateData$State == getState())
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
  
  output$stateMagTable <- DT::renderDataTable({
    mag <- dplyr::filter(stateData, stateData$State == getState())
    
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
      numTorArray <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
      mag <- mag[match(numTorArray, mag$Month),]
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
  
  output$stateTornadoTable <- DT::renderDataTable({
    if(ymhChoice() == "Hourly") {
      numTor <- dplyr::filter(stateData, stateData$State == getState())
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
        numTor$Var1 <- format(strptime(numTor$Var1, '%H'), '%I %p')
      }
      colnames(numTor) <- c("Hour","Number of Tornadoes")
      
      numTornadoTable <- distinct(numTor)
    }
    
    if(ymhChoice() == "Monthly") {
      numTor <- dplyr::filter(stateData, stateData$State == getState())
      numTor$Month <- format(as.POSIXct(numTor$Date, format="%Y-%m-%d"),"%b")
      numTor$Month <- factor(numTor$Month)
      numTor <- as.data.frame(table(numTor$Month))
      numTorArray <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
      numTor <- numTor[match(numTorArray, numTor$Var1),]
      colnames(numTor) <- c("Month","Number of Tornadoes")
      
      numTornadoTable <- distinct(numTor)
    }
    
    if(ymhChoice() == "Yearly") {
      numTor <- dplyr::filter(stateData, stateData$State == getState())
      numTor$Year <- format(as.POSIXct(numTor$Date, format="%Y-%m-%d"),"%Y")
      numTor$Month <- factor(numTor$Year)
      numTor <- as.data.frame(table(numTor$Year))
      colnames(numTor) <- c("Year","Number of Tornadoes")
      
      numTornadoTable <- distinct(numTor)
    }
    
    DT::datatable(numTornadoTable, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
  })
  
  #Distance from Chicago
  output$distanceTable <- DT::renderDataTable({
    mag <- data
    #mag$distance <- mag$Date
    test <- c()
    for(i in 1:nrow(mag)){
      #Lat +                    Long -
      temp <- sqrt((mag[i, c(8)]-41.87)^2+(mag[i, c(9)]+87.62)^2)
      if(measurementSetting() == 1){
        mag$Distance <- (temp*69)
      }
      else{
        mag$Distance <- ((temp*69)*1.60934)
      }
      test <- rbind(test, mag[i,])
      
    }
    
    tmp <- c()
    if(measurementSetting() == 1){
      tmp$one <- as.data.frame(table(test$Distance < 50))
      tmp$one <- tmp$one[-c(1),] #delete the row we dont want to use
      tmp$two <-as.data.frame(table(test$Distance >= 50 & test$Distance <=99.9999999))
      tmp$two <- tmp$two[-c(1),]
      tmp$three <- as.data.frame(table(test$Distance >= 100 & test$Distance <=149.999999))
      tmp$three <- tmp$three[-c(1),]
      tmp$four <- as.data.frame(table(test$Distance >= 150 & test$Distance <=199.9999999))
      tmp$four <- tmp$four[-c(1),]
      tmp$five <- as.data.frame(table(test$Distance >= 200 & test$Distance <= 249.999999))
      tmp$five <- tmp$five[-c(1),]
      tmp$six <- as.data.frame(table(test$Distance >= 250 & test$Distance <= 299.999999))
      tmp$six <- tmp$six[-c(1),]
      tmp$seven <- as.data.frame(table(test$Distance >=300 & test$Distance <= 349.999999))
      tmp$seven <- tmp$seven[-c(1),]
      tmp$eight <- as.data.frame(table(test$Distance >=350 & test$Distance <= 5000))
      tmp$eight <-tmp$eight[-c(1),]
      Final <- c()
      Final <- data.frame(x = c('<50', '50-99.99', '100-149.99', '150-199.99', '200-249.99', '250-299.99', '300-349.99', '>350'), y = c(tmp$one$Freq, tmp$two$Freq, tmp$three$Freq, tmp$four$Freq, tmp$five$Freq, tmp$six$Freq, tmp$seven$Freq, tmp$eight$Freq))
      colnames(Final) <- c('Distance (Miles)', 'Number of Tornadoes')
    }
    else{
      tmp$one <- as.data.frame(table(test$Distance < 75))
      tmp$one <- tmp$one[-c(1),] #delete the row we dont want to use
      tmp$two <-as.data.frame(table(test$Distance >= 75 & test$Distance <149.99))
      tmp$two <- tmp$two[-c(1),]
      tmp$three <- as.data.frame(table(test$Distance >= 150 & test$Distance <=224.999999))
      tmp$three <- tmp$three[-c(1),]
      tmp$four <- as.data.frame(table(test$Distance >=225 & test$Distance <=299.9999999))
      tmp$four <- tmp$four[-c(1),]
      tmp$five <- as.data.frame(table(test$Distance >= 300 & test$Distance <= 374.999999))
      tmp$five <- tmp$five[-c(1),]
      tmp$six <- as.data.frame(table(test$Distance >= 375 & test$Distance <= 449.999999))
      tmp$six <- tmp$six[-c(1),]
      tmp$seven <- as.data.frame(table(test$Distance >=450 & test$Distance <= 524.999999))
      tmp$seven <- tmp$seven[-c(1),]
      tmp$eight <- as.data.frame(table(test$Distance >=525 & test$Distance <= 600))
      tmp$eight <-tmp$eight[-c(1),]
      Final <- c()
      Final <- data.frame(x = c('<75', '75-149.99', '150-224.99', '225-299.99', '300-374.99', '375-449.99', '450-542.99', '>525'), y = c(tmp$one$Freq, tmp$two$Freq, tmp$three$Freq, tmp$four$Freq, tmp$five$Freq, tmp$six$Freq, tmp$seven$Freq, tmp$eight$Freq))
      colnames(Final) <- c('Distance (Kilometers)', 'Number of Tornadoes')
    }
    DT::datatable(Final, options = list(pageLength = 8, lengthChange = FALSE, searching = FALSE))
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
        numTor$Var1 <- format(strptime(numTor$Var1, '%H'), '%I %p')
      }
      colnames(numTor) <- c("Hour","Number of Tornadoes")
      
      numTornadoTable <- distinct(numTor)
    }
    
    if(ymhChoice() == "Monthly") {
      numTor <- data
      numTor$Month <- format(as.POSIXct(numTor$Date, format="%Y-%m-%d"),"%b")
      numTor$Month <- factor(numTor$Month)
      numTor <- as.data.frame(table(numTor$Month))
      numTorArray <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
      numTor <- numTor[match(numTorArray, numTor$Var1),]
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
  
  #Counties most hit by tornados
  output$countiesTable <- DT::renderDataTable({
    
    county <- data
    colnames(county)[14] <- "FIPS.County"
    county <- full_join(county, fips, by = 'FIPS.County')
    countySum <- count(county, County.Name)
    colnames(countySum) <- c("County", "Number of Tornadoes")
    DT::datatable(countySum, options = list(pageLength = 6, lengthChange = FALSE, searching = FALSE))
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
        numTor$Var1 <- format(strptime(numTor$Var1, '%H'), '%I %p')
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
      numTorArray <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
      numTor <- numTor[match(numTorArray, numTor$Var1),]
      dat <- data.frame(numTor)
      
      finalChart <- plot_ly(dat, x = ~dat$Var1, y = ~dat$Freq, name = "Number of Tornadoes", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Month", 
                            tickangle = 45, 
                            categoryorder = "array", 
                            categoryarray =numTorArray),
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
  
  output$magTornadoLineChart <- renderPlotly({
    mag <- data
    if(ymhChoice() == "Yearly"){
      mag$Year <- format(as.POSIXct(mag$Date, format="%Y-%m-%d"),"%Y")
      mag <- group_by(mag, Year)
      mag <- summarise(mag, M1 = sum(Magnitude == 1), M2 = sum(Magnitude == 2), M3 = sum(Magnitude == 3), M4 = sum(Magnitude == 4), M5 = sum(Magnitude == 5), M0 = sum(Magnitude == 0))
      dat <- data.frame(mag)
      
      finalChart <- plot_ly(dat, x=~dat$Year, y =~dat$M1, text = ~paste0((signif((dat$M1/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 1", type = "bar") %>%
        add_trace(y = ~dat$M2, text = ~paste0((signif((dat$M2/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 2") %>%
        add_trace(y = ~dat$M3, text = ~paste0((signif((dat$M3/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 3") %>%
        add_trace(y = ~dat$M4, text = ~paste0((signif((dat$M4/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 4") %>%
        add_trace(y = ~dat$M5, text = ~paste0((signif((dat$M5/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 5") %>%
        add_trace(y = ~dat$M0, text = ~paste0((signif((dat$M0/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude Unknown") %>%
        layout(xaxis=list(title = "Year", tickangle =45), yaxis = list (title = "Magnitude"),barmode = "stack")
    }
    
    if(ymhChoice() == "Monthly"){
      mag$Month <- format(as.POSIXct(mag$Date, format="%Y-%m-%d"),"%b")
      mag <- group_by(mag, Month)
      mag <- summarise(mag, M1 = sum(Magnitude == 1), M2 = sum(Magnitude == 2), M3 = sum(Magnitude == 3), M4 = sum(Magnitude == 4), M5 = sum(Magnitude == 5), M0 = sum(Magnitude == 0))
      numTorArray <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",  "Oct", "Nov", "Dec")
      mag <- mag[match(numTorArray, mag$Month),]
      dat <- data.frame(mag)
      
      finalChart <-plot_ly(dat, x=~dat$Month, y =~dat$M1, text = ~paste0((signif((dat$M1/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 1", type = "bar") %>%
        add_trace(y = ~dat$M2, text = ~paste0((signif((dat$M2/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 2") %>%
        add_trace(y = ~dat$M3, text = ~paste0((signif((dat$M3/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 3") %>%
        add_trace(y = ~dat$M4, text = ~paste0((signif((dat$M4/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 4") %>%
        add_trace(y = ~dat$M5, text = ~paste0((signif((dat$M5/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 5") %>%
        add_trace(y = ~dat$M0, text = ~paste0((signif((dat$M0/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude Unknown") %>%
        layout(xaxis=list(title = "Month", tickangle =45,categoryorder = "array", categoryarray = c(mag$Month)), 
               yaxis = list (title = "Magnitude"),barmode = "stack")
    }
    
    if(ymhChoice() == "Hourly"){
      mag <- data
      mag$Hour <- factor(mag$Time)
      
      if(hourSetting() == 12){
        mag$Hour <- format(strptime(mag$Hour, "%H:%M:%S"),'%H')
        mag <- group_by(mag, Hour)
        mag <- summarise(mag, M1 = sum(Magnitude == 1), M2 = sum(Magnitude == 2), M3 = sum(Magnitude == 3), M4 = sum(Magnitude == 4), M5 = sum(Magnitude == 5), M0 = sum(Magnitude == 0))
        mag <- mag[order(mag$Hour),]
        dat <- data.frame(mag)
        finalChart <- plot_ly(dat, x=~dat$Hour, y =~dat$M1, text = ~paste0((signif((dat$M1/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 1", type = "bar") %>%
          add_trace(y = ~dat$M2, text = ~paste0((signif((dat$M2/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 2") %>%
          add_trace(y = ~dat$M3, text = ~paste0((signif((dat$M3/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 3") %>%
          add_trace(y = ~dat$M4, text = ~paste0((signif((dat$M4/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 4") %>%
          add_trace(y = ~dat$M5, text = ~paste0((signif((dat$M5/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 5") %>%
          add_trace(y = ~dat$M0, text = ~paste0((signif((dat$M0/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude Unknown") %>%
          layout(xaxis=list(title = "Hour", tickangle =45), yaxis = list (title = "Magnitude"),barmode = "stack")
        
      }else{
        mag$Hour <- format(strptime(mag$Hour, "%H:%M:%S"),'%H')
        mag$Hour12 <- format(strptime(mag$Time,"%H:%M:%S"), '%I %p')
        mag <- group_by(mag, Hour)
        mag <- summarise(mag, M1 = sum(Magnitude == 1), M2 = sum(Magnitude == 2), M3 = sum(Magnitude == 3), M4 = sum(Magnitude == 4), M5 = sum(Magnitude == 5), M0 = sum(Magnitude == 0))
        mag <- mag[order(mag$Hour),]
        mag$Hour <- format(strptime(mag$Hour, "%H"), '%I %p')
        dat <- data.frame(mag)
        
        finalChart <- plot_ly(dat, x=~dat$Hour, y =~dat$M1, text = ~paste0((signif((dat$M1/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 1", type = "bar") %>%
          add_trace(y = ~dat$M2, text = ~paste0((signif((dat$M2/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 2") %>%
          add_trace(y = ~dat$M3, text = ~paste0((signif((dat$M3/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 3") %>%
          add_trace(y = ~dat$M4, text = ~paste0((signif((dat$M4/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 4") %>%
          add_trace(y = ~dat$M5, text = ~paste0((signif((dat$M5/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude 5") %>%
          add_trace(y = ~dat$M0, text = ~paste0((signif((dat$M0/(dat$M1+dat$M2+dat$M3+dat$M4+dat$M5+dat$M0)), 2)*100),"%"), name = "Magnitude Unknown") %>%
          layout(xaxis=list(title = "Hour", tickangle =45, categoryorder = "array", 
                            categoryarray = c(mag$Hour)), yaxis = list (title = "Magnitude"))
        
      }
    }
    
    finalChart
  })
  
  output$distanceLineChart <- renderPlotly({
    
    mag <- data
    #mag$distance <- mag$Date
    test <- c()
    for(i in 1:nrow(mag)){
      #Lat +                    Long -
      temp <- sqrt((mag[i, c(8)]-41.87)^2+(mag[i, c(9)]+87.62)^2)
      if(measurementSetting() == 1){
        mag$Distance <- (temp*69)
      }
      else{
        mag$Distance <- ((temp*69)*1.60934)
      }
      test <- rbind(test, mag[i,])
      
    }
    
    tmp <- c()
    if(measurementSetting() == 1){
      tmp$one <- as.data.frame(table(test$Distance < 50))
      tmp$one <- tmp$one[-c(1),] #delete the row we dont want to use
      tmp$two <-as.data.frame(table(test$Distance >= 50 & test$Distance <=99.9999999))
      tmp$two <- tmp$two[-c(1),]
      tmp$three <- as.data.frame(table(test$Distance >= 100 & test$Distance <=149.999999))
      tmp$three <- tmp$three[-c(1),]
      tmp$four <- as.data.frame(table(test$Distance >= 150 & test$Distance <=199.9999999))
      tmp$four <- tmp$four[-c(1),]
      tmp$five <- as.data.frame(table(test$Distance >= 200 & test$Distance <= 249.999999))
      tmp$five <- tmp$five[-c(1),]
      tmp$six <- as.data.frame(table(test$Distance >= 250 & test$Distance <= 299.999999))
      tmp$six <- tmp$six[-c(1),]
      tmp$seven <- as.data.frame(table(test$Distance >=300 & test$Distance <= 349.999999))
      tmp$seven <- tmp$seven[-c(1),]
      tmp$eight <- as.data.frame(table(test$Distance >=350 & test$Distance <= 5000))
      tmp$eight <-tmp$eight[-c(1),]
      Final <- c()
      Final <- data.frame(x = c('<50', '50-99.99', '100-149.99', '150-199.99', '200-249.99', '250-299.99', '300-349.99', '>350'), y = c(tmp$one$Freq, tmp$two$Freq, tmp$three$Freq, tmp$four$Freq, tmp$five$Freq, tmp$six$Freq, tmp$seven$Freq, tmp$eight$Freq))
      colnames(Final) <- c('Distance (Miles)', 'Number of Tornadoes')
    }
    else{
      tmp$one <- as.data.frame(table(test$Distance < 75))
      tmp$one <- tmp$one[-c(1),] #delete the row we dont want to use
      tmp$two <-as.data.frame(table(test$Distance >= 75 & test$Distance <149.99))
      tmp$two <- tmp$two[-c(1),]
      tmp$three <- as.data.frame(table(test$Distance >= 150 & test$Distance <=224.999999))
      tmp$three <- tmp$three[-c(1),]
      tmp$four <- as.data.frame(table(test$Distance >=225 & test$Distance <=299.9999999))
      tmp$four <- tmp$four[-c(1),]
      tmp$five <- as.data.frame(table(test$Distance >= 300 & test$Distance <= 374.999999))
      tmp$five <- tmp$five[-c(1),]
      tmp$six <- as.data.frame(table(test$Distance >= 375 & test$Distance <= 449.999999))
      tmp$six <- tmp$six[-c(1),]
      tmp$seven <- as.data.frame(table(test$Distance >=450 & test$Distance <= 524.999999))
      tmp$seven <- tmp$seven[-c(1),]
      tmp$eight <- as.data.frame(table(test$Distance >=525 & test$Distance <= 600))
      tmp$eight <-tmp$eight[-c(1),]
      Final <- c()
      Final <- data.frame(x = c('<75', '75-149.99', '150-224.99', '225-299.99', '300-374.99', '375-449.99', '450-542.99', '>525'), y = c(tmp$one$Freq, tmp$two$Freq, tmp$three$Freq, tmp$four$Freq, tmp$five$Freq, tmp$six$Freq, tmp$seven$Freq, tmp$eight$Freq))
      colnames(Final) <- c('Distance (Kilometers)', 'Number of Tornadoes')
    }
    
    dat <- data.frame(Final) 
    if(measurementSetting() == 1){
      finalChart <-   plot_ly(dat, x = ~dat$Distance, y = ~dat$Number.of.Tornadoes, name = "Number of Tornadoes", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Distance From Chicago (Miles)", categoryorder = "array",
                            categoryarray = c('<50', '50-99.99', '100-149.99', '150-199.99', '200-249.99', '250-299.99', '300-349.99', '>350')), 
               yaxis = list (title = "Number of Tornadoes"))}
    else{
      finalChart <-   plot_ly(dat, x = ~dat$Distance, y = ~dat$Number.of.Tornadoes, name = "Number of Tornadoes", type = "scatter", mode = "lines") %>%
        layout(xaxis = list(title = "Distance From Chicago (Kilometers", categoryorder = "array",
                            categoryarray = c('<75', '75-149.99', '150-224.99', '225-299.99', '300-374.99', '375-449.99', '450-542.99', '>525')), 
               yaxis = list (title = "Number of Tornadoes"))
    }
    finalChart
  })
  
  output$CountiesLineChart <- renderPlotly({
    county <- data
    colnames(county)[14] <- "FIPS.County"
    county <- full_join(county, fips, by = 'FIPS.County')
    countySum <- count(county, County.Name)
    countySum <- countySum %>% arrange(desc(n))
    countySum <- countySum %>% top_n(14)
    dat <- data.frame(countySum)
    finalChart <-   plot_ly(dat, x = ~dat$County.Name, y = ~dat$n, name = "Number of Tornadoes", type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "County", categoryorder = "array", categoryarray = c(dat$County.Name)), yaxis = list (title = "Number of Tornadoes"))
    finalChart
  })
  
  
  # Leaflet map for all tornadoes
  # Need to add init markers
  output$map <- renderLeaflet({
    m <- leaflet::leaflet() %>% 
      # Add two tiles
      addTiles(options = providerTileOptions(noWrap = TRUE), group="Street") %>% 
      addProviderTiles("Esri.WorldImagery", group="Natural") %>%
      addProviderTiles("Esri.NatGeoWorldMap", group="NatGeo") %>%
      addProviderTiles("OpenTopoMap", group="Topography") %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite", group="Grayscale") %>%
      addLayersControl(baseGroups = c("Street", "Natural","NatGeo","Topography","Grayscale"), options = layersControlOptions(collapsed = TRUE))
      
    
    # -1 means show all tornadoes
    if (magnitudeChoice() == -1){
      map1 <- dplyr::filter(data, data$"End Lon" != 0 &
                              data$Length >= minLength() & data$Length <= maxLength() &
                              data$Width >= minWidth() & data$Width <= maxWidth() &
                              data$Injuries >= minInjury() & data$Injuries <= maxInjury() &
                              data$Fatalities >= minFatal() & data$Fatalities <= maxFatal() &
                              data$Loss >= minLoss() & data$Loss <= maxLoss())
      if(getPlayback() == TRUE){
        map1 <- dplyr:: filter(map1, format(as.POSIXct(map1$Date, format="%Y-%m-%d"),"%Y") == getYearPlay())
      }
      if(countyChoice() != -1){
        map1 <- dplyr::filter(map1, map1$F1 == countyChoice())
      }
      
      for(i in 1:nrow(map1)){
        m <- leaflet::addPolylines(m, lat = as.numeric(map1[i, c(8, 10)]), lng = as.numeric(map1[i, c(9, 11)]))
      }
      
    } else if (magnitudeChoice() == -2)  {
      
      for(i in 1:nrow(top10)){
        m <- leaflet::addPolylines(m, lat = as.numeric(top10[i, c(8, 10)]), lng = as.numeric(top10[i, c(9, 11)]))
      }
      
      
    } else {
      map1 <- dplyr::filter(data, data$"End Lon" != 0 & 
                              data$Magnitude == magnitudeChoice() &
                              data$Length >= minLength() & data$Length <= maxLength() &
                              data$Width >= minWidth() & data$Width <= maxWidth() & 
                              data$Injuries >= minInjury() & data$Injuries <= maxInjury() &
                              data$Fatalities >= minFatal() & data$Fatalities <= maxFatal() &
                              data$Loss >= minLoss() & data$Loss <= maxLoss())
      if(getPlayback() == TRUE){
        map1 <- dplyr:: filter(map1, format(as.POSIXct(map1$Date, format="%Y-%m-%d"),"%Y") == getYearPlay())
      }
      if(countyChoice() != -1){
        map1 <- dplyr::filter(map1, map1$F1 == countyChoice())
      }

      for(i in 1:nrow(map1)){
        m <- leaflet::addPolylines(m, lat = as.numeric(map1[i, c(8, 10)]),
                                   lng = as.numeric(map1[i, c(9, 11)]))
      }
    }
    m
  })
}

shinyApp(ui, server)