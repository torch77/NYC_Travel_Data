##### Shiny App to Explore NYC Modal Trip Making
##### Date Modified: 2/10/17

library(shiny)
library(plotly)
library(leaflet)
library(lubridate)
# dates
min_date <- ymd("2015-01-01")
max_date <- ymd("2016-12-31")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Trip Statistics by Mode for New York City"),

  sidebarLayout(position = "left", 
                sidebarPanel(h3("Data Filters"),
                             h4("Choose the Dates, Modes, Zone, and Aggregations You Would Like to View."), 
                             h5("Click Run Query to Execute."),
                             h5(strong("Select Zones via Interactive Map:")),
                             leafletOutput("map", height = 200),
                             dateRangeInput("date_range", label = "Date Range:",
                                            start = min_date, end = max_date, 
                                            min = min_date, max = max_date),
                             checkboxGroupInput("modes", label = "Mode Choice:",
                                                choices = c("Taxi" = "Taxi", 
                                                            "Bike Share" = "Bike Share",
                                                            "Transportation Network Company" = "TNC"), 
                                                selected = c("Taxi", "Bike Share", "TNC")),
                             radioButtons("time_agg", "Choose a Temporal Aggregation Level:", 
                                          c("Daily", "Hourly"), selected = "Daily"),
                             radioButtons("prov_agg", "Group By:", 
                                          c("Origin-Destination"="Origin-Dest", 
                                            "Provider"="Provider"), selected = "Provider"),
                             radioButtons("smoothing", "Smoothing Method:", 
                                          c("None (Raw Data)" = "None", "Moving Average (Trailing)" = "MA"), selected = "MA"),
                             # allow user to adjust n for moving average
                             conditionalPanel(
                               condition = "input.smoothing == 'MA'",
                               sliderInput("ma_n", "Choose Number of Trailing Time Periods for the Moving Avg.:", min = 2, max = 100, 
                                           value = 10, step = 1)
                             ),
                             radioButtons("trend", "Trend Line:", 
                                          c("None"="None", "LOESS Regression" = "LOESS"), selected = "None"),
                             # radioButtons("forecast", "Forecast Method:", 
                             #              c("None" = "None", "Exp. Smoothing (select best exp. smoothing model via AIC)" = "exp"),
                             #              selected = "None"),
                             inputPanel(
                              actionButton("eval_req", "Click to Run Query")
                             ), wellPanel(
                             helpText("--Jackson Whitmore"),
                             helpText("Data Courtesy of NYC T&LC and Citi Bike."),
                             helpText(a("Source Code",href= "https://github.com/torch77/NYC_Travel_Data", 
                                        target = "blank"))
                             )
                ),
                
                mainPanel(
                  h3("Results"),
                  #leafletOutput("map", width = 700, height = 400),
                  plotlyOutput("plot1",  width = "100%", height = 700),
                  #plotOutput("plot2",  width = "100%", height = 700),
                  h3("Tabular Data Used to Create Charts"),
                  h4("(Click Download to Download Data From Current View)"),
                  downloadButton('downloadData', 'Download'),
                  dataTableOutput("table1")
                ))
  
  
))