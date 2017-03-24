##### Shiny App to Explore NYC Modal Trip Making
##### Date Modified: 2/10/17

library(shiny)
library(plotly)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Trip Statistics by Mode for New York City"),

  sidebarLayout(position = "left", 
                sidebarPanel(helpText("Choose the Dates, Modes, and Aggregations You Would Like to View"), 
                             # inputPanel(
                             #   uiOutput("taxi_zones")
                             # ),
                             #leafletOutput("map", width = 400, height = 400),
                             inputPanel(
                               uiOutput("date_range")
                             ), 
                             #inputPanel(
                            #   uiOutput("hour_range")
                             #),   
                             inputPanel(
                               uiOutput("modes")
                             ), 
                             radioButtons("time_agg", "Choose a Temporal Aggregation Level:", 
                                          c("Daily", "Weekly", "Hourly"), selected = "Daily"),
                             radioButtons("prov_agg", "Group By:", 
                                          c("Origin-Dest", "Provider"), selected = "Provider"),
                             inputPanel(
                              actionButton("eval_req", "Click to Run Query")
                             )
                             #   # download data button
                             #   downloadButton('downloadData', 'Download Current Table View')
                ),
                
                mainPanel(
                  h1("Trips by Mode"),
                  h3("Select Zone Using Map"),
                  leafletOutput("map", width = 700, height = 400),
                  plotlyOutput("plot1",  width = "100%", height = 700),
                  #plotOutput("plot2",  width = "100%", height = 700),
                  downloadButton('downloadData', 'Download'),
                  dataTableOutput("table1")
                ))
  
  
))