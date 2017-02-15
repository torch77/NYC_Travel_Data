##### Shiny App to Explore NYC Modal Trip Making
##### Date Modified: 2/10/17

library(shiny)
library(plotly)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Trip Statistics by Mode for New York City"),

  sidebarLayout(position = "left", 
                sidebarPanel(helpText("Chose the Dates, Hours, and Modes You Would Like to View"), 
                             # inputPanel(
                             #   uiOutput("taxi_zones")
                             # ),
                             #leafletOutput("map", width = 400, height = 400),
                             inputPanel(
                               uiOutput("date_range")
                             ), 
                             inputPanel(
                               uiOutput("hour_range")
                             ),   
                             inputPanel(
                               uiOutput("modes")
                             ), 
                             inputPanel(
                              actionButton("eval_req", "Click to Run Query")
                             )
                             #   # download data button
                             #   downloadButton('downloadData', 'Download Current Table View')
                ),
                
                mainPanel(
                  h1("Trips by Mode"),
                  h3("Select Zone Using Map"),
                  leafletOutput("map", width = 500, height = 250),
                  plotOutput("plot1",  width = "100%", height = 700),
                  #plotOutput("plot2",  width = "100%", height = 700),
                  dataTableOutput("table1")
                ))
  
  
))