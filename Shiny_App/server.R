##### Packages
library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(rgdal)
library(jsonlite)

# http://deanattali.com/blog/building-shiny-apps-tutorial/

#### Connect to DB, add check to make sure it exists
db <- src_sqlite("C:/Users/Jwhit/Dropbox/Datasets/NYC_Travel_Data/Data/db/congestion.sqlite", create = F)
# get taxi zones for ui
taxi_zones <- tbl(db, "taxi_zones") %>% collect()
# get date range for ui, save these as variables in an rda file so db doesn't have to be queried
#dates <- select(tbl(db, "fact_table"), datekey) %>% collect(n = Inf)
# hard code for now
min_date <- ymd("2016-01-01")
max_date <- ymd("2016-10-04")
#rm(dates)
# get modes in db
modes <- tbl(db, "mode_dim") %>% collect()

#tbls for queries

# get shapefile for taxi zones, why aren't relative paths working right now
zone_polys <- readOGR(dsn = "C:/Users/Jwhit/Dropbox/Datasets/NYC_Travel_Data/Data/taxi_zones", layer = "taxi_zones_wgs")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # create dynamic ui drop down selectors for routes
  output$taxi_zones <- renderUI({
    selectInput("taxi_zones", label = h3("TLC Zones"), 
                choices = taxi_zones$Zone, selected = "Union Sq")
  })
  # dynamic date range slection, returns character vector of ymd strings 
  output$date_range <- renderUI({
    dateRangeInput("date_range", label = h3("Date Range"),
                   start = min_date, end = max_date, 
                   min = min_date, max = max_date)
  })
  # dynamic mode slection, returns character vector of ticked boxes
  output$modes <- renderUI({
    checkboxGroupInput("modes", label = h3("Mode Choice"),
                       choices = c("All", modes$mode_text), 
                       selected = "All")
  })
  # dynamic hour slection, returns character vector of ticked boxes
  output$hour_range <- renderUI({
    sliderInput("hour_range", label = h3("Hour Range"),
                value = c(0,23), step = 1,
                min = 0, max = 23)
  })
  
  #####variable and table creation#####
  
  # reactive fx for sql query, only evaluate inputs when action button is clicked
  db_query <- eventReactive(input$eval_req, {
    # get inputs for db query
    #zone_choice <- input$taxi_zones
    zone_choice <- input$map_shape_click$id
    mode_choice <- input$modes
    min_date_choice <- format(input$date_range[1])
    max_date_choice <- format(input$date_range[2])
    hour_choice <- input$hour_range
    
    if("All" %in% mode_choice){
      # select zone
      zone_tbl <- tbl(db, "taxi_zones") %>% filter_(~location_id == zone_choice)
      # select date range
      date_tbl <- tbl(db, "date_dim") %>% filter_(~fulldate >= min_date_choice, 
                                                 ~fulldate <= max_date_choice)
      # select hours
      # hour_tbl <- tbl(db, "hour_dim") %>% filter_(~hour >= hour_choice[1], 
      #                                            ~hour <= hour_choice[2])
      fact_table <- tbl(db, "fact_table")# %>% filter_( ~hour_id >= hour_choice[1], 
                                               #        ~hour_id <= hour_choice[2])
      # all modes
      mode_tbl <- tbl(db, "mode_dim")
      
      # build query
      query <- inner_join(fact_table, zone_tbl) %>%
        inner_join(date_tbl) %>% 
        inner_join(mode_tbl) %>%
        inner_join(tbl(db, "od_dim"))
    }else{
      # select zone
      zone_tbl <- tbl(db, "taxi_zones") %>% filter_(~location_id == zone_choice)
      # select date range
      date_tbl <- tbl(db, "date_dim") %>% filter_(~fulldate >= min_date_choice, 
                                                  ~fulldate <= max_date_choice)
      # select hours
      hour_tbl <- tbl(db, "hour_dim") %>% filter_(~hour >= hour_choice[1], 
                                                  ~hour <= hour_choice[2])
      
      # select modes, if only one mode is selected can't use %in%
      if(length(mode_choice) > 1){
        mode_tbl <- tbl(db, "mode_dim") %>% filter_(~mode_text %in% mode_choice)
      }else{
        mode_tbl <- tbl(db, "mode_dim") %>% filter_(~mode_text == mode_choice)
      }
      
      # build query
      query <- inner_join(tbl(db, "fact_table"), zone_tbl) %>%
        inner_join(date_tbl) %>% 
        inner_join(mode_tbl) %>%
        inner_join(tbl(db, "od_dim"))

    }
   # cat(explain(query))
    # collect results
    results <- query %>% collect(n = Inf)
    
    return(results)
    

  }) # end reactive function
  observe({print(input$modes)})

  ##### Outputs #####
  # create histogram
  output$plot1 <- renderPlot({
    # get df
    trips <- db_query()

    # plot
    p <- ggplot(data = filter(trips),
                aes(x = ymd_h(paste(fulldate, hour_id)), y = trips, color = od_type)) +
      geom_line() +
      facet_wrap("mode_text")
    print(p)

  })

  # Table
  output$table1 <- renderDataTable({
    # table for dynamic view
    db_query()

  })
  
  # selection map
  # render base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = zone_polys, weight = 1, fillOpacity = 0.4, smoothFactor = 0.5, 
                  popup = ~zone, layerId = zone_polys@data$LocationID)
  })
 # observe({print(filter(taxi_zones, location_id == input$map_shape_click$id))})
  
  # 
  # # table for download
  # output$downloadData <- downloadHandler(
  #   filename = "Trains_Per_Hour.csv",
  #   content = function(file){
  #     write.csv(table_list()$hourly, file)
  #   }
  # )
  
})