##### Packages
library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(rgdal)
library(plotly)
library(RcppRoll)
library(forecast)
library(broom)
library(RSQLite)


#### Function for Labelling TNC's in ggplot, update to new labeller API
ggplot_labeller <- function(variable, value){
  return(mode_labels[value])
}
# 
mode_labels <- c("Taxi" = "Taxi", "Bike Share" = "Bike Share", "TNC" ="Transportation Network Companies")

#### Connect to DB, add check to make sure it exists
db <- src_sqlite("./db/congestion_large.sqlite", create = F)
# get taxi zones for ui
taxi_zones <- tbl(db, "taxi_zones") %>% collect()
# set columns to drop for different group by scenarios
hourly_prov_drop <- c("trips", "od_id", "od_type", "fact_id", "person_miles_travelled", 
                      "avg_trip_distance", "passengers")
daily_prov_drop <- c("trips", "od_id", "od_type", "fact_id", "hour_id", "hour_am_pm", "peak_off_peak",
                     "person_miles_travelled", "hour", "am_pm", "avg_trip_distance", "passengers")
weekly_prov_drop <- c("trips", "od_id", "od_type", "fact_id", "hour_id", "hour_am_pm",
                      "peak_off_peak", "person_miles_travelled","hour", "am_pm", 
                      "avg_trip_distance", "passengers", "fulldate", "datekey", "holiday.indicator")

hourly_od_drop <- c("trips", "provider_id", "provider_type", "fact_id", "person_miles_travelled", 
                    "avg_trip_distance", "passengers")
daily_od_drop <- c("trips", "provider_id", "provider_type", "fact_id", "hour_id", "hour_am_pm",
                   "peak_off_peak", "person_miles_travelled", "hour", "am_pm",
                   "avg_trip_distance", "passengers")
weekly_od_drop <- c("trips", "provider_id", "provider_type", "fact_id", "hour_id", "hour_am_pm",
                   "peak_off_peak", "person_miles_travelled", "hour", "am_pm",
                   "avg_trip_distance", "passengers", "fulldate", "datekey", "holiday.indicator")

#tbls for queries
# get shapefile for taxi zones and subways
zone_polys <- readOGR(dsn = "./taxi_zones", layer = "taxi_zones_wgs")
subway_routes <- readOGR(dsn = "./subway/routes_nyc_subway_jan2017",
                         layer = "routes_nyc_subway_jan2017_wgs")
subway_routes$color <- paste0("#", subway_routes$color)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #####variable and table creation#####
    # reactive fx for sql query, only evaluate inputs when action button is clicked
  db_query <- eventReactive(input$eval_req, {
    # get inputs for db query
    # set a default zone if not clicked, union sq
    zone_choice <- ifelse(is.null(input$map_shape_click$id), 234, input$map_shape_click$id)
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
      hour_tbl <- tbl(db, "hour_dim") #%>% filter_(~hour >= hour_choice[1], 
                                       #           ~hour <= hour_choice[2])
      fact_table <- tbl(db, "fact_table")# %>% filter_( ~hour_id >= hour_choice[1], 
                                               #        ~hour_id <= hour_choice[2])
      # all modes
      mode_tbl <- tbl(db, "mode_dim")
      
      # provider operator table
      provider_tbl <- tbl(db, "service_providers")
      
      # build query
      query <- inner_join(fact_table, zone_tbl) %>%
        inner_join(date_tbl) %>% 
        inner_join(mode_tbl) %>%
        inner_join(tbl(db, "od_dim")) %>%
        left_join(provider_tbl) %>%
        inner_join(hour_tbl)
    }else{
      # select zone
      zone_tbl <- tbl(db, "taxi_zones") %>% filter_(~location_id == zone_choice)
      # select date range
      date_tbl <- tbl(db, "date_dim") %>% filter_(~fulldate >= min_date_choice, 
                                                  ~fulldate <= max_date_choice)
      # select hours
      hour_tbl <- tbl(db, "hour_dim") #%>% filter_(~hour >= hour_choice[1], 
                                       #           ~hour <= hour_choice[2])
      
      # select modes, if only one mode is selected can't use %in%
      if(length(mode_choice) > 1){
        mode_tbl <- tbl(db, "mode_dim") %>% filter_(~mode_text %in% mode_choice)
      }else{
        mode_tbl <- tbl(db, "mode_dim") %>% filter_(~mode_text == mode_choice)
      }
      # provider operator table
      provider_tbl <- tbl(db, "service_providers")
      
      # build query
      query <- inner_join(tbl(db, "fact_table"), zone_tbl) %>%
        inner_join(date_tbl) %>% 
        inner_join(mode_tbl) %>%
        inner_join(tbl(db, "od_dim"))%>%
        left_join(provider_tbl)%>%
        inner_join(hour_tbl)

    }
    # cat(explain(query))
    # collect results, drop unnecessary date columns, calculate week
    results <- query %>% 
      collect(n = Inf) %>%
      mutate(fulldate = lubridate::ymd(fulldate), fulldate = force_tz(fulldate, tzone = "America/New_York"),
             week_num = (interval(min(fulldate), fulldate) %/% weeks(1)) + 1)
    
    # summarize according to user selection
    cols_to_drop <- switch(paste(input$time_agg, input$prov_agg, sep = "-"), 
                           "Hourly-Origin-Dest" = hourly_od_drop, 
                           "Daily-Origin-Dest" = daily_od_drop,
                           "Weekly-Origin-Dest" = weekly_od_drop,
                           "Hourly-Provider" = hourly_prov_drop, 
                           "Daily-Provider" = daily_prov_drop, 
                           "Weekly-Provider" = weekly_prov_drop)
    
    results <- group_by_(results, .dots = setdiff(colnames(results), cols_to_drop)) %>%
      summarise(trips = sum(trips)) 
    
    # apply rolling average if selected
    if(input$smoothing == "MA"){
      # arrange at proper aggregation
      if(input$time_agg == "Hourly"){
        results <- arrange(results, paste(fulldate, hour_id))
      }else if(input$time_agg == "Daily"){
        results <- arrange(results, fulldate)
      } else if(input$time_agg == "Weekly"){
        results <- arrange(results, week_num)
      }
      
      #re-group at proper level
      if(input$prov_agg == "Provider"){
        results <- group_by(results, provider_type)
      }
      else if(input$prov_agg == "Origin-Dest"){
        results <- group_by(results, mode_text, od_type)
      }
      # calculate mean
      results <- mutate(results, trips = roll_mean(trips, align = "right", n = input$ma_n, fill = NA))
    }
    
    #### Plots ####
    # color var for ggplot
    color_var <- if_else(input$prov_agg == "Provider", "provider_type", "od_type")
    
    # correctly format plot based on time aggregation
    if(input$time_agg == "Hourly"){
      p <- ggplot(data = results,
                  aes(x = ymd_h(paste(fulldate, hour_id)), y = trips)) +
        geom_line(aes_string(color = color_var),  size = .2) +
        labs(x = "Date and Hour", y = "Trips")
    }else if(input$time_agg == "Daily"){
      p <- ggplot(data = results,
                  aes(x = fulldate, y = trips)) +
        #geom_bar(stat = "identity", aes_string(fill = color_var), color = "gray") +
        geom_line(aes_string(color = color_var),  size = .2) +
        labs(x = "Date", y = "Trips")
    }else if(input$time_agg == "Weekly"){
      p <- ggplot(data = results,
                  aes(x = week_num, y = trips)) +
        #geom_bar(stat = "identity", aes_string(fill = color_var), color = "gray") +
        geom_line(aes_string(color = color_var),  size = .2) +
        labs(x = "Week", y = "Trips") + scale_x_continuous(breaks = unique(results$week_num))
    }
    # facet charts if od gropuing selecting
    if(input$prov_agg == "Origin-Dest"){
      p <- p + facet_wrap("mode_text", nrow = 2, labeller = ggplot_labeller)
    }
    
    # pick correct category labels and colors
    od_cols <- c("Destination" = "Red", "Origin" = "Blue")
    provider_cols <- c("Uber" = "#09091a", "Lyft" = "#E70B81", "Via" = "#B0E2FF", "Yellow Taxi" = "#f7b731",
                       "Green Taxi" = "#93DB70", "Citi Bike" = "#2E4DA7", "Other TNC/Black Car" = "#778899")
    if(input$prov_agg == "Provider"){
      p <- p + scale_color_manual(name = "Service Provider", values = provider_cols)
    } else if(input$prov_agg == "Origin-Dest"){
      p <- p + scale_color_manual(name = "Origin/Dest Trips", values = od_cols)
    }
    
    # apply trend line if selected
    if(input$trend == "LOESS"){
      p <- p + geom_smooth(method = "loess", aes_string(color = color_var))
    }
    
    # simple forecast, using exponential smoothing b/c I'm more familiar with it and it doesn't assume stationarity 
    # which I haven't checked for and this is for fun. use forecast's ets function to pick the best exp. smoothing
    # method via AIC
    # 
    # if(input$forecast == "exp"){
    #   # fit an exp. smoothing model by grouping
    #   forecast <- group_by(results, provider_type) %>%
    #     do(forecasts = forecast(ets(.$trips)))
    #   
    #   cat(forecast[forecast$provider_type == "Lyft",][["forecasts"]]$mean)
    # 
    # }
    # 
    return(list(results = results, p = p))
    

  }) # end reactive function
  observe({print(input$modes)})
  observe({print(input$time_agg)})

  ##### Outputs #####
  # output plot
  output$plot1 <- renderPlotly({
    print(ggplotly(db_query()$p))

  })

  # Table
  output$table1 <- renderDataTable({
    trips <- db_query()$results

    return(trips)

  })
  
  # selection and base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 40.730610, lng = -73.935242, zoom = 10) %>%
      addPolylines(data = subway_routes, color = subway_routes$color, smoothFactor = 1, 
                   opacity = 1, weight = 1) %>%
      addPolygons(data = zone_polys, weight = 1, fillOpacity = 0, smoothFactor = 1, color = "grey", 
                  popup = ~zone, layerId = zone_polys@data$LocationID)
  })
  # change color of selected polygon
  observe({
    # set a default zone if not clicked, union sq
    zone_choice <- ifelse(is.null(input$map_shape_click$id), 234, input$map_shape_click$id)
    
    proxy <- leafletProxy("map", data = zone_polys) %>%
      removeShape(layerId = "clicked_poly") %>%
      addPolygons(data = subset(zone_polys, LocationID == zone_choice), 
                  color = "red", weight = 1, fillOpacity = 0.4, smoothFactor = 0.5, layerId = "clicked_poly")
  })

  # table for download
  output$downloadData <- downloadHandler(
    filename = "Results.csv",
    content = function(file){
      write.csv(db_query()$results, file)
    }
  )
  
})