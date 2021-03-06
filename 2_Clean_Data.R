### Script to Clean Data for Congestion Analysis
### Save data for each mode in a seperate .rda file so data doesn't need to be reloaded
### 

##### Packages #####
req_packages <- c("tidyverse", "lubridate", "sp", "spatialEco")

if(!all(lapply(req_packages, require, character.only = T))){
  print("Attempting to install the required packages")
  install.packages(req_packages)
  if(!all(lapply(req_packages, require, character.only = T))){
    print("packages installed")
  }else{
    print("packages did not install. clean data script will not run")
  }
}

##### Functions #####
save_dfs <- function(dfs, location){
  save(list = dfs, file = location)
  rm(list = dfs, pos = ".GlobalEnv")
  gc()
}
spatial_convert <- function(o_df, d_df, o_coords, d_coords){
  origins <- SpatialPointsDataFrame(o_coords, o_df, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  dests <- SpatialPointsDataFrame(d_coords, d_df,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
  trip_list <- list(origins, dests)
  names(trip_list) <- c("origins", "dests")
  
  return(trip_list)
  
}
# function to handle data that is too much to process in one df
list_clean_up <- function(list, zones){
  final_list <- list()
  for(i in 1:length(list)){
    print(paste("handling item", i))
    # drop unused columns
    list[[i]] <- select(list[[i]], one_of("tpep_pickup_datetime", "tpep_dropoff_datetime", 
                                                        "pickup_longitude", "pickup_latitude", "dropoff_longitude",
                                                        "dropoff_latitude","passenger_count", "trip_distance")) %>%
      mutate(tpep_pickup_datetime = ymd_h(sub(":.*$", "", tpep_pickup_datetime)), tpep_dropoff_datetime = 
               ymd_h(sub(":.*$", "", tpep_dropoff_datetime)))
    # lower case all columns
    colnames(list[[i]]) <- tolower(colnames(list[[i]]))
    
    # create unique ID to match origins and destinations
    if(i == 1){
      list[[i]]$p_id <- seq(1, nrow(list[[i]]), by = 1)
      print(range(list[[i]]$p_id))
    }else{
      list[[i]]$p_id <- seq(max(list[[i-1]]$p_id)+1, 
                                        nrow(list[[i]])+max(list[[i-1]]$p_id), by = 1)
      print(range(list[[i]]$p_id))
      # null out list entry to save memory
      list[[i-1]] <-NA
      gc()
    }
    
    # convert to spatial points data frame for intersection with tlc zones
    temp <- spatial_convert(select(list[[i]], -one_of(c("pickup_longitude", "pickup_latitude")),
                                               -contains("dropoff")),
                                        select(list[[i]], -one_of(c("dropoff_longitude", "dropoff_latitude")),
                                               -contains("pickup")),
                                        select(list[[i]], one_of(c("pickup_longitude", "pickup_latitude"))),
                                        select(list[[i]], one_of(c("dropoff_longitude", "dropoff_latitude"))))
    print('points in poly')
    temp$origins <- point.in.poly(temp$origins, zones)
    temp$dests <- point.in.poly(temp$dests, zones) 
    gc()
    
    final_list[[i]] <- temp
  }
  return(final_list)
}
# function to handle subway outliers
tsoutliers <- function(x,plot=FALSE){
  # cribbed from http://stats.stackexchange.com/questions/1142/simple-algorithm-for-online-outlier-detection-of-a-generic-time-series
  x <- as.ts(x)
  if(frequency(x)>1){
    resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  }else{
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }
  resid.q <- quantile(resid,prob=c(0.25,0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
  if(plot){
    plot(x)
    x2 <- ts(rep(NA,length(x)))
    x2[score>0] <- x[score>0]
    tsp(x2) <- tsp(x)
    points(x2,pch=19,col="red")
    return(invisible(score))
  }else{
    return(as.numeric(score))
  }
}
# function to segregate o/ds and add zone info for data that doesn't have lat/lons
zonal_data_clean <- function(x, taxi_type, zones){
  # origins
  if(taxi_type == "green"){
  origins <- select_(x, "lpep_pickup_datetime", "pulocationid", "passenger_count", "trip_distance", "p_id") %>%
    rename_("LocationID" = "pulocationid") %>%
    inner_join(zones@data)
  
  # dests
  dests <- select_(x, "lpep_dropoff_datetime", "dolocationid", "passenger_count", "trip_distance", "p_id") %>%
    rename_("LocationID" = "dolocationid") %>%
    inner_join(zones@data)
  }else if(taxi_type =="yellow"){
    origins <- select_(x, "tpep_pickup_datetime", "pulocationid", "passenger_count", "trip_distance", "p_id") %>%
      rename_("LocationID" = "pulocationid") %>%
      inner_join(zones@data)
    
    # dests
    dests <- select_(x, "tpep_dropoff_datetime", "dolocationid", "passenger_count", "trip_distance", "p_id") %>%
      rename_("LocationID" = "dolocationid") %>%
      inner_join(zones@data)
  }
  
  trip_list <- list(origins, dests)
  names(trip_list) <- c("origins", "dests")
  
  return(trip_list)
  
}

##### Load/Clean Data #####
### TLC zones ###
load("./Data/tlc_zones.rda")
zones <- zones[c(1,4,5,6)]

### TNC ###
load("./Data/TNC_data_frames.rda")

# drop out NA's for now to shrink table size
tnc_data <- filter(tnc_data, complete.cases(tnc_data))

# merge taxi zones to data, not needed b/c of later aggregation
# tnc_data <- inner_join(tnc_data, taxi_zone_lookup, by = c("locationID" = "LocationID"))

# convert pickup date column to date type
# ymd_hms with tz is taking forever so not doing for now, leave as UTC
# truncating date hour for aggregation later, delete rest of time string after first hour
# tnc_data <- mutate(tnc_data, Pickup_date = ymd_hms(Pickup_date, tz = "America/New_York"))
tnc_data <- mutate(tnc_data, pickup_date_hour = ymd_h(sub(":.*$", "", Pickup_date)))

# save/clear data
save_dfs(c("tnc_data", "taxi_zone_lookup"), "./Data/Clean_TNC_data_frames.rda")

#### Bike ###
# load data
load("./Data/Bike_data_frames.rda")

# drop birth.year column since it's the only one with NA's, drop other personal data since not used
# need to handle change in date format starting oct-2016. format went from m/d/y to y-m-d, lets hope hold
# throws error b/c if_else evalates both true/false functions and then assigns correct value based on 
# result of condition so ok to ignore. doesn't seem the most efficient 
bike_data <- select(bike_data, -one_of(c("birth.year", "gender", "usertype", "bikeid"))) %>%
  mutate(starttime = if_else(grepl("/", starttime), mdy_h(sub(":.*$", "", starttime)), ymd_h(sub(":.*$", "", starttime))),
         stoptime = if_else(grepl("/", stoptime), mdy_h(sub(":.*$", "", stoptime)), ymd_h(sub(":.*$", "", stoptime))))

# create unique ID to match origins and destinations
bike_data$p_id <- seq(1, nrow(bike_data), by = 1)

# convert to spatial points data frame for intersection with tlc zones
bike_list <- spatial_convert(select(bike_data, -one_of(c("start.station.longitude", "start.station.latitude")), -contains("end.")),
                             select(bike_data, -one_of(c("end.station.longitude", "end.station.latitude")), -contains("start.")),
                             select(bike_data, one_of(c("start.station.longitude", "start.station.latitude"))),
                             select(bike_data, one_of(c("end.station.longitude", "end.station.latitude"))))

# match points to TLC Zones, this will drop out trip ends outside of TLC zones
bike_list$origins <- point.in.poly(bike_list$origins, zones)
bike_list$dests <- point.in.poly(bike_list$dests, zones) 

# examine stations, duplicated end and start ids are the same
end_stations <- distinct(bike_data, end.station.id, end.station.name)
# start_stations <- distinct(bike_data, start.station.id, start.station.name)
duplicate_end_stations <- filter(end_stations, duplicated(end.station.id))
# duplicate_start_stations <- filter(start_stations, duplicated(start.station.id))
duplicated_station_names <- arrange(filter(end_stations, end.station.id %in% duplicate_end_stations$end.station.id),
                                    end.station.id)

# get master station list, it appears all start stations are contained in the end station list which is larger
# filter(start_stations, !(start.station.id %in% end_stations$end.station.id), !(start.station.name %in% end_stations$end.station.name))
# need to drop out stations with different longitudes and latitudes
station_list <- inner_join(end_stations, distinct(bike_data, end.station.id, end.station.latitude, end.station.longitude))
rm("end_stations", "duplicate_end_stations", "bike_data")
save_dfs(c("bike_list", "duplicated_station_names", "station_list"), "./Data/Clean_Bike_data_frames.rda")

#### Yellow Cab ###
### load and clean lat lon data ###
load("./Data/Yellow_data_frames_2015.rda")
# names for 2015
names(yellow_data_2015) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
gc()
yellow_list_2015 <- list_clean_up(yellow_data_2015, zones)

save_dfs(c("yellow_list_2015"), "./Data/Clean_Yellow_data_frames_2015.rda")

load("./Data/Yellow_data_frames_2016.rda")
# names for 2016
names(yellow_data_2016) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
gc()
yellow_list_2016 <- list_clean_up(yellow_data_2016, zones)
save_dfs(c("yellow_list_2016"), "./Data/Clean_Yellow_data_frames_2016_lat_lon.rda")

### load non-lat lon data and combine with new data ###
load("./Data/Yellow_data_frames_2016_no_lat_lon.rda")
load("./Data/Clean_Yellow_data_frames_2016_lat_lon.rda")

# primary id for data with no lat lon
yellow_data_2016_no_lat_lon$p_id <- seq((max(yellow_list_2016[[length(yellow_list_2016)]]$origins@data$p_id)+1),
                                  (nrow(yellow_data_2016_no_lat_lon)+
                                     max(yellow_list_2016[[length(yellow_list_2016)]]$origins@data$p_id)), by = 1) 
yellow_data_2016_no_lat_lon <- mutate(yellow_data_2016_no_lat_lon, 
                                      tpep_pickup_datetime = ymd_h(sub(":.*$", "", tpep_pickup_datetime)), 
                                      tpep_dropoff_datetime = ymd_h(sub(":.*$", "", tpep_dropoff_datetime)))

# get o/d's for new data w/o lats/lons
temp_list <- zonal_data_clean(yellow_data_2016_no_lat_lon, "yellow", zones)

# add to list 
yellow_list_2016[[length(yellow_list_2016)+1]] <- temp_list
rm("temp_list", "yellow_data_2016_no_lat_lon")
save_dfs(c("yellow_list_2016"), "./Data/Clean_Yellow_data_frames_2016.rda")

### Green Cab ###
# load data
load("./Data/Green_data_frames.rda")

# drop unused columns
green_data <- select(green_data, one_of("lpep_pickup_datetime", "lpep_dropoff_datetime", 
                                                  "pickup_longitude", "pickup_latitude", "dropoff_longitude",
                                                  "dropoff_latitude","passenger_count", "trip_distance")) %>%
  mutate(lpep_pickup_datetime = ymd_h(sub(":.*$", "", lpep_pickup_datetime)), lpep_dropoff_datetime = 
           ymd_h(sub(":.*$", "", lpep_dropoff_datetime)))

green_data_no_lat_lon <- select(green_data_no_lat_lon, one_of("lpep_pickup_datetime", "lpep_dropoff_datetime", 
                                        "pulocationid", "dolocationid", "passenger_count", "trip_distance")) %>%
  mutate(lpep_pickup_datetime = ymd_h(sub(":.*$", "", lpep_pickup_datetime)), lpep_dropoff_datetime = 
           ymd_h(sub(":.*$", "", lpep_dropoff_datetime)))
# lower case all columns
colnames(green_data) <- tolower(colnames(green_data))
colnames(green_data_no_lat_lon) <- tolower(colnames(green_data_no_lat_lon))

# create unique ID to match origins and destinations
green_data$p_id <- seq(1, nrow(green_data), by = 1)
green_data_no_lat_lon$p_id <- seq((max(green_data$p_id)+1),
                                  (nrow(green_data_no_lat_lon)+
                                     max(green_data$p_id)), by = 1)

# convert to spatial points data frame for intersection with tlc zones
green_list <- spatial_convert(select(green_data, -one_of(c("pickup_longitude", "pickup_latitude")), -contains("dropoff")),
                             select(green_data, -one_of(c("dropoff_longitude", "dropoff_latitude")), -contains("pickup")),
                             select(green_data, one_of(c("pickup_longitude", "pickup_latitude"))),
                             select(green_data, one_of(c("dropoff_longitude", "dropoff_latitude"))))

# match points to TLC Zones, this will drop out trip ends outside of TLC zones
green_list$origins <- point.in.poly(green_list$origins, zones)
green_list$dests <- point.in.poly(green_list$dests, zones) 

# get o/d's for new data w/o lats/lons
temp_list <- zonal_data_clean(green_data_no_lat_lon, "green", zones)

# bind data together
green_list$origins <- bind_rows(green_list$origins@data, temp_list$origins)
green_list$dests <- bind_rows(green_list$dests@data, temp_list$dests)

rm("green_data", "temp_list", "green_data_no_lat_lon")
save_dfs(c("green_list"), "./Data/Clean_Green_data_frames.rda")

### Turnstile ###
# load data
load("./Data/Turnstile_data_frames.rda")


unique(paste(turnstile_agg$station, turnstile_agg$linename, sep = "-"))

# correct misnames stations and lines and then group


turnstile_agg <- mutate(turnstile_data, datetime = mdy_hms(paste(date, time), tz = "America/New_York"),
                            entries = abs(entries), exits = abs(exits), linename = as.character(linename)) %>%
  group_by(station, linename, division, datetime) %>%
  # need to be numeric b/c integers are too large
  summarise(entries = sum(as.numeric(entries)), exits = sum(as.numeric(exits))) %>%
  # drop entries that seem to be in error
  filter(minute(datetime) == 0)

# Calculate Entries/Exits based on difference between cumulative values
turnstile_agg <- group_by(turnstile_agg, station) %>%
  arrange(datetime) %>%
  mutate(entry_val = c(NA,diff(entries)), exit_val = c(NA,diff(exits))) %>%
  filter(!is.na(entry_val)) 
# check for outliers
turnstile_agg <- arrange(turnstile_agg, datetime) %>%
  group_by(station) %>%
  mutate(weekday = wday(datetime, label = T, abbr = T), entry_score = tsoutliers(entry_val), exit_score = tsoutliers(exit_val),
         entry_val2 = if_else((entry_score > 5 | entry_val < 0), as.numeric(NA), as.numeric(entry_val)),
         entry_val2 = na.interp(entry_val2), 
         exit_val2 = if_else((exit_score > 5 | exit_val < 0), as.numeric(NA), as.numeric(exit_val)),
         exit_val2 = na.interp(exit_val2)) %>% 
  filter(datetime > "2017-01-01")

# create date time
turnstile_data <- mutate(turnstile_data, date_time = mdy_h(paste(DATE, sub(":.*$", "", TIME))))

save_dfs(c("turnstile_data"), "./Data/Clean_Turnstile_data_frames.rda")
