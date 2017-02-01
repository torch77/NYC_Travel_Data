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
bike_data <- select(bike_data, -one_of(c("birth.year", "gender", "usertype", "bikeid"))) %>%
  mutate(starttime = mdy_h(sub(":.*$", "", starttime)), stoptime = mdy_h(sub(":.*$", "", stoptime)))

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
# load data
load("./Data/Yellow_data_frames_2016.rda")
# names for 2016
names(yellow_data_2016) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")

# drop unused columns
# yellow_data_2016 <- select(yellow_data_2016, one_of("tpep_pickup_datetime", "tpep_dropoff_datetime", 
#                                                   "pickup_longitude", "pickup_latitude", "dropoff_longitude",
#                                                   "dropoff_latitude","passenger_count", "trip_distance")) %>%
#   mutate(tpep_pickup_datetime = ymd_h(sub(":.*$", "", tpep_pickup_datetime)), tpep_dropoff_datetime = 
#            ymd_h(sub(":.*$", "", tpep_dropoff_datetime)))
# # lower case all columns
# colnames(yellow_data_2016) <- tolower(colnames(yellow_data_2016))
# 
# # create unique ID to match origins and destinations
# yellow_data_2016$p_id <- seq(1, nrow(yellow_data_2016), by = 1)
# 
# # convert to spatial points data frame for intersection with tlc zones
# yellow_list_2016 <- spatial_convert(select(yellow_data_2016, -one_of(c("pickup_longitude", "pickup_latitude")), -contains("dropoff")),
#                                    select(yellow_data_2016, -one_of(c("dropoff_longitude", "dropoff_latitude")), -contains("pickup")),
#                                    select(yellow_data_2016, one_of(c("pickup_longitude", "pickup_latitude"))),
#                                    select(yellow_data_2016, one_of(c("dropoff_longitude", "dropoff_latitude"))))
# 
# 
# 
# # match points to TLC Zones, this will drop out trip ends outside of TLC zones
# rm("yellow_data_2016")
# gc()
# yellow_list_2016$origins <- point.in.poly(yellow_list_2016$origins, zones)
# gc()
# yellow_list_2016$dests <- point.in.poly(yellow_list_2016$dests, zones) 

yellow_list_2016 <- list_clean_up(yellow_data_2016, zones)


save_dfs(c("yellow_list_2016"), "./Data/Clean_Yellow_data_frames_2016.rda")

### Green Cab ###
# load data
load("./Data/Green_data_frames_2016.rda")

# drop unused columns
green_data_2016 <- select(green_data_2016, one_of("lpep_pickup_datetime", "Lpep_dropoff_datetime", 
                                                  "Pickup_longitude", "Pickup_latitude", "Dropoff_longitude",
                                                  "Dropoff_latitude","Passenger_count", "Trip_distance")) %>%
  mutate(lpep_pickup_datetime = ymd_h(sub(":.*$", "", lpep_pickup_datetime)), Lpep_dropoff_datetime = 
           ymd_h(sub(":.*$", "", Lpep_dropoff_datetime)))
# lower case all columns
colnames(green_data_2016) <- tolower(colnames(green_data_2016))

# create unique ID to match origins and destinations
green_data_2016$p_id <- seq(1, nrow(green_data_2016), by = 1)

# convert to spatial points data frame for intersection with tlc zones
green_list_2016 <- spatial_convert(select(green_data_2016, -one_of(c("pickup_longitude", "pickup_latitude")), -contains("dropoff")),
                             select(green_data_2016, -one_of(c("dropoff_longitude", "dropoff_latitude")), -contains("pickup")),
                             select(green_data_2016, one_of(c("pickup_longitude", "pickup_latitude"))),
                             select(green_data_2016, one_of(c("dropoff_longitude", "dropoff_latitude"))))

# match points to TLC Zones, this will drop out trip ends outside of TLC zones
green_list_2016$origins <- point.in.poly(green_list_2016$origins, zones)
green_list_2016$dests <- point.in.poly(green_list_2016$dests, zones) 

rm("green_data_2016")
save_dfs(c("green_list_2016"), "./Data/Clean_Green_data_frames_2016.rda")

### Turnstile ###
# load data
load("./Data/Turnstile_data_frames.rda")

# create date time
turnstile_data <- mutate(turnstile_data, date_time = mdy_h(paste(DATE, sub(":.*$", "", TIME))))

save_dfs(c("turnstile_data"), "./Data/Clean_Turnstile_data_frames.rda")
