### Script to Aggregate and Store Data in Database
### Save data for each mode in a seperate .rda file so data doesn't need to be reloaded
### 

##### Packages #####
req_packages <- c("tidyverse", "lubridate", "RSQLite", "readxl")

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
load_table <- function(db, table, table_name, pk = NA){
  
  print(paste("Loading", table_name))
  dbWriteTable(conn = db, name = table_name,
               value = table, 
               row.names = F, overwrite = T)
  
  # create index
  if(!is.na(pk)){
    s <- paste0("CREATE UNIQUE INDEX ", table_name, "_", pk, "_index ON ", table_name, " (", pk, ");")
    dbClearResult(dbSendQuery(conn = db, s))
  }
  # create fact table indices
  if(table_name == "fact_table"){
    s <- paste0("CREATE INDEX ", table_name, "_", "hour_id", "_index ON ",
                table_name, " (", "hour_id", ");")
    dbClearResult(dbSendQuery(conn = db, s))
    s <- paste0("CREATE INDEX ", table_name, "_", "datekey", "_index ON ",
                table_name, " (", "datekey", ");")
    dbClearResult(dbSendQuery(conn = db, s))
    s <- paste0("CREATE INDEX ", table_name, "_", "provider_id", "_index ON ",
                table_name, " (", "provider_id", ");")
    dbClearResult(dbSendQuery(conn = db, s))
  }
  
}
analyze_db <- function(db){
  s <- "Analyze;"
  dbClearResult(dbSendQuery(conn = db, s))
}
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE, na.strings = c("NA")))
}
import.xlsx <- function(filename, sheet, skip = 0, col_names = T) {
  return(read_excel(path = filename, sheet = sheet, na = "NA", skip = skip, col_names = col_names))
}
save_dfs <- function(dfs, location){
  save(list = dfs, file = location)
  rm(list = dfs, pos = ".GlobalEnv")
  gc()
}
# function to create datekey and handle padding of day and month
create_datekey <- function(df, col){
  
  df <- ungroup(df)
  df <- mutate(df, year = year(df[[col]]), month = month(df[[col]]), day = day(df[[col]]), 
               month = if_else(month < 10, paste0(0, month), as.character(month)), 
               day = if_else(day < 10, paste0(0, day), as.character(day)), 
               datekey = as.numeric(paste0(year, month, day))) %>%
    select(-one_of(c("year", "month", "day", col)))
  
  return(df)
  
}
# function to collapse origin destination lists into one table and create columns
od_collapse <- function(list, mode, date_col_o, date_col_d){
  
  colnames(list$origins) <- tolower(colnames(list$origins))
  colnames(list$dests) <- tolower(colnames(list$dests))
  
  origins <- ungroup(list$origins) %>%
    mutate(hour_id = hour(list$origins[[date_col_o]]), mode_id = mode, od_id = 1) %>% 
    rename(location_id = locationid, datetime = pickup_datetime) 
  
  dests <- ungroup(list$dests) %>%
    mutate(hour_id = hour(list$dests[[date_col_d]]), mode_id = mode, od_id = 2) %>% 
    rename(location_id = locationid,  datetime = dropoff_datetime)
  
  # combine and create datekey
  df <- bind_rows(origins, dests)
  df <- create_datekey(df, "datetime")
  
  # keep only correct columns, put NAs for modes other than yellow and green
  if(mode == 2 | mode == 3){
    df <- select(df, hour_id, datekey, mode_id, od_id, location_id, trips,
                 passengers, person_miles_travelled, avg_trip_distance)
  }else{
    df <- mutate(df, passengers = NA, person_miles_travelled = NA, avg_trip_distance = NA) %>%
      select(hour_id, datekey, mode_id, od_id, location_id, trips)
  }
  
  return(df)
  
}
# function to aggregate yellow data, split variable indicates whether to combine dfs before aggregating
# not combining saves memory since the records for each table are reduced, however, this can cause some erroneous entries
# around the beginning of a month from trips which began in the previous month and are split between csvs
# I have only tried combining 6 months at once (on a machine with 32gb ram) so any more may cause memory issues
aggregate_yellow <- function(yellow_list_2016, split){
  if(split){
    for(i in 1:length(yellow_list_2016)){
      print(i)
      if(i==1){
        if(is.data.frame(yellow_list_2016[[i]]$origins)){
          # handle no lat lon data in df
          agg_yellow_origins <- group_by(yellow_list_2016[[i]]$origins, LocationID, tpep_pickup_datetime) %>%
            summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                      person_miles_travelled = sum(trip_distance*passenger_count)) 
          
          agg_yellow_dests <- group_by(yellow_list_2016[[i]]$dests, LocationID, tpep_dropoff_datetime) %>%
            summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                      person_miles_travelled = sum(trip_distance)/sum(passenger_count)) 
        }else{
          agg_yellow_origins <- group_by(yellow_list_2016[[i]]$origins@data, LocationID, tpep_pickup_datetime) %>%
            summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                      person_miles_travelled = sum(trip_distance*passenger_count)) 
          
          agg_yellow_dests <- group_by(yellow_list_2016[[i]]$dests@data, LocationID, tpep_dropoff_datetime) %>%
            summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                      person_miles_travelled = sum(trip_distance)/sum(passenger_count)) 
        }
      }else{
        if(is.data.frame(yellow_list_2016[[i]]$origins)){
        agg_yellow_origins <- bind_rows(agg_yellow_origins, 
                                        group_by(yellow_list_2016[[i]]$origins, LocationID, tpep_pickup_datetime) %>%
                                          summarise(trips = n(),
                                                    passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                                                    person_miles_travelled = sum(trip_distance*passenger_count)))
        
        agg_yellow_dests <- bind_rows(agg_yellow_dests, 
                                     group_by(yellow_list_2016[[i]]$dests, LocationID, tpep_dropoff_datetime) %>%
                                       summarise(trips = n(),
                                                 passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                                                 person_miles_travelled = sum(trip_distance)/sum(passenger_count))) 
        }else{
          agg_yellow_origins <- bind_rows(agg_yellow_origins, 
                                          group_by(yellow_list_2016[[i]]$origins@data, LocationID, tpep_pickup_datetime) %>%
                                            summarise(trips = n(),
                                                      passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                                                      person_miles_travelled = sum(trip_distance*passenger_count)))
          
          agg_yellow_dests <- bind_rows(agg_yellow_dests, 
                                        group_by(yellow_list_2016[[i]]$dests@data, LocationID, tpep_dropoff_datetime) %>%
                                          summarise(trips = n(),
                                                    passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                                                    person_miles_travelled = sum(trip_distance)/sum(passenger_count))) 
        }
        
      }
      
    }
  }else{
    for(i in 1:length(yellow_list_2016)){
      print(i)
      if(i==1){
        if(is.data.frame(yellow_list_2016[[i]]$origins)){
          agg_yellow_origins <- yellow_list_2016[[i]]$origins
          
          agg_yellow_dests <- yellow_list_2016[[i]]$dests
        }else{
          agg_yellow_origins <- yellow_list_2016[[i]]$origins@data 
          
          agg_yellow_dests <- yellow_list_2016[[i]]$dests@data
        }
      }else{
        if(is.data.frame(yellow_list_2016[[i]]$origins)){
          agg_yellow_origins <- bind_rows(agg_yellow_origins, yellow_list_2016[[i]]$origins)
          
          agg_yellow_dests<- bind_rows(agg_yellow_dests, yellow_list_2016[[i]]$dests) 
        }else{
          agg_yellow_origins <- bind_rows(agg_yellow_origins, yellow_list_2016[[i]]$origins@data)
          
          agg_yellow_dests<- bind_rows(agg_yellow_dests, yellow_list_2016[[i]]$dests@data) 
        }
        
      }
      yellow_list_2016[[i]] <- NA
      gc()
    }
    # aggregate data
    print("aggregating")
    agg_yellow_origins <-  group_by(agg_yellow_origins, LocationID, tpep_pickup_datetime) %>%
      summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                person_miles_travelled = sum(trip_distance*passenger_count))
    
    agg_yellow_dests <- group_by(agg_yellow_dests, LocationID, tpep_dropoff_datetime) %>%
      summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                person_miles_travelled = sum(trip_distance)/sum(passenger_count))
  }
  
  # clean up columns
  agg_yellow_origins <- rename(agg_yellow_origins, pickup_datetime = tpep_pickup_datetime)
  agg_yellow_dests <- rename(agg_yellow_dests, dropoff_datetime = tpep_dropoff_datetime)
  # list dfs and name
  agg_yellow_list <- list(agg_yellow_origins, agg_yellow_dests)
  names(agg_yellow_list) <- c("origins","dests")
  gc()
  
  return(agg_yellow_list)
}



##### Aggregate Data #####
### TNC ###
load("./Data/Clean_TNC_data_frames.rda")

### Aggregate by day hour and zone
agg_tnc_data <- group_by(tnc_data, Dispatching_base_num, locationID, pickup_date_hour) %>%
  summarise(trips = n()) 

rm("tnc_data")
save_dfs(c("agg_tnc_data"), "./Data/Agg_TNC_data_frames.rda")

### Bike ###
load("./Data/Clean_Bike_data_frames.rda")

### Aggregate by day hour and zone
agg_bike_origins <- group_by(bike_list$origins@data, LocationID, starttime) %>%
  summarise(trips = n()) %>%
  rename(pickup_datetime = starttime)

agg_bike_dests <- group_by(bike_list$dests@data, LocationID, stoptime) %>%
  summarise(trips = n()) %>%
  rename(dropoff_datetime = stoptime)

agg_bike_list <- list(agg_bike_origins, agg_bike_dests)
names(agg_bike_list) <- c("origins","dests")

rm("bike_list", "duplicated_station_names", "agg_bike_origins", "agg_bike_dests", "station_list")
save_dfs(c("agg_bike_list"), "./Data/Agg_Bike_data_frames.rda")

### Green ###
load("./Data/Clean_Green_data_frames.rda")

### Aggregate by day hour and zone
agg_green_origins <- group_by(green_list$origins, LocationID, lpep_pickup_datetime) %>%
  summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
            person_miles_travelled = sum(trip_distance*passenger_count)) %>%
  rename(pickup_datetime = lpep_pickup_datetime)

agg_green_dests <- group_by(green_list$dests, LocationID, lpep_dropoff_datetime) %>%
  summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
            person_miles_travelled = sum(trip_distance)/sum(passenger_count)) %>%
  rename(dropoff_datetime = lpep_dropoff_datetime)

agg_green_list <- list(agg_green_origins, agg_green_dests)
names(agg_green_list) <- c("origins","dests")

rm("green_list", "agg_green_origins", "agg_green_dests")
save_dfs(c("agg_green_list"), "./Data/Agg_Green_data_frames.rda")

### Yellow ###
load("./Data/Clean_Yellow_data_frames_2016.rda")

### Aggregate by day hour and zone
agg_yellow_list <- aggregate_yellow(yellow_list_2016, split = F)
# throwing a warning re memory right now but not an error, assuming this is fine
# should look into using swap file space
rm("yellow_list_2016")
gc()
save_dfs(c("agg_yellow_list"), "./Data/Agg_yellow_data_frames.rda")


##### Dimensions #####
# hour table
hour_tb <- tibble(hour_id = 0:23, hour = 0:23) %>% 
  mutate(hour_am_pm = if_else(hour<12, paste0(hour, "am"), paste0(hour%%12, "pm")),
         hour_am_pm = gsub("^0", "12", hour_am_pm),
         hour_am_pm = as.factor(hour_am_pm), 
         am_pm = as.factor(if_else(hour_id < 12, "AM", "PM")), 
         peak_off_peak = as.factor(if_else((hour_id >= 7 & hour_id < 10) | (hour_id >= 17 & hour_id < 20), 
                                           "Peak", "Off Peak")))
date_tb <- import.csv("./Data/Date_Table_v2.csv")
colnames(date_tb) <- tolower(colnames(date_tb))
#date_tb <- select(date_tb, datekey, fulldate, holiday.indicator)
mode_tb <- tibble(mode_id = 1:4, mode_text = c("TNC", "Taxi", "Subway", "Bike Share"))
# tnc companies and service providers
provider_tb <- import.csv("./Data/Aggregate_TLC_Data/FHV_Base_Aggregate_Report.csv")
# categorize dispatching stations 
provider_tb <- distinct(provider_tb, Base_License_Number, Base_Name, DBA) %>%
  mutate(provider_type = if_else(grepl("^UBER-.*$", DBA), "Uber", 
                            if_else(grepl("^LYFT-.*$", DBA), "Lyft", 
                                    if_else(grepl("^VIA-.*$", DBA), "Via",
                                            if_else(grepl("^GETT$", DBA), "Gett", 
                                                    if_else(grepl("^JUNO$", DBA), "Juno", "Other TNC/Black Car")))))) 

provider_tb <- mutate(provider_tb, provider_id = if_else(provider_type == "Uber", 1, 
                                          if_else(provider_type == "Lyft", 2, 
                                                  if_else(provider_type == "Via", 3, 
                                                          if_else(provider_type == "Gett", 4, 
                                                                  if_else(provider_type == "Juno", 5, 6)))))) %>%
  select(provider_id, Base_License_Number, provider_type) %>%
  # add other service providers
  bind_rows(data_frame(provider_id = c(7,8,9,10), Base_License_Number = c(NA,NA,NA,NA),
                       provider_type = c("Yellow Taxi", "Green Taxi", "Citi Bike", "Subway")))
weather_tb <- import.csv("./Data/Weather/weather_data.csv")
weather_tb <- filter(weather_tb, STATION_NAME == "NY CITY CENTRAL PARK NY US") %>%
  rename(datekey = DATE)
taxi_zone_lookup <- as_data_frame(import.csv("./Data/TNC_Data/taxi_zone_lookup.csv")) %>%
  rename(location_id = LocationID)
od_tb <- tibble(od_id = 1:2, od_type = c("Origin", "Destination"))
##### Store Data #####
### Initial Set Up ###
# create db
if(!dir.exists("./Shiny_App/db")){
  dir.create("./shiny_App/db")
}

# Connect or Create Database in db directory
db <- dbConnect(SQLite(), "./Shiny_App/db/congestion_large.sqlite")

# load dimensions, no primary keys for now
load_table(db, taxi_zone_lookup, "taxi_zones", "location_id")
load_table(db, hour_tb, "hour_dim", "hour_id")
load_table(db, date_tb, "date_dim", "datekey")
load_table(db, mode_tb, "mode_dim", "mode_id")
load_table(db, distinct(provider_tb, provider_id, provider_type), "service_providers", "provider_id")
load_table(db, weather_tb, "weather_dim", "datekey")
load_table(db, od_tb, "od_dim", "od_id")

### Fact Table 
load("./Data/Agg_TNC_data_frames.rda")
load("./Data/Agg_Bike_data_frames.rda")
load("./Data/Agg_yellow_data_frames.rda")
load("./Data/Agg_Green_data_frames.rda")
# TNC
agg_tnc_table <- ungroup(agg_tnc_data) %>%
  mutate(hour_id = hour(pickup_date_hour), mode_id = 1, od_id = 1) %>% 
  inner_join(select(provider_tb, provider_id, Base_License_Number), by = c("Dispatching_base_num" = "Base_License_Number")) %>%
  rename(location_id = locationID) %>%
  select(hour_id, pickup_date_hour, mode_id, od_id, location_id, provider_id, trips) %>%
  # group by the 6 company categories to cut down on table size
  group_by(hour_id, pickup_date_hour, mode_id, od_id, location_id, provider_id) %>%
  summarise(trips = sum(trips))
agg_tnc_table <- create_datekey(agg_tnc_table, "pickup_date_hour")
# other modes
agg_yellow_table <- od_collapse(agg_yellow_list, 2, "pickup_datetime", "dropoff_datetime") %>%
  mutate(provider_id = 7) %>%
  # only doing trips for now to save space
  select(-one_of(c("passengers", "person_miles_travelled", "avg_trip_distance")))
agg_green_table <- od_collapse(agg_green_list, 2, "pickup_datetime", "dropoff_datetime")%>%
  mutate(provider_id = 8) %>%
  # only doing trips for now to save space
  select(-one_of(c("passengers", "person_miles_travelled", "avg_trip_distance")))
agg_bike_table <- od_collapse(agg_bike_list, 4, "pickup_datetime", "dropoff_datetime")%>%
  mutate(provider_id = 9)

# bind into one table and sort by date, hour, mode before creating pk
fact_table <- bind_rows(agg_bike_table, agg_green_table, agg_yellow_table, agg_tnc_table) %>%
  mutate(hour_am_pm = if_else(hour_id <12, paste0(hour_id, "am"), paste0(hour_id%%12, "pm")),
         hour_am_pm = gsub("^0", "12", hour_am_pm),
         hour_am_pm = as.factor(hour_am_pm))%>%
  arrange(datekey, hour_id, mode_id)
fact_table <- mutate(fact_table, fact_id = seq(1:nrow(fact_table))) 

# maybe we should have a seperate table for tnc companies?
load_table(db, fact_table, "fact_table", "fact_id")

# analyze db
analyze_db(db)
