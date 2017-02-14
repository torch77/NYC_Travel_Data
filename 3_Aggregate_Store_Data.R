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
# function to create datekey nad handle padding of day and month
create_datekey <- function(df, col){
  
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
load("./Data/Clean_Green_data_frames_2016.rda")

### Aggregate by day hour and zone
agg_green_origins <- group_by(green_list_2016$origins@data, LocationID, lpep_pickup_datetime) %>%
  summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
            person_miles_travelled = sum(trip_distance*passenger_count)) %>%
  rename(pickup_datetime = lpep_pickup_datetime)

agg_green_dests <- group_by(green_list_2016$dests@data, LocationID, lpep_dropoff_datetime) %>%
  summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
            person_miles_travelled = sum(trip_distance)/sum(passenger_count)) %>%
  rename(dropoff_datetime = lpep_dropoff_datetime)

agg_green_list <- list(agg_green_origins, agg_green_dests)
names(agg_green_list) <- c("origins","dests")

rm("green_list_2016", "agg_green_origins", "agg_green_dests")
save_dfs(c("agg_green_list"), "./Data/Agg_Green_data_frames.rda")

### Yellow ###
load("./Data/Clean_Yellow_data_frames_2016.rda")

### Aggregate by day hour and zone
for(i in 1:length(yellow_list_2016)){
  print(i)
  if(i==1){
    agg_yellow_origins <- group_by(yellow_list_2016[[i]]$origins@data, LocationID, tpep_pickup_datetime) %>%
      summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                person_miles_travelled = sum(trip_distance*passenger_count)) 
    
    agg_yellow_dests <- group_by(yellow_list_2016[[i]]$dests@data, LocationID, tpep_dropoff_datetime) %>%
      summarise(trips = n(), passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                person_miles_travelled = sum(trip_distance)/sum(passenger_count)) 
  }else{
    agg_yellow_origins <- bind_rows(agg_yellow_origins, 
                                    group_by(yellow_list_2016[[i]]$origins@data, LocationID, tpep_pickup_datetime) %>%
                                      summarise(trips = n(),
                                        passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                                        person_miles_travelled = sum(trip_distance*passenger_count)))
    
    agg_yellow_dests<- bind_rows(agg_yellow_dests, 
                                 group_by(yellow_list_2016[[i]]$dests@data, LocationID, tpep_dropoff_datetime) %>%
                                  summarise(trips = n(),
                                    passengers = sum(passenger_count), avg_trip_distance = mean(trip_distance), 
                                    person_miles_travelled = sum(trip_distance)/sum(passenger_count))) 


  }
  
}
agg_yellow_origins <- rename(agg_yellow_origins, pickup_datetime = tpep_pickup_datetime)
agg_yellow_dests <- rename(agg_yellow_dests, dropoff_datetime = tpep_dropoff_datetime)

agg_yellow_list <- list(agg_yellow_origins, agg_yellow_dests)
names(agg_yellow_list) <- c("origins","dests")

rm("yellow_list_2016", "agg_yellow_origins", "agg_yellow_dests")
save_dfs(c("agg_yellow_list"), "./Data/Agg_yellow_data_frames.rda")



##### Dimensions #####
# hour table
hour_tb <- tibble(hour_id = 0:23, hour = 0:23) %>% 
  mutate(hour_am_pm = if_else(hour<12, paste0(hour, "am"), paste0(hour%%12, "pm")),
         hour_am_pm = gsub("^0", "12", hour_am_pm),
         hour_am_pm = as.factor(hour_am_pm))
date_tb <- import.csv("./Data/Date_Table.csv")
colnames(date_tb) <- tolower(colnames(date_tb))
mode_tb <- tibble(mode_id = 1:5, mode_text = c("TNC", "Yellow Taxi", "Green Taxi", "Subway", "CitiBike"))
tnc_tb <- import.csv("./Data/Aggregate_TLC_Data/FHV_Base_Aggregate_Report.csv")
tnc_tb <- distinct(tnc_tb, Base_License_Number, Base_Name)
tnc_tb <- mutate(tnc_tb, tnc_id = seq(1:nrow(tnc_tb))) %>%
  select(tnc_id, Base_License_Number, Base_Name)
weather_tb <- import.csv("./Data/Weather/weather_data.csv")
weather_tb <- filter(weather_tb, STATION_NAME == "NY CITY CENTRAL PARK NY US") %>%
  rename(datekey = DATE)
taxi_zone_lookup <- as_data_frame(import.csv("./Data/TNC_Data/taxi_zone_lookup.csv")) %>%
  rename(location_id = LocationID)
od_tb <- tibble(od_id = 1:3, od_type = c("Origin", "Destination", NA))
##### Store Data #####
### Initial Set Up ###
# create db
if(!dir.exists("./Data/db")){
  dir.create("./Data/db")
}

# Connect or Create Database in db directory
db <- dbConnect(SQLite(), "./Data/db/congestion.sqlite")

# load dimensions, no primary keys for now
load_table(db, taxi_zone_lookup, "taxi_zones", "location_id")
load_table(db, hour_tb, "hour_dim", "hour_id")
load_table(db, date_tb, "date_dim", "datekey")
load_table(db, mode_tb, "mode_dim", "mode_id")
load_table(db, tnc_tb, "tnc_companies", "tnc_id")
load_table(db, weather_tb, "weather_dim", "datekey")
load_table(db, od_tb, "od_dim", "od_id")

### Fact Table 
load("./Data/Agg_TNC_data_frames.rda")
load("./Data/Agg_Bike_data_frames.rda")
load("./Data/Agg_yellow_data_frames.rda")
load("./Data/Agg_Green_data_frames.rda")
# TNC
agg_tnc_table <- ungroup(agg_tnc_data) %>%
  mutate(hour_id = hour(pickup_date_hour), mode_id = 1, od_id = 3) %>% 
  inner_join(select(tnc_tb, tnc_id, Base_License_Number), by = c("Dispatching_base_num" = "Base_License_Number")) %>%
  rename(location_id = locationID) %>%
  select(hour_id, pickup_date_hour, mode_id, od_id, location_id, trips)
agg_tnc_table <- create_datekey(agg_tnc_table, "pickup_date_hour")
# other modes
agg_yellow_table <- od_collapse(agg_yellow_list, 2, "pickup_datetime", "dropoff_datetime")
agg_green_table <- od_collapse(agg_green_list, 3, "pickup_datetime", "dropoff_datetime")
agg_bike_table <- od_collapse(agg_bike_list, 5, "pickup_datetime", "dropoff_datetime")

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
