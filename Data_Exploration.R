# Initial Data Exploration for NYC Travel Congestion Tool
# 12/27/16

library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)



Sys.setenv("plotly_username"="torch77")
Sys.setenv("plotly_api_key"="qtHd6eMLInfEgxhJMRCI")

options("scipen" = 100)

##### Functions
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE, na.strings = c("NA")))
}
write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}
import.xlsx <- function(filename, sheet, skip = 0, col_names = T) {
  return(read_excel(path = filename, sheet = sheet, na = "NA", skip = skip, col_names = col_names))
}
load.dir <- function(dir_name, file_ext, excl_word, sheet_name){
  # Load all files of proper ext type in directory
  file_names <- list.files(path = dir_name)
  # drop out GTFS directory and 
  file_names <- file_names[grepl(paste0("*", file_ext, "$"), file_names)]
  file_names <- file_names[!grepl(paste0("*", excl_word, "*"), file_names)]
  print(file_names)
  #loop over files to load
  for(i in 1:length(file_names)){
    # get gtfs from dir
    print(file_names[i])
    if(file_ext == ".csv"){
      tmp_file <- import.csv(paste0(dir_name, file_names[i]))
    }else if(file_ext == ".xlsx"){
      tmp_file <- import.xlsx(file_names[i], sheet_name)
    }else{
      print("incorrect file ext")
    }
    #add file to main
    #file_list[[filename[i]]] <- tmp_file
    if(i==1){
      main_df <- tmp_file
    }else{
      main_df <- bind_rows(main_df, tmp_file)
    }
  }
  return(as_data_frame(main_df))
  
}

#### TNC Data
load("./Data/Yellow_data_frames.rda")
### Missing values
print("TNC Missing Values")
print(paste("There are:", nrow(filter(tnc_data, complete.cases(tnc_data))), "complete obs out of", 
            nrow(tnc_data), "or", 
            nrow(filter(tnc_data, complete.cases(tnc_data)))/nrow(tnc_data)))
print(paste("There are:", nrow(filter(tnc_data, is.na(Dispatching_base_num))), "missing dispatch bases
            out of", nrow(tnc_data), "or", 
            nrow(filter(tnc_data, is.na(Dispatching_base_num)))/nrow(tnc_data)))
print(paste("There are:", nrow(filter(tnc_data, is.na(Pickup_date))), "missing pickup dates
            out of", nrow(tnc_data), "or", 
            nrow(filter(tnc_data, is.na(Pickup_date)))/nrow(tnc_data)))
print(paste("There are:", nrow(filter(tnc_data, is.na(locationID))), "missing location IDs
            out of", nrow(tnc_data), "or", 
            nrow(filter(tnc_data, is.na(locationID)))/nrow(tnc_data)))

#### Turnstile Data
load("./Data/Turnstile_data_frames.rda")
### Missing values
print("Turnstile Missing Values")
print(paste("There are:", nrow(filter(turnstile_data, complete.cases(turnstile_data))), "complete obs out of", 
            nrow(turnstile_data), "or", 
            nrow(filter(turnstile_data, complete.cases(turnstile_data)))/nrow(turnstile_data)))
## find duplicate stations, stations with low counts likely are an error
group_by(turnstile_data, STATION, LINENAME, DIVISION) %>%
  summarise(num = n()) %>%
  arrange(num) %>%
  print(n = 50)

filter(turnstile_data, grepl(".42.",STATION)) %>% distinct(STATION, LINENAME, DIVISION)



#### Look at Database
db <- src_sqlite("./Data/db/congestion.sqlite")

### Tables
fact_tbl <- tbl(db, "fact_table")
taxi_zones <- tbl(db, "taxi_zones")
date_tbl <- tbl(db, "date_dim")

### Look at Bike trips in Manhattan
taxi_zones_mn <- filter(taxi_zones, Borough == "Manhattan")
bike_trips <- filter(fact_tbl, mode_id == 5)
bike_trips_mn <- inner_join(bike_trips, taxi_zones_mn) %>%
  inner_join(date_tbl) %>%
  inner_join(tbl(db, "od_dim")) %>%
  group_by(location_id, Borough, Zone, od_type, FullDate) %>%
  summarise( trips = sum(trips)) %>%
  collect(n = Inf) %>%
  mutate(FullDate = ymd(FullDate))

p <- ggplot(data = bike_trips_mn, aes(x = FullDate, y = trips, color = od_type)) +
  geom_line() +
  facet_wrap("Zone")
ggplotly(p)


#### Look at Trips by Mode for a Specific Zone
# union sq zone id
un_sq <- 234

all_trips <- inner_join(fact_tbl, filter(taxi_zones, location_id == un_sq)) %>%
  inner_join(date_tbl) %>%
  inner_join(tbl(db, "od_dim")) %>%
  inner_join(tbl(db, "hour_dim")) %>%
  inner_join(tbl(db, "mode_dim")) %>%
  collect(n = Inf) %>%
  mutate(date_hour = ymd_h(paste(fulldate, hour_id)))
         
p <- ggplot(data = filter(all_trips),
            aes(x = date_hour, y = trips, color = od_type)) +
  geom_line() +
  facet_wrap("mode_text")
ggplotly(p)




