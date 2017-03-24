### Script to Load Data for Congestion Data Analysis
### Save data for each mode in a seperate .rda file so data doesn't need to be reloaded
### 

##### Packages #####
req_packages <- c("tidyverse", "readxl", "rgdal")

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
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE, na.strings = c("NA")))
}
import.xlsx <- function(filename, sheet, skip = 0, col_names = T) {
  return(read_excel(path = filename, sheet = sheet, na = "NA", skip = skip, col_names = col_names))
}
load.dir <- function(dir_name, file_ext, excl_word = NA, sheet_name, list = F){
  # Load all files of proper ext type in directory
  file_names <- list.files(path = dir_name)
  # drop out GTFS directory and 
  file_names <- file_names[grepl(paste0(".*", file_ext, "$"), file_names)]
  file_names <- file_names[!grepl(paste0(".*", excl_word, ".*"), file_names)]
  print(file_names)
  #loop over files to load and place in list
  file_list <- list()
  for(i in 1:length(file_names)){
    # get gtfs from dir
    print(file_names[i])
    if(file_ext == ".csv"){
      tmp_file <- import.csv(paste0(dir_name, file_names[i]))
    }else if(file_ext == ".xlsx"){
      tmp_file <- import.xlsx(file_names[i], sheet_name)
    }else if(file_ext == ".txt"){
      # assume txt is csv
      tmp_file <- import.csv(paste0(dir_name, file_names[i]))
    }else{
      print("incorrect file ext")
    }
    #add file to main
    file_list[[i]] <- tmp_file
    # if(i==1){
    #   main_df <- tmp_file
    # }else{
    #   main_df <- bind_rows(main_df, tmp_file)
    # }
  }
  # check if one df or list is desired, list is better for large data to process each df sequentially
  if(list){
    return(file_list)
  }else{
    print('files loaded. creating data frame.')
    return(bind_rows(file_list))
  }
  
  
}
save_dfs <- function(dfs, location){
  save(list = dfs, file = location)
  rm(list = dfs, pos = ".GlobalEnv")
  gc()
}

##### Data for Congestion Analysis #####

### TNC ##
# load data
tnc_data <- load.dir("./Data/TNC_Data/", ".csv", "taxi")
taxi_zone_lookup <- as_data_frame(import.csv("./Data/TNC_Data/taxi_zone_lookup.csv"))
# save/clear data
dfs <- c("tnc_data", "taxi_zone_lookup")
save_dfs(dfs, "./Data/TNC_data_frames.rda")


#### Bike ###
# load data
bike_data <- load.dir("./Data/Bike/", ".csv", NA)
# save/clear data
dfs <- c("bike_data")
save_dfs(dfs,  "./Data/Bike_data_frames.rda")


#### Yellow Cab ###
# load data into two data frames, one for each year
yellow_data_2016 <- load.dir("./Data/Yellow_Cab/", ".csv", list = T)
# save/clear data
dfs <- c("yellow_data_2016")
save_dfs(dfs,"./Data/Yellow_data_frames_2016.rda")


### Green Cab ###
# load data into two data frames, one for each year
green_data_2016 <- load.dir("./Data/Green_Cab/", ".csv", "2015")
# save/clear data
dfs <- c("green_data_2016")
save_dfs(dfs, "./Data/Green_data_frames_2016.rda")


### Turnstile ###
# load data
turnstile_data <- load.dir("./Data/Turnstile_data/", ".txt", "Field")
colnames(turnstile_data) <- tolower(colnames(turnstile_data))
# save/clear data
dfs <- c("turnstile_data")
save_dfs(dfs, "./Data/Turnstile_data_frames.rda")

### TLC Zones ###
zones <- readOGR("./Data/taxi_zones", layer = "taxi_zones_wgs")
dfs <- c("zones")
save_dfs(dfs, "./Data/tlc_zones.rda")
