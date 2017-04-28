# NYC_Travel_Data
## Data Backend for Aggregating/Storing and Shiny App for Analyzing NYC Trip Data by Mode

A set of scripts for cleaning, aggregating, and storing TLC, CitiBike, and Subway (forthcoming) trip data in an SQLite database. 
The data loading script assumes each dataset is stored in its own directory in the project root. The raw data which was processed and placed in the database and app can be downloaded from MTA, TLC, and CitiBike websites. Note that all pre-processing was done on a maching with 32GB of RAM. Given the large size of the data, users with less RAM may encounter issues. A future step is to reduce the dependency of the pre-processing scripts on large amounts of RAM. 

The SQLite database is formatted as a data warehouse with the fact table containing the number of trips between TLC Zones (only origins
for TNC data) along with summary statistics for yellow and green taxi trips such as average trip distance. The data warehouse also has
dimensions for hour, date, zone, mode, weather (forthcoming), and origin/destination to allow for easy filtering along these dimensions.

The database drives a R Shiny dashboard which allows for users to query the data geospatially, temporally, and by provider (taxi, uber, citi bike, etc.) while providing the ability to smooth the time series data and identify trends. 

A recent version of the dashboard can be accessed here: https://torch77.shinyapps.io/transpo_app_v2/. The server hosting this version of the app is a free version so it may be a bit laggy.
