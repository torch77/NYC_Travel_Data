# NYC_Travel_Data
Data Backend for Aggregating and Analyzing NYC Trip Data by Mode
A set of scripts for cleaning, aggregating, and storing TLC, CitiBike, and Subway trip data in an SQLite database. 
The data loading script assumes each dataset is stored in its own directory in the project root. The data can be 
downloaded from MTA, TLC, and CitiBike websites.

The SQLite database is formatted as a datawarehouse with the fact table containing the number of trips between TLC Zones (only origins
for TNC data) along with summary statistics for yellow and green taxi trips such as average trip distance. The data warehouse also has
dimensions for hour, date, zone, mode, weather, and origin/destination to allow for easy filtering along these dimensions.

The beginnings of Shiny App to query the SQLite database and visualize the results are located in the Shiny App Directory. The app allows
for querying of the data by system, hour, date, and zone via an interactive map made using Leaflet.
