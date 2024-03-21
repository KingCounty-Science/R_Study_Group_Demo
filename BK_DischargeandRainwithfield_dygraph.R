# Load libraries
install.packages("RODBC")
install.packages("dygraphs")
install.packages("odbc")
install.packages("dplyr")
require(dygraphs)
require(odbc)
library(DBI)
library(dygraphs)
library(RODBC)
library(odbc)
library(dplyr)

# Connect to the database
con <- dbConnect(odbc(), Driver = "SQL Server", Server = "KCITSQLPRNRPX01", Database = "GData", Trusted_Connection = "True")

# Load functions
source("C:/Users/bkeeler/R/GdataConnection/my_functions.R")

# Enter start_date, end_date and optional time (yyyy-mm-dd hh:mm:ss), site codes
# can add multiple with commas
# NEED TO ADD 7 HOURS TO CONVERT TO UTC IF MATCHING FIELD VISITS
start_date <- "2023-11-01"
end_date <- "" #can leave blank, will pull to last record in db
rain_site_codes <- c("44u")
discharge_site_codes <- c("14b", "63a")

#Location-based site codes; comment/uncomment to run

#RPWS-Tosh and Country
# rain_site_codes <- c("RPWS_RG_TOS")
# discharge_site_codes <- c("RPWS_TOSMI", "RPWS_TOSMO", "RPWS_COUMI", "RPWS_COUMO")

#RPWS-Monticello and Tylers
# rain_site_codes <- c("RPWS_RG_MON")
# discharge_site_codes <- c("RPWS_TYLMI", "RPWS_TYLMO", "RPWS_MONMS", "RPWS_MONMN", "RPWS_MONMO")

#RPWS-Watershed-preserve
#rain_site_codes <- c("02VN")
#discharge_site_codes <- c("RPWS_COLM", "RPWS_SEIMS", "RPWS_SEIMN")

#North Lake Washington
# rain_site_codes <- c("35u")
# discharge_site_codes <- c("35c", "34a", "45c", "30an")

#Thornton Creek
# rain_site_codes <- c("35u")
# discharge_site_codes <- c("58a", "58c", "58d", "58e", "58f")

#source functions
source("C:/Users/bkeeler/R/GdataConnection/my_functions.R")

# Initialize lists to store rain, discharge, and field data
rain_data_list <- retrieve_rain_data(con, start_date, end_date, rain_site_codes)
discharge_data_list <- retrieve_discharge_data(con, start_date, end_date, discharge_site_codes)

# Combine discharge and field discharge data into a single data frame
merged_data <- NULL
for (discharge_site_code in discharge_site_codes) {
  discharge_data <- discharge_data_list[[discharge_site_code]]
  # Rename columns
  colnames(discharge_data) <- c("Timestamp", paste("Discharge", discharge_site_code))
  # Merge discharge data into the combined data frame
  if (is.null(merged_data)) {
    merged_data <- discharge_data
  } else {
    merged_data <- merge(merged_data, discharge_data, by = "Timestamp", all.x = TRUE)
  }
}

# Rename rain data columns and merge using "Timestamp"
for (rain_site_code in rain_site_codes) {
  rain_data <- rain_data_list[[rain_site_code]]
  colnames(rain_data) <- c("Timestamp", paste("Rain", rain_site_code))
  merged_data <- merge(merged_data, rain_data, by = "Timestamp", all.x = TRUE)
}

#turn data into PDT from UTC
#merged_data$Timestamp <- merged_data$Timestamp - hours(7)

# Create dygraph with rain and discharge series
dygraph_object <- dygraph(merged_data)

# Add rain series to dygraph as bars
for (rain_site_code in rain_site_codes) {
  col_name_rain <- paste("Rain", rain_site_code)
  dygraph_object <- dygraph_object %>%
    dyBarSeries(col_name_rain, label = paste("Rain", rain_site_code), axis = "y2", color = "blue") %>%
    dyAxis("y2", label = "Rain", valueRange = c(0.40, 0), independentTicks = TRUE)
}

# Add discharge series to dygraph
for (discharge_site_code in discharge_site_codes) {
  col_name_discharge <- paste("Discharge", discharge_site_code)
  dygraph_object <- dygraph_object %>%
    dySeries(col_name_discharge, label = paste("Discharge", discharge_site_code), axis = "y")
}

# Set options to adjust legend size and position
dygraph_object <- dygraph_object %>%
  dyAxis("x", label = "Timestamp (PDT)")%>%
  dyLegend(show = c("onmouseover"), width = 250, labelsSeparateLines = FALSE)%>%
  dyCrosshair(direction = "both")%>%
  dyOptions(useDataTimezone = TRUE) # Keep in PDT

print(dygraph_object)

# Format end_date for file name (remove spaces and colons)
formatted_end_date <- gsub("[: ]", "_", end_date)

# Create a file name based on discharge_site_code and formatted_end_date
file_name <- paste(discharge_site_code,"Discharge_",rain_site_code,"rain",formatted_end_date,".csv")

# Write CSV with the formatted file name
write.csv(merged_data, file = file_name, na = "")

# Disconnect - recommended
dbDisconnect(con)