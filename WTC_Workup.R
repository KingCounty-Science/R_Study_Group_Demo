# Define the list of packages and their versions you need
packages <- c('DBI', 'odbc', 'dplyr', 'dygraphs', 'lubridate', 'zoo')

# Function to install and load a package
install_and_load <- function(package) {
  # Attempt to load the package silently
  if (!suppressPackageStartupMessages(require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))) {
    tryCatch({
      # Install the package from the specified repository
      install.packages(package, repos = "http://cran.us.r-project.org")
      # Load the package, suppressing startup messages
      suppressPackageStartupMessages(library(package, character.only = TRUE))
      # Print a message indicating the package was installed and loaded
      message(paste("Installed and loaded package:", package))
    }, error = function(e) {
      # Handle any errors that occur during installation or loading
      message(paste("Failed to install or load package:", package, "\nError message:", e$message))
    })
  } else {
    # Print a message indicating the package was already loaded
    message(paste("Package already loaded:", package))
  }
}

# Loop through each package in the list
for (package in packages) {
  install_and_load(package)
}

# Connect to the database
con <- dbConnect(odbc(), Driver = "SQL Server", Server = "KCITSQLPRNRPX01", Database = "GData", Trusted_Connection = "True")

# Enter start_date, end_date and optional time (yyyy-mm-dd hh:mm:ss), site codes
# can add multiple with commas
# NEED TO ADD 7 HOURS TO CONVERT TO UTC IF MATCHING FIELD VISITS
start_date <- "2024-03-25 20:00:00"
end_date <- "2024-06-10 20:30:00" #can leave blank, will pull to last record in db
rain_site_codes <- c("RPWS_RG_TOS") #also used for air temperature
WTC_site_codes <- c("RPWS_TOSMO", "RPWS_TOSMI")

#Location-based site codes; comment/uncomment to run

#RPWS-Tosh and Country
# rain_site_codes <- c("RPWS_RG_TOS")
# WTC_site_codes <- c("RPWS_TOSMI", "RPWS_TOSMO", "RPWS_COUMI", "RPWS_COUMO")

#RPWS-Monticello and Tylers
# rain_site_codes <- c("RPWS_RG_MON")
# WTC_site_codes <- c("RPWS_TYLMI", "RPWS_TYLMO", "RPWS_MONMS", "RPWS_MONMN", "RPWS_MONMO")

#RPWS-Watershed-preserve
# rain_site_codes <- c("RPWS_RG_MON") #02VN is closer but doesn't have ATC
# WTC_site_codes <- c("RPWS_COLM", "RPWS_SEIMS", "RPWS_SEIMN")

#North Lake Washington
# rain_site_codes <- c("35u")
# WTC_site_codes <- c("35c", "34a", "56e", "45c", "30an")

#Thornton Creek
# rain_site_codes <- c("35u")
# WTC_site_codes <- c("58a", "58c", "58d", "58e", "58f")

#Source reusable functions
source("my_functions.R")

# Initialize lists to store rain, instrument, and field data
rain_data_list <- retrieve_rain_data(con, start_date, end_date, rain_site_codes)
ATC_data_list <- retrieve_ATC_data(con, start_date, end_date, rain_site_codes)
WTC_data_list <- retrieve_WTC_data(con, start_date, end_date, WTC_site_codes)
field_WTC_data_list <- retrieve_field_WTC(con, start_date, end_date, WTC_site_codes)

#print(instrument_data_list)
# Initialize merged_data as NULL
merged_data <- NULL

for (WTC_site_code in WTC_site_codes) {
  WTC_data <- WTC_data_list[[WTC_site_code]]
  field_WTC_data_site <- field_WTC_data_list[[WTC_site_code]]

  # Rename columns
  colnames(WTC_data) <- c("Timestamp", paste("Raw WTC", WTC_site_code), paste("Corrected WTC", WTC_site_code))
  colnames(field_WTC_data_site) <- c("Timestamp", paste("Field WTC", WTC_site_code))
  col_name_difference <- paste("Difference", WTC_site_code)
  
  #rounding of field data to match continuous data routine
  
  # Calculate time_interval using the first time difference in WTC data
  time_interval <- as.numeric(diff(WTC_data$Timestamp)[1])
  #specify units
  time_interval <- paste(time_interval, "minutes")
  # round field WTC to match continuous
  field_WTC_data_site$Timestamp <- round_date(field_WTC_data_site$Timestamp, time_interval)
  
  # Check if WTC_data is not NULL before merging
  if (!is.null(WTC_data)) {
    # Merge WTC data into the combined data frame
    if (is.null(merged_data)) {
      merged_data <- WTC_data
    } else {
      merged_data <- merge(merged_data, WTC_data, by = "Timestamp", all.x = TRUE)
    }
  }

  # Check if field_WTC_data_site is not NULL before merging
  if (!is.null(field_WTC_data_site)) {
    # Merge field WTC data into the combined field_WTC_data
    if (is.null(merged_data)) {
      merged_data <- field_WTC_data_site
    } else {
      merged_data <- merge(merged_data, field_WTC_data_site, by = "Timestamp", all.x = TRUE)
    }
    # Calculate and add the difference column
    merged_data <- merged_data %>%
      mutate(!!col_name_difference := get(paste("Field WTC", WTC_site_code)) - get(paste("Corrected WTC", WTC_site_code)))
  }
}
 # print("Structure of merged_data after loop:")
 # print(str(merged_data))
 # print(field_WTC_data_site)
 # print(WTC_data)
# Rename rain data columns and merge using "Timestamp"
for (rain_site_code in rain_site_codes) {
  rain_data <- rain_data_list[[rain_site_code]]
  ATC_data <- ATC_data_list[[rain_site_code]]
  colnames(rain_data) <- c("Timestamp", paste("Rain", rain_site_code))
  colnames(ATC_data) <- c("Timestamp", paste("ATC", rain_site_code))
  merged_data <- merge(merged_data, rain_data, by = "Timestamp", all.x = TRUE)
  merged_data <- merge(merged_data, ATC_data, by = "Timestamp", all.x = TRUE)
}

# Interpolate ATC data to match WTC timestamps
for (rain_site_code in rain_site_codes) {
  col_name_ATC <- paste("ATC", rain_site_code)
  if (any(is.na(merged_data[[col_name_ATC]]))) {
    merged_data[[col_name_ATC]] <- na.approx(merged_data[[col_name_ATC]], x = merged_data$Timestamp, na.rm = FALSE)
  }
}

#turn data into PDT from UTC
merged_data$Timestamp <- merged_data$Timestamp - hours(7)

# Create dygraph with rain, WTC, and field WTC series
dygraph_object <- dygraph(merged_data)


# Add rain series to dygraph as bars with a specified color
for (rain_site_code in rain_site_codes) {
  col_name_rain <- paste("Rain", rain_site_code)
  col_name_ATC <- paste("ATC", rain_site_code)
  dygraph_object <- dygraph_object %>%
    dyBarSeries(col_name_rain, label = paste("Rain", rain_site_code), axis = "y2", color = "blue") %>%
  dySeries(col_name_ATC, label = paste("ATC", rain_site_code), axis = "y", drawPoints = TRUE)
}

# Add WTC and field WTC series to dygraph with automatically selected colors
unique_colors <- rainbow(length(WTC_site_codes))  # Generating unique colors for each WTC site code

for (i in seq_along(WTC_site_codes)) {
  WTC_site_code <- WTC_site_codes[i]
  col_name_raw_WTC <- paste("Raw WTC", WTC_site_code)
  col_name_corrected_WTC <- paste("Corrected WTC", WTC_site_code)
  col_name_field_WTC <- paste("Field WTC", WTC_site_code)
  col_name_difference <- paste("Difference", WTC_site_code)
  
  WTC_color <- unique_colors[i]
  
  dygraph_object <- dygraph_object %>%
    dyAxis("y", label = "Celsius") %>%
    dySeries(col_name_raw_WTC, label = paste(WTC_site_code, "Raw WTC"), axis = "y", color = WTC_color) %>%
    dySeries(col_name_corrected_WTC, label = paste(WTC_site_code, "Corrected WTC"), axis = "y", color = WTC_color) %>%
    dySeries(
      col_name_field_WTC,
      label = paste(WTC_site_code, "Field WTC"),
      axis = "y",
      drawPoints = TRUE,
      pointSize = 6,
      color = WTC_color
    ) %>%
    dySeries(
      col_name_difference,
      label = paste(WTC_site_code, "Difference"),
      axis = "y",
      drawPoints = TRUE,
      pointSize = 6,
      color = WTC_color
    )
}
# Set options to adjust legend size and position
dygraph_object <- dygraph_object %>%
  dyAxis("x", label = "Timestamp (PDT)")%>%
  dyAxis("y2", label = "Rain", valueRange = c(0.4, 0), independentTicks = TRUE)%>%
  dyLegend(show = c("onmouseover"), width = 250, labelsSeparateLines = FALSE)%>%
  dyCrosshair(direction = "both")%>%
  dyOptions(useDataTimezone = TRUE)


print(dygraph_object)


# Format start_date for file name (remove spaces and colons)
formatted_start_date <- gsub("[: ]", "_", start_date)

# Create a file name based on WTC_site_code and formatted_start_date
file_name <- paste(WTC_site_code,"WTC_Field_",formatted_start_date,".csv")

# Write CSV with the formatted file name
write.csv(merged_data, file = file_name, na = "")


# Disconnect - recommended
dbDisconnect(con)
