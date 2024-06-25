# Define the list of packages and their versions you need
packages <- c('tidyverse', 'zoo')

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


# Function to fill missing D_TimeDate timestamps with the last available value
# Work in progress - only called in instrument workup code and retrieve_instrument_data function as of 3/22/2024
fill_missing_timestamps <- function(data) {
  # Identify missing timestamps - uses first difference in timedates as interval
  missing_timestamps <- setdiff(seq(min(data$D_TimeDate), max(data$D_TimeDate), by = diff(data$D_TimeDate)[1]), data$D_TimeDate)
  
  # Use lapply to create a list of data frames
  filled_data_list <- lapply(missing_timestamps, function(timestamp) {
    # Extract the last available value before the current timestamp
    last_value <- tail(data$D_Value[data$D_TimeDate < timestamp], 1)
    
    # Create a new row with the missing timestamp and the corresponding last available value
    new_row <- data.frame(D_TimeDate = timestamp, D_Value = ifelse(is.na(last_value), NA, last_value))
    
    return(new_row)
  })
  
  
  
  # Combine the list of data frames into a single data frame
  filled_data <- do.call(rbind, filled_data_list)
  
  # Combine the filled_data with the original data
  data <- rbind(data, filled_data)
  
  # Order the data by timestamp
  data <- data[order(data$D_TimeDate), ]
  
  return(data)
}

#function to fill missing WTC timestamps
fill_missing_WTC_timestamps <- function(data) {
  if (nrow(data) < 2) {
    # If there are less than two rows, return the data as is
    return(data)
  }
  
  # Ensure data is sorted by timestamp
  data <- data[order(data$W_TimeDate), ]
  
  # Calculate the time interval based on the first two timestamps
  time_interval <- as.numeric(difftime(data$W_TimeDate[2], data$W_TimeDate[1], units = "mins"))
  
  if (time_interval <= 0) {
    stop("Error: Invalid time interval calculated.")
  }
  
  # Identify missing timestamps based on the time interval
  start_timestamp <- min(data$W_TimeDate)
  end_timestamp <- max(data$W_TimeDate)
  all_timestamps <- seq.POSIXt(start_timestamp, end_timestamp, by = paste(time_interval, "min"))
  
  # Check for missing timestamps
  missing_timestamps <- setdiff(all_timestamps, data$W_TimeDate)
  
  if (length(missing_timestamps) == 0) {
    return(data)  # No missing timestamps, return the original data
  }
  
  # Use lapply to create a list of data frames
  filled_data_list <- lapply(missing_timestamps, function(timestamp) {
    # Extract the last available value before the current timestamp for W_Value
    last_W_value <- tail(data$W_Value[data$W_TimeDate < timestamp], 1)
    
    # Extract the last available value before the current timestamp for W_ValueCorrected
    last_W_valueCorrected <- tail(data$W_ValueCorrected[data$W_TimeDate < timestamp], 1)
    
    # Create a new row with the missing timestamp and the corresponding last available values
    new_row <- data.frame(
      W_TimeDate = timestamp,
      W_Value = ifelse(length(last_W_value) == 0, NA, last_W_value),
      W_ValueCorrected = ifelse(length(last_W_valueCorrected) == 0, NA, last_W_valueCorrected)
    )
    
    return(new_row)
  })
  
  # Combine the list of data frames into a single data frame
  filled_data <- do.call(rbind, filled_data_list)
  
  # Combine the filled_data with the original data
  data <- rbind(data, filled_data)
  
  # Order the data by timestamp
  data <- data[order(data$W_TimeDate), ]
  
  return(data)
}


# Function to fill missing WTC W_TimeDate timestamps with the last available value
fill_missing_corrected_WTC_timestamps <- function(data) {
  # Identify missing timestamps - uses first difference in timedates as interval
  missing_timestamps <- setdiff(seq(min(data$W_TimeDate), max(data$W_TimeDate), by = diff(data$W_TimeDate)[1]), data$W_TimeDate)
  
  # Use lapply to create a list of data frames
  filled_data_list <- lapply(missing_timestamps, function(timestamp) {
    # Extract the last available value before the current timestamp for W_Value
   # last_W_value <- tail(data$W_Value[data$W_TimeDate < timestamp], 1)
    
    # Extract the last available value before the current timestamp for W_ValueCorrected
    last_W_valueCorrected <- tail(data$W_ValueCorrected[data$W_TimeDate < timestamp], 1)
    
    # Create a new row with the missing timestamp and the corresponding last available values
    new_row <- data.frame(
      W_TimeDate = timestamp,
     # W_Value = ifelse(is.na(last_W_value), NA, last_W_value),
      W_ValueCorrected = ifelse(is.na(last_W_valueCorrected), NA, last_W_valueCorrected)
    )
    
    return(new_row)
  })
  
  # Combine the list of data frames into a single data frame
  filled_data <- do.call(rbind, filled_data_list)
  
  # Combine the filled_data with the original data
  data <- rbind(data, filled_data)
  
  # Order the data by timestamp
  data <- data[order(data$W_TimeDate), ]
  
  return(data)
}

# Function to retrieve rain data
retrieve_rain_data <- function(con, start_date, end_date, rain_site_codes) {
  data_list <- list()
  str(data_list)
  for (rain_site_code in rain_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.R_TimeDate, t1.R_VALUE
                FROM tblRainGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.Precip = 1
                AND t1.R_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, rain_site_code))
    } else {
      query <- "SELECT t1.R_TimeDate, t1.R_VALUE
                FROM tblRainGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.Precip = 1
                AND t1.R_TimeDate >= ?
                AND t1.R_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, rain_site_code))
    }
    if (nrow(result) > 0) {
      # Convert Unix timestamps to POSIXct format
      result$R_TimeDate <- as.POSIXct(result$R_TimeDate, tz = "UTC", origin = "1970-01-01", format = "%Y-%m-%dT%H:%M:%S")
      
      data_list[[rain_site_code]] <- result
    } else {
      cat("Warning: Rain data is not available for site_code =", rain_site_code, "\n")
      data_list[[rain_site_code]] <- result
    }
}
  
return(data_list)
}

# Function to retrieve air temperature data using rain_site_code
retrieve_ATC_data <- function(con, start_date, end_date, rain_site_codes) {
  data_list <- list()
  str(data_list)
  for (rain_site_code in rain_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.A_TimeDate, t1.A_VALUE
                FROM tblAirTempGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.AirTemp = 1
                AND t1.A_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, rain_site_code))
    } else {
      query <- "SELECT t1.A_TimeDate, t1.A_VALUE
                FROM tblAirTempGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.AirTemp = 1
                AND t1.A_TimeDate >= ?
                AND t1.A_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, rain_site_code))
    }
    if (nrow(result) > 0) {
      # Convert Unix timestamps to POSIXct format
      result$A_TimeDate <- as.POSIXct(result$A_TimeDate, tz = "UTC", origin = "1970-01-01", format = "%Y-%m-%dT%H:%M:%S")
      
      data_list[[rain_site_code]] <- result
    } else {
      cat("Warning: Air temperature data is not available for site_code =", rain_site_code, "\n")
      data_list[[rain_site_code]] <- result
    }
  }
  
  return(data_list)
}


# Function to retrieve discharge data with updated error handling
retrieve_discharge_data <- function(con, start_date, end_date, discharge_site_codes) {
  data_list <- list()
  
  for (discharge_site_code in discharge_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.D_TimeDate, t1.D_Discharge
                FROM tblDischargeGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.FlowLevel = 1
                AND t1.D_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, discharge_site_code))
    } else {
      query <- "SELECT t1.D_TimeDate, t1.D_Discharge
                FROM tblDischargeGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.FlowLevel = 1
                AND t1.D_TimeDate >= ?
                AND t1.D_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, discharge_site_code))
    }
    
    if (nrow(result) > 0) {
      # Convert Unix timestamps to POSIXct format
      result$D_TimeDate <- as.POSIXct(result$D_TimeDate, tz = "UTC", origin = "1970-01-01", format = "%Y-%m-%dT%H:%M:%S")
      # Fill missing values
      #result$D_Discharge <- fill_missing_timestamps(result$D_Discharge)
      data_list[[discharge_site_code]] <- result
    } else {
      cat("Warning: Discharge data is not available for site_code =", discharge_site_code, "\n")
    }
  }
  
  return(data_list)
}


# Function to retrieve lake level data
retrieve_lake_level_data <- function(con, start_date, end_date, water_level_site_codes) {
  data_list <- list()
  
  for (site_code in water_level_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.L_TimeDate, t1.L_Level
                FROM tblLakeLevelGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.L_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
      print(result)
    } else {
      query <- "SELECT t1.L_TimeDate, t1.L_Level
                FROM tblLakeLevelGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.L_TimeDate >= ?
                AND t1.L_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    
    if (nrow(result) > 0) {
      # Fill missing values
      #result$L_Level <- fill_missing_values(result$L_Level)
      data_list[[site_code]] <- result
    } else {
      cat("Warning: Lake level data is not available for site_code =", site_code, "\n")
    }
  }
  
  return(data_list)
}

retrieve_field_discharge <- function(con, start_date, end_date, discharge_site_codes) {
  data_list <- list()  # Create a list to store the data for each combination
  
  for (site_code in discharge_site_codes) {
    if (end_date == "") {
      # If end date is empty, retrieve all data after the start date
      query <- "SELECT t1.Date_Time, t2.Parameter_Value
                FROM tblFieldVisitInfo AS t1
                INNER JOIN tblFieldData AS t2 ON t1.FieldVisit_ID = t2.FieldVisit_ID
                INNER JOIN tblGaugeLLID AS t3 ON t2.G_ID = t3.G_ID
                WHERE t3.status = 'Active'
                AND t2.Parameter = '2'
                AND t1.Measurement_Number IS NOT NULL
                AND t1.Date_Time >= ?
                AND t3.SITE_CODE = ?
                "
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
    } else {
      # If end date is specified, use it in the query
      query <- "SELECT t1.Date_Time, t2.Parameter_Value
                FROM tblFieldVisitInfo AS t1
                INNER JOIN tblFieldData AS t2 ON t1.FieldVisit_ID = t2.FieldVisit_ID
                INNER JOIN tblGaugeLLID AS t3 ON t2.G_ID = t3.G_ID
                WHERE t3.status = 'Active'
                AND t2.Parameter = '2'
                AND t1.Measurement_Number IS NOT NULL
                AND t1.Date_Time >= ?
                AND t1.Date_Time <= ?
                AND t3.SITE_CODE = ?
                "
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    
    if (nrow(result) > 0) {
      # Ensure the Date_Time column is in POSIXct format
      result$Date_Time <- as.POSIXct(result$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      
      # Round the Date_Time column to the nearest 15-minute (or 5 - need to change) interval
      # Moved rounding to dynamically select based on continuous data in main code but leaving for reference
      #result$Date_Time <- round_date(result$Date_Time, "5 minutes")
      
      data_list[[site_code]] <- result
    } else {
      cat("Warning: Field Discharge data is not available for site_code =", site_code, "\n")
    }
  }
  
  return(data_list)
}

# Function to retrieve instrument data with missing timestamp handling
retrieve_instrument_data <- function(con, start_date, end_date, discharge_site_codes) {
  data_list <- list()
  
  for (site_code in discharge_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.D_TimeDate, t1.D_Value
                FROM tblDischargeGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.FlowLevel = 1
                AND t1.D_TimeDate >= ?
                AND t2.SITE_CODE = ?
                ORDER BY t1.D_TimeDate"  # Ensure data is ordered by timestamp
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
    } else {
      query <- "SELECT t1.D_TimeDate, t1.D_Value
                FROM tblDischargeGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.FlowLevel = 1
                AND t1.D_TimeDate >= ?
                AND t1.D_TimeDate <= ?
                AND t2.SITE_CODE = ?
                ORDER BY t1.D_TimeDate"  # Ensure data is ordered by timestamp
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    
    if (nrow(result) > 0) {
      # Convert Unix timestamps to POSIXct format
      result$D_TimeDate <- as.POSIXct(result$D_TimeDate, tz = "UTC", origin = "1970-01-01", format = "%Y-%m-%dT%H:%M:%S")
      
      # # Print the structure of the result data frame
      # str(result)
      # 
      # # Print the timestamps before filling
      # cat("Timestamps before filling:")
      # print(result$D_TimeDate)
      # 
      # # Fill missing timestamps with last available D_Value
       result <- fill_missing_timestamps(result)
      # 
      # # Print the timestamps after filling
       #cat("Timestamps after filling:")
      # print(result$D_TimeDate)
      
      data_list[[site_code]] <- result
    } else {
      cat("Warning: Instrument data is not available for site_code =", site_code, "\n")
    }
  }
  return(data_list)
}




retrieve_field_WL <- function(con, start_date, end_date, discharge_site_codes) {
  data_list <- list()  # Create a list to store the data for each combination
  
  for (site_code in discharge_site_codes) {
    if (end_date == "") {
      # If end date is empty, retrieve all data after the start date
      query <- "SELECT t1.Date_Time, t1.Stage_Feet
                FROM tblFieldVisitInfo AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.Date_Time >= ?
                AND t2.SITE_CODE = ?
                "
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
    } else {
      # If end date is specified, use it in the query
      query <- "SELECT t1.Date_Time, t1.Stage_Feet
                FROM tblFieldVisitInfo AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.Date_Time >= ?
                AND t1.Date_Time <= ?
                AND t2.SITE_CODE = ?
                "
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    
    if (nrow(result) > 0) {
      # Ensure the Date_Time column is in POSIXct format
      result$Date_Time <- as.POSIXct(result$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      
      # Round the Date_Time column to the nearest 15-minute (or 5 - need to change) interval
      # Moved rounding to dynamically select based on continuous data in main code but leaving for reference
      #result$Date_Time <- round_date(result$Date_Time, "5 minutes")
      
      data_list[[site_code]] <- result
    } else {
      cat("Warning: Field Discharge data is not available for site_code =", site_code, "\n")
    }
  }
  
  return(data_list)
}

retrieve_WTC_data <- function(con, start_date, end_date, WTC_site_codes) {
  data_list <- list()
  
  for (WTC_site_code in WTC_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.W_TimeDate, t1.W_Value, t1.W_ValueCorrected
                FROM tblWaterTempGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.W_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, WTC_site_code))
    } else {
      query <- "SELECT t1.W_TimeDate, t1.W_Value, t1.W_ValueCorrected
                FROM tblWaterTempGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.W_TimeDate >= ?
                AND t1.W_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, WTC_site_code))
    }
    
    if (nrow(result) > 0) {
      
      # Ensure the data is sorted by timestamp
      result <- result[order(result$W_TimeDate), ]
      
      # Fill missing timestamps
      result <- fill_missing_WTC_timestamps(result)
      
      data_list[[WTC_site_code]] <- result
      # Print the structure of the result data frame
      str(result)
    } else {
      cat("Warning: WTC data is not available for site_code =", WTC_site_code, "\n")
    }
  }
  
  return(data_list)
}

retrieve_field_WTC <- function(con, start_date, end_date, WTC_site_codes) {
  data_list <- list()  # Create a list to store the data for each combination
  
  for (WTC_site_code in WTC_site_codes) {
    if (end_date == "") {
      # If end date is empty, retrieve all data after the start date
      query <- "SELECT t1.Date_Time, t2.Parameter_Value
                FROM tblFieldVisitInfo AS t1
                INNER JOIN tblFieldData AS t2 ON t1.FieldVisit_ID = t2.FieldVisit_ID
                INNER JOIN tblGaugeLLID AS t3 ON t2.G_ID = t3.G_ID
                WHERE t3.status = 'Active'
                AND t2.Parameter = '3'
                AND t1.Date_Time >= ?
                AND t3.SITE_CODE = ?
                "
      result <- dbGetQuery(con, query, params = list(start_date, WTC_site_code))
    } else {
      # If end date is specified, use it in the query
      query <- "SELECT t1.Date_Time, t2.Parameter_Value
                FROM tblFieldVisitInfo AS t1
                INNER JOIN tblFieldData AS t2 ON t1.FieldVisit_ID = t2.FieldVisit_ID
                INNER JOIN tblGaugeLLID AS t3 ON t2.G_ID = t3.G_ID
                WHERE t3.status = 'Active'
                AND t2.Parameter = '3'
                AND t1.Date_Time >= ?
                AND t1.Date_Time <= ?
                AND t3.SITE_CODE = ?
                "
      result <- dbGetQuery(con, query, params = list(start_date, end_date, WTC_site_code))
    }
    
    if (nrow(result) > 0) {
      # Ensure the Date_Time column is in POSIXct format
      result$Date_Time <- as.POSIXct(result$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      
      # Round the Date_Time column to the nearest 15-minute (or 5 - need to change) interval
      # Moved rounding to dynamically select based on continuous data in main code but leaving for reference
      #result$Date_Time <- round_date(result$Date_Time, "5 minutes")
      
      data_list[[WTC_site_code]] <- result
      cat("Number of rows in result for site_code", WTC_site_code, ":", nrow(result), "\n")
    } else {
      cat("Warning: Field WTC data is not available for site_code =", WTC_site_code, "\n")
    }
  }
  
  return(data_list)
}

# Function to retrieve well level data
retrieve_well_level_data <- function(con, start_date, end_date, well_level_site_codes) {
  data_list <- list()
  
  for (site_code in well_level_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.P_TimeDate, t1.P_Level
                FROM tblPiezometerGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.P_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
      #print(result)
    } else {
      query <- "SELECT t1.P_TimeDate, t1.P_Level
                FROM tblPiezometerGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.P_TimeDate >= ?
                AND t1.P_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    
    if (nrow(result) > 0) {
      # Fill missing values
      #result$P_Level <- fill_missing_values(result$P_Level)
      data_list[[site_code]] <- result
    } else {
      cat("Warning: Well level data is not available for site_code =", site_code, "\n")
    }
  }
  
  return(data_list)
}

# Function to filter data from socrata db based on selected site_ids and datetime ranges
filter_data <- function(data, start_date, end_date, discharge_site_codes) {
  data_list <- list()
  
  for (site_code in discharge_site_codes) {
    filtered_data <- data %>%
      filter(site_id == site_code,
             datetime >= as.POSIXct(start_date),
             datetime <= as.POSIXct(end_date),
             parameter == "discharge") %>%
      select(datetime, corrected_data)
    
    if (nrow(filtered_data) > 0) {
      data_list[[site_code]] <- filtered_data
    } else {
      cat("Warning: Data is not available for site_code =", site_code, "\n")
    }
  }
  
  return(data_list)
}

# Function to retrieve only corrected Water temperature data
retrieve_corrected_WTC_data <- function(con, start_date, end_date, WTC_site_codes) {
  data_list <- list()
  
  for (WTC_site_code in WTC_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.W_TimeDate, t1.W_ValueCorrected
                FROM tblWaterTempGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.W_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, WTC_site_code))
    } else {
      query <- "SELECT t1.W_TimeDate, t1.W_ValueCorrected
                FROM tblWaterTempGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.W_TimeDate >= ?
                AND t1.W_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, WTC_site_code))
    }
    
    if (nrow(result) > 0) {
      # Fill missing values
      #result$L_Level <- fill_missing_values(result$L_Level)
      
      # # Fill missing timestamps with last available W_Value
     # result <- fill_missing_corrected_WTC_timestamps(result)
      data_list[[WTC_site_code]] <- result
      # Print the structure of the result data frame
      str(result)
    } else {
      cat("Warning: WTC data is not available for site_code =", WTC_site_code, "\n")
    }
  }
  
  return(data_list)
}