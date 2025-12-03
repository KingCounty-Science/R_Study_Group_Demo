# Load the tidyverse package
library(tidyverse)
library(zoo)
# Function to fill missing timestamps with the last available value
fill_missing_timestamps <- function(data) {
  # Identify missing timestamps
  missing_timestamps <- setdiff(seq(min(data$D_TimeDate), max(data$D_TimeDate), by = "5 min"), data$D_TimeDate)
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


# Function to retrieve rain data
retrieve_rain_data <- function(con, start_date, end_date, rain_site_codes) {
  data_list <- list()
  for (site_code in rain_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.R_TimeDate, t1.R_VALUE
                FROM tblRainGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.Precip = 1
                AND t1.R_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
    } else {
      query <- "SELECT t1.R_TimeDate, t1.R_VALUE
                FROM tblRainGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.Precip = 1
                AND t1.R_TimeDate >= ?
                AND t1.R_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    if (nrow(result) > 0) {
      # Convert Unix timestamps to POSIXct format
      result$R_TimeDate <- as.POSIXct(result$R_TimeDate, tz = "UTC", origin = "1970-01-01", format = "%Y-%m-%dT%H:%M:%S")
      data_list[[site_code]] <- result
    } else {
      cat("Number of rows in result for site_code", site_code, ":", nrow(result), "\n")
      data_list[[site_code]] <- result
    }
  }
  return(data_list)
}

# Function to retrieve discharge data
retrieve_discharge_data <- function(con, start_date, end_date, discharge_site_codes) {
  data_list <- list()
  for (site_code in discharge_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.D_TimeDate, t1.D_Discharge
                FROM tblDischargeGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.FlowLevel = 1
                AND t1.D_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
    } else {
      query <- "SELECT t1.D_TimeDate, t1.D_Discharge
                FROM tblDischargeGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t2.FlowLevel = 1
                AND t1.D_TimeDate >= ?
                AND t1.D_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    if (nrow(result) > 0) {
      # Convert Unix timestamps to POSIXct format
      result$D_TimeDate <- as.POSIXct(result$D_TimeDate, tz = "UTC", origin = "1970-01-01", format = "%Y-%m-%dT%H:%M:%S")
      # Fill missing values
      #result$D_Discharge <- fill_missing_timestamps(result$D_Discharge)
      data_list[[site_code]] <- result
    } else {
      cat("Warning: Discharge data is not available for site_code =", site_code, "\n")
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
      result$Date_Time <- round_date(result$Date_Time, "5 minutes")
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
      # result <- fill_missing_timestamps(result)
      # 
      # # Print the timestamps after filling
      # cat("Timestamps after filling:")
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
      result$Date_Time <- round_date(result$Date_Time, "15 minutes")
      data_list[[site_code]] <- result
    } else {
      cat("Warning: Field Discharge data is not available for site_code =", site_code, "\n")
    }
  }
  return(data_list)
}

# Function to retrieve Water temperature data
retrieve_WTC_data <- function(con, start_date, end_date, WTC_site_codes) {
  data_list <- list()
  for (site_code in WTC_site_codes) {
    if (end_date == "") {
      query <- "SELECT t1.W_TimeDate, t1.W_Value, t1.W_ValueCorrected
                FROM tblWaterTempGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.W_TimeDate >= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
    } else {
      query <- "SELECT t1.W_TimeDate, t1.W_Value, t1.W_ValueCorrected
                FROM tblWaterTempGauging AS t1
                INNER JOIN tblGaugeLLID AS t2 ON t1.G_ID = t2.G_ID
                WHERE t2.status = 'Active'
                AND t1.W_TimeDate >= ?
                AND t1.W_TimeDate <= ?
                AND t2.SITE_CODE = ?"
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    if (nrow(result) > 0) {
      # Fill missing values
      #result$L_Level <- fill_missing_values(result$L_Level)
      data_list[[site_code]] <- result
      # Print the structure of the result data frame
      str(result)
    } else {
      cat("Warning: WTC data is not available for site_code =", site_code, "\n")
    }
  }
  return(data_list)
}

retrieve_field_WTC <- function(con, start_date, end_date, WTC_site_codes) {
  data_list <- list()  # Create a list to store the data for each combination
  for (site_code in WTC_site_codes) {
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
      result <- dbGetQuery(con, query, params = list(start_date, site_code))
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
      result <- dbGetQuery(con, query, params = list(start_date, end_date, site_code))
    }
    if (nrow(result) > 0) {
      # Ensure the Date_Time column is in POSIXct format
      result$Date_Time <- as.POSIXct(result$Date_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      # Round the Date_Time column to the nearest 15-minute (or 5 - need to change) interval
      result$Date_Time <- round_date(result$Date_Time, "15 minutes")
      data_list[[site_code]] <- result
      cat("Number of rows in result for site_code", site_code, ":", nrow(result), "\n")
    } else {
      cat("Warning: Field WTC data is not available for site_code =", site_code, "\n")
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