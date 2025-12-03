# Load needed packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI, odbc, dygraphs, lubridate, shiny, htmlwidgets, tidyverse, zoo)

# Define UI
ui <- fillPage(
  titlePanel("Water and Air Temperature + Rain Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      dateInput("start_date", "Select Start Date:"),
      dateInput("end_date", "Select End Date:"),
      selectizeInput("rain_site", "Select Rain Site Code:", choices = NULL, multiple = TRUE),
      selectizeInput("WTC_sites", "Select Water Temperature Site Codes:", choices = NULL, multiple = TRUE),
      actionButton("update_data", "Update Data"),
      downloadButton("download_dygraph", "Download Entire Dygraph as HTML"),
      downloadButton("download_csv", "Download Zoom Window Data as CSV"),
      width = 2
    ),
    mainPanel(
      dygraphOutput("dygraph", width = "83vw", height = "95vh")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Establish a persistent database connection
  con <- dbConnect(odbc(), Driver = "SQL Server", Server = "KCITSQLPRNRPX01", Database = "GData", Trusted_Connection = "True")
  onStop(function() {
    dbDisconnect(con)
    print("Database connection closed")
  })
  
  #source reusable functions
  source("myshinyfunctions.R")
  
  # get and cache site codes
  cached_rain_ATC_site_codes <- fetch_rain_ATC_site_codes(con)
  cached_WTC_site_codes <- fetch_WTC_site_codes(con)
  
  # Update the choices for rain and WTC site codes
  updateSelectizeInput(session, "rain_site", choices = cached_rain_ATC_site_codes, server = TRUE)
  updateSelectizeInput(session, "WTC_sites", choices = cached_WTC_site_codes, server = TRUE)
  
  # Reactive values to store the dygraph object and the current zoom range
  dygraph_print <- reactiveVal(NULL)
  zoom_range <- reactiveValues(start = NULL, end = NULL)
  
  observeEvent(input$update_data, {
    print("Update Data Button Clicked")
    
    start_date <- input$start_date + hours(7) # add 7 hours to convert to UTC
    end_date <- input$end_date + hours(7) # add 7 hours to convert to UTC
    rain_site_codes <- input$rain_site
    WTC_site_codes <- input$WTC_sites
    
    if (length(rain_site_codes) == 0 && length(WTC_site_codes) == 0) {
      showNotification("Please select at least one site code.", type = "error")
      return(NULL)
    }
    
    # Retrieve data
    rain_data_list <- retrieve_rain_data(con, start_date, end_date, rain_site_codes)
    ATC_data_list <- retrieve_ATC_data(con, start_date, end_date, rain_site_codes)
    WTC_data_list <- retrieve_WTC_data(con, start_date, end_date, WTC_site_codes)
    field_WTC_data_list <- retrieve_field_WTC(con, start_date, end_date, WTC_site_codes)
    
    merged_data <- NULL
    
    # Process Water Temperature Data
    for (WTC_site_code in WTC_site_codes) {
      WTC_data <- WTC_data_list[[WTC_site_code]]
      field_WTC_data_site <- field_WTC_data_list[[WTC_site_code]]
      
      if (!is.null(WTC_data)) {
        colnames(WTC_data) <- c("Timestamp", paste("Raw WTC", WTC_site_code), paste("Corrected WTC", WTC_site_code))
        if (is.null(merged_data)) {
          merged_data <- WTC_data
        } else {
          merged_data <- merge(merged_data, WTC_data, by = "Timestamp", all.x = TRUE)
        }
      }
      
      if (!is.null(field_WTC_data_site)) {
        colnames(field_WTC_data_site) <- c("Timestamp", paste("Field WTC", WTC_site_code))
        time_interval <- paste(as.numeric(diff(WTC_data$Timestamp)[1]), "minutes")
        field_WTC_data_site$Timestamp <- round_date(field_WTC_data_site$Timestamp, time_interval)
        merged_data <- merge(merged_data, field_WTC_data_site, by = "Timestamp", all.x = TRUE)
        merged_data <- merged_data %>%
          mutate(!!paste("Difference", WTC_site_code) := get(paste("Field WTC", WTC_site_code)) - get(paste("Corrected WTC", WTC_site_code)))
      } else {
        warning(paste("Field WTC data is not available for site_code =", WTC_site_code))
      }
      
      # Interpolate Raw and Corrected WTC data if there are missing values
      if (!is.null(WTC_data)) {
        colname_raw_wtc <- paste("Raw WTC", WTC_site_code)
        colname_corrected_wtc <- paste("Corrected WTC", WTC_site_code)
        merged_data[[colname_raw_wtc]] <- na.approx(merged_data[[colname_raw_wtc]], x = merged_data$Timestamp, na.rm = FALSE)
        merged_data[[colname_corrected_wtc]] <- na.approx(merged_data[[colname_corrected_wtc]], x = merged_data$Timestamp, na.rm = FALSE)
      }
    }
    
    # Process and merge Rain Data
    for (rain_site_code in rain_site_codes) {
      rain_data <- rain_data_list[[rain_site_code]]
      ATC_data <- ATC_data_list[[rain_site_code]]
      
      if (!is.null(rain_data)) {
        colnames(rain_data) <- c("Timestamp", paste("Rain", rain_site_code))
        merged_data <- merge(merged_data, rain_data, by = "Timestamp", all.x = TRUE)
      }
      
      if (!is.null(ATC_data)) {
        colnames(ATC_data) <- c("Timestamp", paste("ATC", rain_site_code))
        merged_data <- merge(merged_data, ATC_data, by = "Timestamp", all.x = TRUE)
        merged_data[[paste("ATC", rain_site_code)]] <- na.approx(merged_data[[paste("ATC", rain_site_code)]], x = merged_data$Timestamp, na.rm = FALSE)
      }
    }
    #warn if merged data is null and return null
    if (is.null(merged_data)) {
      showNotification("No data available for the selected date range and site codes.", type = "warning")
      return(NULL)
    }
    
    #subtract 7 hours to put in PDT
    merged_data$Timestamp <- merged_data$Timestamp - hours(7)
    
    #create dygraph with merged data
    dygraph_object <- dygraph(merged_data) %>% 
    dyRangeSelector()
    
    unique_colors <- rainbow(length(WTC_site_codes))
    
    for (i in seq_along(WTC_site_codes)) {
      WTC_site_code <- WTC_site_codes[i]
      dygraph_object <- dygraph_object %>%
        dySeries(paste("Raw WTC", WTC_site_code), label = paste(WTC_site_code, "Raw WTC"), axis = "y", color = unique_colors[i]) %>%
        dySeries(paste("Corrected WTC", WTC_site_code), label = paste(WTC_site_code, "Corrected WTC"), axis = "y", color = unique_colors[i]) %>%
        dySeries(paste("Field WTC", WTC_site_code), label = paste(WTC_site_code, "Field WTC"), axis = "y", drawPoints = TRUE, pointSize = 6, color = unique_colors[i]) %>%
        dySeries(paste("Difference", WTC_site_code), label = paste(WTC_site_code, "Difference"), axis = "y", drawPoints = TRUE, pointSize = 6, color = unique_colors[i])
    }
    
    for (rain_site_code in rain_site_codes) {
      dygraph_object <- dygraph_object %>%
        dyBarSeries(paste("Rain", rain_site_code), label = paste("Rain", rain_site_code), axis = "y2", color = "blue") %>%
        dySeries(paste("ATC", rain_site_code), label = paste("ATC", rain_site_code), axis = "y", drawPoints = TRUE)
    }
    
    dygraph_object <- dygraph_object %>%
      dyAxis("y", label = "Celsius") %>%
      dyAxis("x", label = "Timestamp (PDT)") %>%
      dyAxis("y2", label = "Rain", valueRange = c(0.4, 0), independentTicks = TRUE) %>%
      dyLegend(show = c("onmouseover"), width = 250, labelsSeparateLines = FALSE) %>%
      dyCrosshair(direction = "both") %>%
      dyOptions(useDataTimezone = TRUE)%>%
    dyCallbacks(zoomCallback = JS("function(minDate, maxDate, yRanges) {
        Shiny.onInputChange('dygraph_date_window', [minDate, maxDate]);
      }"))
    
    print("Data Processing Completed")
  
    # Store the dygraph object in the reactive value
    dygraph_print(dygraph_object)
    
    # Render dygraph
    output$dygraph <- renderDygraph({
      print("Rendering Dygraph")
      dygraph_print()
    })
  })

  # Handle dygraph HTML download
  output$download_dygraph <- downloadHandler(
    filename = function() {
      paste0("dygraph_", paste(input$WTC_sites, collapse = "_"), ".html")
    },
    content = function(file) {
      saveWidget(dygraph_print(), file)
    }
  )
  #str(dygraph_print)
  # Observe the dygraph zoom range
  observeEvent(input$dygraph_date_window, {
    zoom_range$start <- input$dygraph_date_window[[1]]
    zoom_range$end <- input$dygraph_date_window[[2]]
  })
  
  # Download handler for dygraph data as CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("WTC_data_", paste(input$WTC_sites, collapse = "_"), ".csv")
    },
    content = function(file) {
      print("Attempting to download CSV...")
      
      if (is.null(dygraph_print())) {
        print("dygraph_print() is NULL")
        showNotification("No data available for download.", type = "warning")
      } else {
        print("dygraph_print() is not NULL")
        dygraph_object <- dygraph_print()  # Get the dygraph object
        
        if (!is.null(dygraph_object$x$attrs$labels)) {
          # Extract labels from dygraph object
          labels <- dygraph_object$x$attrs$labels
          
          # Get data from dygraph object
          dygraph_data <- dygraph_object$x$data
          
          if (!is.null(dygraph_data) && length(dygraph_data) > 0) {
            # Convert data to data frame
            dygraph_df <- as.data.frame(do.call(cbind, dygraph_data))
            
            # Set column names based on indexed labels
            colnames(dygraph_df) <- c("Timestamp", labels[-1])  # Exclude first label "minute"
            
            # Reformat Timestamp column to UTC
            dygraph_df$Timestamp <- as.POSIXct(dygraph_df$Timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
            
            # Optionally filter data based on zoom range
            zoom_start <- as.POSIXct(as.numeric(zoom_range$start) / 1000, origin = "1970-01-01", tz = "UTC")
            zoom_end <- as.POSIXct(as.numeric(zoom_range$end) / 1000, origin = "1970-01-01", tz = "UTC")
            
            if (!is.na(zoom_start) && !is.na(zoom_end) && zoom_end > zoom_start) {
              dygraph_df <- dygraph_df %>%
                filter(Timestamp >= zoom_start & Timestamp <= zoom_end)
            }
            
            # Convert Timestamp column to the desired timezone (e.g., PST)
            dygraph_df$Timestamp <- format(dygraph_df$Timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
            
            # Write CSV file
            write.csv(dygraph_df, file, row.names = FALSE, na = "")
            print("CSV download complete.")
          } else {
            print("No data available for download.")
            showNotification("No data available for download.", type = "warning")
          }
        } else {
          print("No labels available for column names.")
          showNotification("No labels available for column names.", type = "warning")
        }
      }
    }
  )
  
}
# Run the application
shinyApp(ui = ui, server = server)
