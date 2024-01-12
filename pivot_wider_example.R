# This script is designed to do the following
# 1. Read in data from LIMS
# 2. Grab the same data from the Portal (if it exists!)
# 3. Plot data from all parameters
#     - Scatterplot of each date (y = depth, x = param), with lines connecting each depth point for a given date
#     - Latest profile is highlighted
#     - Last 2 profiles are highlighted to a lesser degree

rm(list = ls())
library(tidyverse)
library(readr)
library(lubridate)
library(here)
library(svDialogs)
library(svDialogs)


# Setup -------------------------------------------------------------------

# Downloads new data from the portal and writes it to temporary_parms_download.csv
portal_dl <- dlgInput("Do you want to download new portal data? Takes about ~4 mins (y/n", "n")$res
if(portal_dl %in% c("yes", "y", "Yes", "YES", "Y")){
  source(here("portal_downloader.R"))
}

# Pick your station 
station <- dlgInput("Enter your station", "KSBP01")$res

# Pick your month
month_of_interest <- dlgInput("Enter the month, in numerical format (e.g. Jan = 1)", "1")$res

# Load in LIMS Data ---------------------------------------------------------------

LIMS_file <- choose.files("Select LIMS Report",
                          default = paste0(here("Data")))

LIMS_report <- read_delim(LIMS_file, 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE) %>%
  mutate(date = as.Date(mdy_hms(COLLECTDATE)),
         datetime = mdy_hms(COLLECTDATE),
         month = month(date),
         year = as.factor(year(date)))

# Modify LIMS DMPA format to tidy format
LIMS_wide <- LIMS_report %>%
  pivot_wider(names_from = c(PARMNAME, UNITS),
              values_from = c(NUMVALUE, QUALIFIER),
              id_cols = c(date, datetime, SITE, LOCATOR, SAMPLE_DEPTH, LABSAMPLENUM)) %>%
  mutate(year = as.factor(year(date)),
         month = as.factor(month(date)))

# This is the working data
LIMS_working_data <- LIMS_wide %>%
  filter(month == month_of_interest,
         LOCATOR == station)


# TEST - CHECKING IF WIDENING FUNCTIONS WORKED  ----------------------------
# Values of zero mean the LIMS long and LIMS wide format have the same amount of data and are equivalent

for(i in seq(7,30,1)){
  num <- i
  test_doi_long <- LIMS_report %>%
    filter(month == month_of_interest,
           LOCATOR == station,
           PARMNAME == unique(LIMS_report$PARMNAME)[num - 6],
           !is.na(NUMVALUE)) %>%
    arrange(by = date) %>%
    select(NUMVALUE, everything())
  
  test_doi_wide <- LIMS_wide %>%
    filter(month == month_of_interest,
           LOCATOR == station,
           !is.na(eval(as.symbol(colnames(LIMS_wide)[num])))) %>%
    arrange(by = date) %>%
    select(num, everything())
  
  
  vector_long <- test_doi_wide[1]
  vector_wide <- test_doi_long$NUMVALUE
  vector_diff = vector_long - vector_wide
  print(vector_diff)
}



# Plot LIMS Data ---------------------------------------------------------------

Enterococcus <- ggplot(test)+
  theme_bw() + 
  theme(legend.position = "none", 
        panel.spacing.y = unit(-0.2, "lines")) + 
  labs(x = "Depth (m)", 
       y = "Enterococcus CFU/100 mL") + 
  coord_flip() + 
  geom_point(aes(x = SAMPLE_DEPTH, 
                 y = `NUMVALUE_Enterococcus_CFU/100ml`)) +
  geom_smooth(aes(x = SAMPLE_DEPTH, 
                 y = `NUMVALUE_Enterococcus_CFU/100ml`)) +
  scale_x_reverse()
Enterococcus

Fecal_Coliform


Density


Dissolved_Organic_Carbon


Dissolved_Oxygen


Dissolved_Oxygen_Field
pH_Field
Salinity
Salinity_Field
Temperature
Total_Organic_Carbon
Total_Suspended_Solids
Light_Intensity_(PAR)
Light_Transmissivity
Secchi_Transparency
Surface_Light_Intensity_(PAR)
Ammonia_Nitrogen
Nitrite_Nitrate
Orthophosphate_Phosphorus
Silica
Total_Nitrogen
Chlorophyll_a
Chlorophyll_Field
Pheophytin_a


p <- ggplot(data = discrete_data$data %>% 
              mutate(Locator = factor(Locator, 
                                      levels = locators_cb), 
                     Shape = case_when(QualityID == 4 ~ "Bad", 
                                       NonDetect == TRUE ~ "ND", 
                                       TRUE ~ "Regular")) %>% 
              filter(Parameter == input$parm_2, 
                     Locator %in% locators_cb, 
                     !is.na(Depth)) %>% 
              {if (input$include_bad) . else filter(., QualityID != 4)}) + 
  theme_bw() + 
  theme(legend.position = "none", 
        panel.spacing.y = unit(-0.2, "lines")) + 
  labs(x = "Depth (m)", 
       y = parm_units$Label[parm_units$Parameter == input$parm_2]) + 
  coord_flip() + 
  geom_point(aes(x = Depth, y = Value, 
                 color = WeekDate == input$dates_2, 
                 shape = Shape, 
                 customdata = URL, 
                 text = paste0(Value, "; ", CollectDate))) + 
  facet_wrap(~ Locator, ncol = 3, scales = "free_y") + 
  scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                "FALSE" = alpha("black", 0.2))) + 
  scale_shape_manual(values = c("Bad" = 15, 
                                "ND" = 6, 
                                "Regular" = 16))
  



# Load in Portal Data -------------------------------------------------------------
if(portal_dl %in% c("yes", "y", "Yes", "YES", "Y")){
  portal_file <- here("temporary_parms_download.csv")
} else{
  portal_file <- choose.files("Select Portal Download",
                              default = here("temporary_parms_download.csv"))
}
portal_report <- read_csv(portal_file) 


# Taylor's code from the Shiny App ----------------------------------------

output$plot_cb <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                mutate(Locator = factor(Locator, 
                                        levels = locators_cb), 
                       Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                filter(Parameter == input$parm_2, 
                       Locator %in% locators_cb, 
                       !is.na(Depth)) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none", 
          panel.spacing.y = unit(-0.2, "lines")) + 
    labs(x = "Depth (m)", 
         y = parm_units$Label[parm_units$Parameter == input$parm_2]) + 
    coord_flip() + 
    {if (input$log & (input$parm_2 %in% parms_log)) scale_y_continuous(trans = "log", 
                                                                       labels = scales::number_format(accuracy = 0.001))} +  
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = WeekDate == input$dates_2, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      facet_wrap(~ Locator, ncol = 3, scales = "free_y") + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
  pp <- ggplotly(p, tooltip = c("text"))
  onRender(
    pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
  )
})












# nrow comparison -------------------------------------------------------------------------

first_date <- max(c(min(LIMS_report$date), min(date(portal_report$CollectDate), na.rm = T)))
last_date <- min(c(max(LIMS_report$date), max(date(portal_report$CollectDate), na.rm = T))) - 1

LIMS_data <- LIMS_report %>%
  filter(date >= first_date,
         date <= last_date)

portal_data <- portal_report %>%
  filter(CollectDate >= first_date,
         CollectDate  <= last_date)

# Removes data that is in the portal_data dataframe from the LIMS dataframe to be uploaded
LIMS_to_upload_pre <- anti_join(LIMS_data, portal_data,
                                by = c("NUMVALUE" = "Value",
                                       "LABSAMPLENUM" = "LabSampleNum",
                                       "LOCATOR" = "Locator",
                                       "WORKNUM" = "WorkNum")) %>%
  mutate_all(~ ifelse(is.na(.), "",(.))) %>%
  select(-date, -datetime)

# Filter out unnecessary project #s and QUALS data to upload -----------------------------------------------

# Removes project #s 421250-100 (TSG) and 423283-100 (bacteria study)
# Removes non <MDL, <RDL, TA qualifier from the dataset, and writes those to a seperate df
LIMS_to_upload <- LIMS_to_upload_pre %>%
  filter(!PROJECT %in% c("421250-100", "423283-100"),
         QUALIFIER %in% c("", "<MDL", "<RDL", "<RDL,TA", "<MDL,TA", "TA"))

data_with_quals <- LIMS_to_upload_pre %>%
  filter(!QUALIFIER %in% c("", "<MDL", "<RDL", "<RDL,TA", "<MDL,TA", "TA"))

write_delim(LIMS_to_upload,
            file = here(paste0("LIMS_upload_", first_date, "--", last_date, ".xlsx")),
            delim = "\t")
write_delim(data_with_quals,
            file = here(paste0("qual_data_for_review_", first_date, "--", last_date, ".xlsx")),
            delim = "\t")

# Identifies the difference in the number of samples btwn LIMS and the portal --------
# Writes the output to a txt file in the project folder

sink(here("LIMS - Portal Samples.txt"))
for(sample in unique(LIMS_data$LABSAMPLENUM)){
  LIMS <- LIMS_data %>%
    filter(LABSAMPLENUM == sample)
  portal <- portal_data %>%
    filter(`LabSampleNum` == sample)
  cat("\n", paste(unique(LIMS$date), unique(LIMS$LABSAMPLENUM), "\n", "LIMS", paste0("(",nrow(LIMS),")"), "-", "Portal", paste0("(",nrow(portal),")"), "=", "\t", nrow(LIMS) - nrow(portal)), "\n")
}
sink()

shell.exec(here())



# Scratch -----------------------------------------------------------------
# 
# LIMS_test_file <- "C:\\Users\\gikeda\\OneDrive - King County\\Documents\\September 2023\\Bottle Data Investigation\\greg_test.xls"
# 
# NSAJ02_LIMS <- read_delim(LIMS_test_file, 
#                           delim = "\t", escape_double = FALSE, 
#                           trim_ws = TRUE) %>%
#   mutate(date = as.Date(mdy_hms(COLLECTDATE)),
#          datetime = mdy_hms(COLLECTDATE))
# 
# test_dates <- ymd(c("2023-05-24", "2023-06-21", "2023-05-03", "2023-06-07", "2023-05-16"))
# 
# NSAJ02_portal <- bottle_data %>%
#   filter(CollectDate > mdy("5/1/2023"), 
#          Locator == "NSAJ02")
# 
# 
# summary(portal_report$datetime) # Latest sample is 7-5-2023
# 
# test <- LIMS_to_upload %>%
#   mutate(datetime = date(mdy_hms(COLLECTDATE))) %>%
#   select(datetime, PARMNAME, everything()) %>%
#   filter(datetime < mdy("6/1/2023"),
#          LOCATOR == "NSAJ02",
#          LABSAMPLENUM == "L80810-3")
# 
# test2 <- LIMS_report %>%
#   mutate(datetime = date(mdy_hms(COLLECTDATE))) %>%
#   select(datetime, PARMNAME, everything()) %>%
#   filter(datetime < mdy("6/1/2023"),
#          LOCATOR == "NSAJ02",
#          LABSAMPLENUM == "L80810-3")
# 
# test2 <- LIMS_report %>%
#   mutate(datetime = date(mdy_hms(COLLECTDATE))) %>%
#   select(datetime, PARMNAME, everything()) %>%
#   filter(datetime < mdy("7/1/2023"))
# 
# 
# # FIND OUT - The Eneterococcus value in test is actually in the portal. Find out why. 
# # Otherwise, follow along with the instructions in the notebook.
# 
