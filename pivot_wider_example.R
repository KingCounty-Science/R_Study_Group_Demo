# This script is designed to do the following
# 1. Read in data from LIMS
# 2. Grab the same data from the Portal (if it exists!)

rm(list = ls())
library(tidyverse)
library(readr)
library(lubridate)
library(here)
library(svDialogs)
library(svDialogs)

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


# Modify LIMS DMPA format to tidy format ----------------------------------

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


# Experimenting with dates and times in lubridate -------------------------

date_test <- "2000-05-01T07:00:00Z"

date_test_real <- ymd_hms(date_test,
                          tz = "America/Los_Angeles")

