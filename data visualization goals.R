## GOALS: 

## Read in data (Hydrology_UKEZC, in "data" folder of this project)
  #note: Bailey changed the headers because R didn't like importing Hydrology_UKEZC. Saved it in GitHub as "Hydrology_UKEZC_bk.csv". 

## Visualize daily discharge over time from 2001-2003, coloring the values which are "E" (estimated) vs those with no flags

## Visualize water temperature for 2022-2023 on a shared x-axis spanning the first to last day of the year


# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here())


# Read in data ------------------------------------------------------------

# Read csv file into R and change the two datetime columns into proper date/time formats

# Adding notgreg_ plus some additional code to the main branch of "data visualization goals.R" so we can see how new code + code changes work. 

notgreg_hydro_data <- read_csv(here("Hydrology_UKEZC_bk.csv")) %>%
  mutate(CollectDate_UTC = mdy_hm(CollectDate_UTC),
        `Collect Date_PDT` = mdy_hm(`Collect Date_PDT`),
        date = date(`Collect Date_PDT`))

# Checking to make sure that the date range is as expected (2001 - 2023) 
summary(notgreg_hydro_data$CollectDate_UTC)

# Creating seperate dataframe for 2001 - 2003 data
notgreg_hydro_01_03 <- hydro_data %>%
  filter(date >= mdy("1-1-2001"),
         date < mdy("1-1-2004"))

summary(notgreg_hydro_01_03$date)

# 2001 - 2003 data --------------------------------------------------------

# visualize daily discharge with flags on estimated values
# data were collected at 15-min intervals so we need to decide what we are interested in to summarize over day (e.g., mean, max, min). If we choose mean, and there are flags on some records but not others within the same day, we have to choose how to reflect that.

daily_max_discharge <- notgreg_hydro_01_03 %>%
  select(date, Discharge_cfs, Flag_StageQ) %>% 
  group_by(date) %>% 
  slice_max(Discharge_cfs, with_ties = FALSE) %>% 
  ungroup()

summary(daily_max_discharge)
  
  
  

