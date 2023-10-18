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
hydro_data <- read_csv(here("Hydrology_UKEZC_bk.csv")) %>%
  mutate(CollectDate_UTC = mdy_hm(CollectDate_UTC),
        `Collect Date_PDT` = mdy_hm(`Collect Date_PDT`),
        date = date(`Collect Date_PDT`))

# Checking to make sure that the date range is as expected (2001 - 2023) 
summary(hydro_data$CollectDate_UTC)

# Creating seperate dataframe for 2001 - 2003 data
hydro_01_03 <- hydro_data %>%
  filter(date >= mdy("1-1-2001"),
         date < mdy("1-1-2004"))

summary(hydro_01_03$date)

# 2001 - 2003 data --------------------------------------------------------



