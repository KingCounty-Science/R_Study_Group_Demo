## GOALS: 

## Read in data (Hydrology_UKEZC, in "data" folder of this project)
  #note: Bailey changed the headers because R didn't like importing Hydrology_UKEZC. Saved it in GitHub as "Hydrology_UKEZC_bk.csv". 

## Visualize daily discharge over time from 2001-2003, coloring the values which are "E" (estimated) vs those with no flags

## Visualize water temperature for 2022-2023 on a shared x-axis spanning the first to last day of the year

# Setup -------------------------------------------------------------------
install.packages("here")
 
library(tidyverse)
library(lubridate)
library(here())
library(plotly)


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


# Visualize daily discharge over 2001 - 2003 -------------------------------------

hydro_daily_discharge <- hydro_01_03 %>%
  group_by(date) %>%
  summarize(daily_discharge_avg = mean(Discharge_cfs))

# Append daily discharge values to the full dataframe to retain the error flags. 
# Note that error flags apply to 15-minute samples, not the daily averages. Will need to think through that!
hydro_withaverages <- left_join(hydro_01_03, hydro_daily_discharge,
                  by = join_by(date))

# Plotting all 2001 - 2003 data, coloring points by the Flag Value
# Not separating by year yet
testplot <- ggplot(hydro_withaverages)+
  geom_point(aes(x = CollectDate_UTC,
                 y = daily_discharge_avg,
                 color = as.factor(Flag_StageQ)))




# Testing conflicts -------------------------------------------------------

1+1
2+2

cool_person = "Iris"

