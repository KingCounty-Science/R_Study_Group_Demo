## GOALS: 

## Read in data (Hydrology_UKEZC, in "data" folder of this project)
  #note: Bailey changed the headers because R didn't like importing Hydrology_UKEZC. Saved it in GitHub as "Hydrology_UKEZC_bk.csv". 

## Visualize daily discharge over time from 2001-2003, coloring the values which are "E" (estimated) vs those with no flags

## Visualize water temperature for 2022-2023 on a shared x-axis spanning the first to last day of the year

rm (list=ls()) # clean environment

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here())
library(plotly)
library(anytime) # I had no idea this package existed, TIL! 


# Read in data ------------------------------------------------------------
# read in data and ignore column header reading errors (reads in as is)
# use here and "data" so we all can run code
# hydro <- read.csv("C:/GitHub/R_Study_Group_Demo/data/Hydrology_UKEZC.csv", check.names = F)
# change header names to remove spaces
hydro <- read.csv(here("data","Hydrology_UKEZC.csv"), check.names = F,
                  col.names = c("Site_Code","date_UTC","date_local","stage_ft",
                                "discharge_cfs","discharge_qualifier",
                                "turbidity_NTU","turbidity_qualifier",
                                "temperature_C","temperature_qualifier"))
hydro_b <- read_csv(here("Hydrology_UKEZC_bk.csv"))

# # change header name
# colnames(hydro)[2] <- "date_UTC"


# Visualize daily discharge over 2001 - 2003 -------------------------------------
#leaving this here for whomever had pre-populated this section
ggplot(subset(hydro, discharge_qualifier == "P")) +
  geom_point(aes(x=date_local, y=discharge_cfs, color=discharge_qualifier)) +
  theme_bw()

#first check the date format of hydro (referencing greg's tutorial from a few months ago)
#check date format of data
str(hydro$date_local) #it is a character string

#mutate to POSIXct and create a new column called "dt" 
hydro_dt <- hydro %>% 
  mutate(dt = dmy_hm(date_local))

str(hydro_dt$dt) #now it is POSIXct

#now we just need the 01-03 data 
hydro_dt_0103 <- hydro_dt %>% 
  filter(dt > "2001-01-01 00:00:00" & 
           dt < "2003-12-31 23:59:59") #(maybe cutoff Jan 1 2004?)

## explore the data ####
#first I want to know if there are any missing values for our y variable.
hydro_dt_0103 %>% 
  filter(is.na(discharge_cfs)) %>% 
  count() 

#then I want to know if there are any missing values for our x variable.
hydro_dt_0103 %>% 
  filter(is.na(dt)) %>% 
  count() #there not missing time/dates (i.e., no cfs values missing a timestamp)

#then I want to know what the qualifiers are, and how many there are of each
hydro_dt_0103 %>% 
  group_by(discharge_qualifier) %>% 
  summarise(qualifier_tally = n())
## about 8k E values

#It seems there are two gaps that have estimated values.
ggplot(hydro_dt_0103, aes(x=dt, y=discharge_cfs)) +
  geom_point(aes(color = discharge_qualifier),
             size = .75) + #reduced the size of the points
  theme_bw() +
  scale_color_manual(name='Discharge qualifier', #adding names manually to give our colors names and a legend title, and whatever colors we want.
                     labels = c("collected", "estimated"),
                     values=c("#5ab4ac", "#d8b365")) +
  ylab("Discharge (cfs)") +
  scale_x_datetime(name = "date",
                   date_breaks = "3 months", #could be more or less
                   minor_breaks = "1 month", #too many lines?
                   date_labels = "%b '%y") #there are lots of ways we could specify the display here: https://rdrr.io/r/base/strptime.html

#alternative idea with lines. Is it OK to have lines between the main points or should all of it be points?
ggplot(hydro_dt_0103, aes(x=dt, y=discharge_cfs)) +
  geom_line() +
  geom_point(data = hydro_dt_0103 %>% filter(discharge_qualifier == "E"), 
             color = "orange",
             size = .75) +
  theme_bw()



#visualize daily discharge over 2001-2003 using hydro_b

# we don't have to do all the QA checking above again since hydro_b is identical data to hydro (except that it is in tibble format https://tibble.tidyverse.org), but I am going to do one additional QA and check for duplicates
hydro_b %>% filter(duplicated(CollectDate_UTC)) # no duplicates found

hydro_b %>% 
  select(CollectDate_UTC, Discharge_cfs, Flag_StageQ) %>%  # these are the only columns I care about for this visualization
  mutate(ReDate = mdy_hm(CollectDate_UTC)) %>%  # convert to dttm
  filter(year(ReDate) %in% (2001:2003)) %>%
  mutate(eflag = case_when(
    Flag_StageQ == "E" ~ "E", 
    .default = "not E"
  )) %>%
  ggplot(aes(x = ReDate, y = Discharge_cfs)) +
  geom_line() +
  geom_point(data = ~subset(., eflag == "E"), 
             color = "orange", 
             size = 0.75) + 
  theme_bw()



# Visualize water temperature for 2022-2023 on a shared x-axis spa --------




