## GOALS: 

## Read in data (Hydrology_UKEZC, in "data" folder of this project)
  #note: Bailey changed the headers because R didn't like importing Hydrology_UKEZC. Saved it in GitHub as "Hydrology_UKEZC_bk.csv". 

## Visualize daily discharge over time from 2001-2003, coloring the values which are "E" (estimated) vs those with no flags

## Visualize water temperature for 2022-2023 on a shared x-axis spanning the first to last day of the year

rm (list=ls())

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here())
library(plotly)
library(anytime)


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
ggplot(subset(hydro_dt, discharge_qualifier == "P")) +
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
           dt < "2004-01-01 00:00:00") #(maybe cutoff Jan 1 2004?)

## explore the data ####
#first I want to know if there are any missing values for our y variable.
hydro_dt_0103 %>% 
  filter(is.na(discharge_cfs)) %>% 
  count() #there are 35 missing cfs values

#then I want to know if there are any missing values for our x variable.
hydro_dt_0103 %>% 
  filter(is.na(dt)) %>% 
  count() #there not missing dates

#then I want to know what the qualifiers are, and how many there are of each
hydro_dt_0103 %>% 
  group_by(discharge_qualifier) %>% 
  summarise(qualifier_tally = n())
## about 8k E values

ggplot(hydro_dt_0103, aes(x=dt, y=discharge_cfs)) +
  geom_point(aes(color = discharge_qualifier)) +
  theme_bw()

#visualize daily discharge over 2001-2003 using hydro_b



# Visualize water temperature for 2022-2023 on a shared x-axis spa --------




