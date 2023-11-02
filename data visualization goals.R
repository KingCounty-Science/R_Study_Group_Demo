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
hydro <- read.csv("C:/GitHub/R_Study_Group_Demo/data/Hydrology_UKEZC.csv", check.names = F)

# change headers to no spaces
colnames(hydro)[2] <- "date_UTC"
colnames(hydro)[3] <- "date_local"
colnames(hydro)[4] <- "stage_ft"
colnames(hydro)[5] <- "discharge_cfs"
colnames(hydro)[6] <- "discharge_qualifier"
colnames(hydro)[7] <- "turbidity_NTU"
colnames(hydro)[8] <- "turbidity_qualifier"
colnames(hydro)[9] <- "temperature_C"
colnames(hydro)[10] <- "temperature_qualifier"


# Visualize daily discharge over 2001 - 2003 -------------------------------------

ggplot(subset(hydro, discharge_qualifier == "P")) +
  geom_point(aes(x=date_local, y=discharge_cfs, color=discharge_qualifier)) +
  theme_bw()


# Visualize water temperature for 2022-2023 on a shared x-axis spa --------




