#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?
##OECD: EPIC 2022

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr","jsonlite")

#load data
## EPIC
epic_data <- read.csv("./processed_data/epic_data_var_interest.csv")
glimpse(epic_data)

## OECD carbon rate
C02rate_data <- read.csv("./processed_data/OCED_CO2rate_data.csv")
glimpse(C02rate_data)

## OECD EPS
EPS_data <- read.csv("./processed_data/OCED_EPS_data.csv")
glimpse(EPS_data)

## Our World in Data - weighted carbon price
wC02price_data <- read.csv("./processed_data/OWiD_C02price_data.csv")
glimpse(wC02price_data)







