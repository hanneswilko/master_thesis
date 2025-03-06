#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: EPIC 2022

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr","jsonlite")

## OECD carbon rate
epic_data <- read.csv("./processed_data/epic_data_var_interest.csv")
glimpse(epic_data)




