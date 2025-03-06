#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: EPIC 2022

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr","jsonlite")

##OECD carbon rate
epic_data <- read.csv("./processed_data/epic_data_var_interest.csv")
glimpse(epic_data)
###Variables of interest for core message of research project
###Subsample for HHs where adoption of EETs is feasible --> see Hassett et al. (2024) pp.43
###EET adoption
###Why not EET
###Drivers for adopting EET
###income
###government support
###home ownership
###detached housing
###environmental attitude
###country - ref area
###weights




