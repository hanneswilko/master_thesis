#----------------------------EPIC Survey Data-----------------------------------
##Feedback loop Nr. 1
##Goal: data variation in variable of interests

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr")

#load data
getwd()
epic_raw <- read.csv("./rawdata/epic_data.csv")
