#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?
#-----------------------------Loading Data -------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr",
               "srvyr", "survey", "ggsurvey")

#EPIC
epic_data <- read.csv("./processed_data/epic_data_var_interest.csv")
glimpse(epic_data)
View(epic_data)

#-------------------------------------------------------------------------------
#------------------------------- Weights ---------------------------------------
#-------------------------------------------------------------------------------