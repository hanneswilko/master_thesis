#----------------------------EPIC Survey Data-----------------------------------
##Feedback loop Nr. 1
##Goal: data variation in variable of interests

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr")

#load data
getwd()
epic_raw <- read.csv("./raw_data/epic_data.csv")
head(epic_raw)
class(epic_raw)
glimpse(epic_raw)
View(epic_raw)

#check variables of interest
unique(epic_raw$SCR2) #HH repsonsibilty
unique(epic_raw$Gender) #gender
unique(epic_raw$S1) #sex2
unique(epic_raw$Income) #income categorized --> drop S12_1 - S12_9
unique(epic_raw$S5) #home ownership
unique(epic_raw$C34_1_1) #heating/cooling with electricity
unique(epic_raw$C44_9) #EET - heat pumps
unique(epic_raw$C46_1) #EET - why not - ee appliances
unique(epic_raw$C46_9) #EET - why not - ??
unique(epic_raw$C47_2) #factors reducing energy consumption - higher prices

#variable of interest
var_interest <- c("X", "weight", "weight_2", "Country", "Gender",
                  "Age_cat", "Income", "S20", "S5", "S18", "S19_1",
                  "S19_1_1", "S19_2", "S9_US", "S9_UK", "S9_FR",
                  "S9_SE", "S9_CH", "S9_NL", "S9_CA", "S9_BE", "S9_IL",
                  "S10", "B23_1", "B31_1", "B31_3", "B31_5", "B31_6",
                  "B31_7", "B31_8", "C34_1_2", "C34_1_3", "C34_1_4",
                  "C34_1_5", "C34_1_6", "C34_1_7", "C34_1_89",
                  "C34_1_888888", "C34_2_2", "C34_2_3", "C34_2_4",
                  "C34_2_5", "C34_2_6", "C34_2_7", "C34_2_89",
                  "C34_2_888888", "C35_1", "C35_2", "C35_3", "C35_89",
                  "C35_888888", "C37_1", "C37_2", "C37_3",
                  "C37_4", "C37_5", "C37_6", "C37_8", "C44_1", "C44_2",
                  "C44_3", "C44_4", "C44_6", "C44_7", "C44_8", "C44_9",
                  "C45_1", "C45_3", "C45_4", "C45_6", "C45_7", "C45_8",
                  "C45_9", "C46_1", "C46_2", "C46_3", "C46_4", "C46_5",
                  "C46_6", "C46_7", "C46_8", "C46_9", "C47_1", "C47_2",
                  "C47_4", "C47_6", "C49_1", "C49_2", "C49_3", "C50")

#subset - variables of interest
epic_data <- epic_raw %>% select(var_interest)
length(epic_data)
glimpse(epic_data)

#save subset of interest as .csv
write.csv(epic_data, "./processed_data/epic_data_var_interest.csv", row.names = FALSE)
                                      

                                        