#----------------------------EPIC Survey Data-----------------------------------
##Feedback loop Nr. 1
##Goal: data variation in variable of interests

#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr")

#load data
getwd()
epic_raw <- read_sav("./raw_data/epic_data.sav") #using .sav file cuz column name attr
head(epic_raw)
class(epic_raw)
glimpse(epic_raw)
View(epic_raw)

#-----------------------------Data wrangling------------------------------------
#sample
length(epic_raw$ID) #17721 full sample

##general part: A. sociodemographic & B. attitudional characterisitcs
###A.
summary(as.factor(epic_raw$Country))
summary(as.factor(epic_raw$Age_cat))
summary(as.factor(epic_raw$Gender))
summary(as.factor(epic_raw$Income))
summary(as.factor(epic_raw$S5)) #home ownership
summary(as.factor(epic_raw$S18)) #dwelling characteristics
#no NAs in part A on full sample

###B.
summary(as.factor(epic_raw$B23_1)) #importance climate change
#no NAs in part B on full sample

##thematic part: C. energy
###C35: energy source for space heating/cooling - all respondents
summary(as.factor(epic_raw$C34_1_1)) #electricity: 8925 NAs
summary(as.factor(epic_raw$C34_1_6)) #heat pumps: 8925 NAs
###C44: EET adoption - all respondents
summary(as.factor(epic_raw$C44_1)) #appliances: 8925 NAs
summary(as.factor(epic_raw$C44_9)) #heat pumps: 8925 NAs

##thematic part: D. transport
###D51_1: means of individual transport - all respondents
summary(as.factor(epic_raw$D51_1_5)) #car: 8795 NAs
summary(as.factor(epic_raw$D51_1_7)) #e-car pumps: 8795 NAs
###D59: support of env. transport policies - all respondents
summary(as.factor(epic_raw$D59_1)) #tax airplane tickets: 8795 NAs
summary(as.factor(epic_raw$D59_5)) #invest in alt. modes of transport: 8795 NAs

##thematic part: E. waste
###E66: mixed waste per week - all respondents
summary(as.factor(epic_raw$E66)) #8855 NAs
###E66: recyclable waste per week - all respondents
summary(as.factor(epic_raw$E70)) #8855 NAs

##thematic part: F. food
###F89_1: eat red meat - all respondents
summary(as.factor(epic_raw$F89_1)) #8867 NAs
###F92: improve the environmental sustainability of food systems - all respondents
summary(as.factor(epic_raw$F92_3)) #tax on meat: 8867 NAs

#-------------- Survey parts A, B and C are important --------------------------
unique(epic_raw$order_parts)
epic_raw_filtered <- epic_raw[grepl("C", epic_raw$order_parts), ] #observations with information on C. energy part

#check outcome of filter on survey parts
check_energy <- epic_raw %>%
  filter(rowSums(!is.na(select(., starts_with("C44")))) > 0)

length(epic_raw_filtered$ID) #8796 full sample
length(check_energy$ID) #8796 equivalent

##general part: A. sociodemographic & B. attitudional characterisitcs
###A.
summary(as.factor(epic_raw_filtered$Country))
summary(as.factor(epic_raw_filtered$Age_cat))
summary(as.factor(epic_raw_filtered$Gender))
summary(as.factor(epic_raw_filtered$Income))
summary(as.factor(epic_raw_filtered$S5)) #home ownership
summary(as.factor(epic_raw_filtered$S18)) #dwelling characteristics
#no NAs in part A on full sample

###B.
summary(as.factor(epic_raw_filtered$B23_1)) #importance climate change
#no NAs in part B on full sample

##thematic part: C. energy
###C35: energy source for space heating/cooling - all respondents
summary(as.factor(epic_raw_filtered$C34_1_1)) #electricity: 0 NAs
summary(as.factor(epic_raw_filtered$C34_1_6)) #heat pumps: 0 NAs
###C44: EET adoption - all respondents
summary(as.factor(epic_raw_filtered$C44_1)) #appliances: 0 NAs
summary(as.factor(epic_raw_filtered$C44_9)) #heat pumps: 0 NAs

##thematic part: D. transport
###D51_1: means of individual transport - all respondents
summary(as.factor(epic_raw_filtered$D51_1_5)) #car: 5836 NAs
summary(as.factor(epic_raw_filtered$D51_1_7)) #e-car pumps: 5836 NAs
###D59: support of env. transport policies - all respondents
summary(as.factor(epic_raw_filtered$D59_1)) #tax airplane tickets: 5836 NAs
summary(as.factor(epic_raw_filtered$D59_5)) #invest in alt. modes of transport: 5836 NAs

##thematic part: E. waste
###E66: mixed waste per week - all respondents
summary(as.factor(epic_raw_filtered$E66)) #5876 NAs
###E66: recyclable waste per week - all respondents
summary(as.factor(epic_raw_filtered$E70)) #5876 NAs

##thematic part: F. food
###F89_1: eat red meat - all respondents
summary(as.factor(epic_raw_filtered$F89_1)) #5880 NAs
###F92: improve the environmental sustainability of food systems - all respondents
summary(as.factor(epic_raw_filtered$F92_3)) #tax on meat: 5880 NAs

##checking sum observations without NAs in other parts
length(epic_raw_filtered$F89_1[!is.na(epic_raw_filtered$F89_1)])

#----------------------- Variables of Interest ----------------------------------
var_interest <- c("ID", "weight", "weight_2", "Country", "REGION_UK", "REGION_SE3",
                  "REGION_US2", "REGION_NL2", "REGION_CH", "REGION_FR2", "REGION_CA",
                  "REGION_BE", "REGION_IL","Gender",
                  "Age_cat", "Income", "order_parts", "S20", "S5", "S18", "S19_1",
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

#subset filtered - variables of interest
epic_data_ABC <- epic_raw_filtered %>% select(var_interest)
length(epic_data_ABC)
length(epic_data_ABC$ID)
glimpse(epic_data_ABC)

#subset - variables of interest
epic_data <- epic_raw %>% select(var_interest)
length(epic_data)
length(epic_data$ID)
glimpse(epic_data)

#save subset of interest as .csv
write.csv(epic_data_ABC, "./processed_data/epic_data_ABC_VoI.csv", row.names = FALSE)
write.csv(epic_data, "./processed_data/epic_data_VoI.csv", row.names = FALSE)














                                        