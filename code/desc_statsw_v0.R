#-------------------------------------------------------------------------------
#---------------------------- Descriptive Analysis -----------------------------
#-------------------------------------------------------------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?

#Table of Content
#1. Loading data
#2. Specifying survey desing
#3. Descriptive Analysis

#-------------------------------------------------------------------------------
#---------------------------- 1. Loading Data ----------------------------------
#-------------------------------------------------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr",
               "srvyr", "survey", "ggsurvey")

#EPIC
epic <- read.csv("./processed_data/epic_data_var_interest.csv")

epic <- epic %>%
  mutate(Country_name = case_when(
    Country == 1 ~ "US",
    Country == 2 ~ "UK",
    Country == 3 ~ "FR",
    Country == 4 ~ "NL",
    Country == 5 ~ "SE",
    Country == 6 ~ "CH",
    Country == 7 ~ "IL",
    Country == 8 ~ "CA",
    Country == 9 ~ "BE",
    TRUE ~ NA_character_  # Assign NA if no match
  )) %>%
  rename(Country_code = Country) %>%
  select(X, weight, weight_2, Country_code, Country_name, Age_cat, Income, S5,
         S18, B23_1, B31_1, B31_3, B31_5, B31_6, B31_7, B31_8, C49_1, C49_2,
         C49_3, C44_1, C44_2, C44_3, C44_4, C44_6, C44_7, C44_9, C45_1, C45_3, C45_4,
         C45_6, C45_7, C45_9, C46_1, C46_2, C46_3, C46_5, C46_6, C46_8, C47_2, C47_6)

View(epic)
#-------------------------------------------------------------------------------
#------------------------------- 2. Survey Design ------------------------------
#-------------------------------------------------------------------------------
#weight: without post-stratification income adjustment
epic_svy1 <- epic %>%
  as_survey_design(
    weights = weight, #weight for analysis without income var.
    strata = Country_code #post-stratification on country-by-country basis
  )

##filter version weight
epic_svy1_sub <- epic_svy1 %>%
  filter(rowSums(!is.na(as.data.frame(select(., starts_with("C44"))))) > 0)

#weight_2: with post-stratification income adjustment
epic_svy2 <- epic %>%
  as_survey_design(
    weights = weight_2, #weight_2 for analysis with income var.
    strata = Country_code #post-stratification on country-by-country basis
  )

##filter version weight_2
epic_svy2_sub <- epic_svy2 %>%
  filter(rowSums(!is.na(as.data.frame(select(., starts_with("C44"))))) > 0)

#-------------------------------------------------------------------------------
#------------------------- 3. Descriptive Analysis -----------------------------
#-------------------------------------------------------------------------------

#-------------------------- 3.2 Adoption of EET --------------------------------
##Highly energy-efficient appliances: C44_1, 1 = Yes, 2 = No, 99 = Don't know
###C46_1:  1 = installed >10Y, 2 = planning to 2/3Y, 3 = interest but not affordable,
###        4 = not possible, 5 = not interested, 6 = not aware


##Low-energy light bulbs: C44_2, 1 = Yes, 2 = No, 99 = Don't know
##Energy-efficient windows: C44_3, 1 = Yes, 2 = No, 99 = Don't know
##Thermal insulation of walls/roof/floor: C44_4, 1 = Yes, 2 = No, 99 = Don't know
##Solar panels for electricity: C44_6, 1 = Yes, 2 = No, 99 = Don't know
##Heat pumps: C44_9, 1 = Yes, 2 = No, 99 = Don't know
##Income: Income (categorized)

#C44_1: appliances adoption by country
epic_svy1_sub %>%
  group_by(Country_name, C44_1) %>%
  summarize(p = survey_prop())

epic_svy1 %>%
  group_by(Country_name, C44_1) %>%
  summarize(p = survey_prop())


















