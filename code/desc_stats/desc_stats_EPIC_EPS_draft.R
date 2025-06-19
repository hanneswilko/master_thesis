#-------------------------------------------------------------------------------
#------------------- Descriptive Analysis - EPIC -------------------------------
#-------------------------------------------------------------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?

#Table of Content
#1. Loading data
#2. Descriptive Analysis
#3. Graphs

#-------------------------------------------------------------------------------
#---------------------------- 1. Loading Data ----------------------------------
#-------------------------------------------------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr",
               "srvyr", "survey", "ggsurvey")

#EPIC
epic <- read.csv("./processed_data/epic_data_ABC_VoI.csv") #subsample var interest of survey parts A,B,C

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
  select(ID, Country_code, Country_name, Age_cat, Income, S5, REGION_UK, REGION_SE3,
         REGION_US2, REGION_NL2, REGION_CH, REGION_FR2, REGION_CA, REGION_BE, REGION_IL,
         S18, B23_1, B31_1, B31_3, B31_5, B31_6, B31_7, B31_8, C49_1, C49_2, C49_3, C44_1,
         C44_2, C44_3, C44_4, C44_6, C44_9, C45_1, C45_3, C45_4, C45_6, C45_9, C46_1,
         C46_2, C46_3, C46_4, C46_6, C46_9, C47_2, C47_6)

#Environmental Stringency
##OECD - EPS Index
EPS_data <- read.csv("./processed_data/OECD_EPS_data.csv")

EPS <- EPS_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  filter(TIME_PERIOD %in% 2010:2020)

## Our World in Data - weighted carbon price
wC02price_data <- read.csv("./processed_data/OWiD_C02price_data.csv")

wC02price <- wC02price_data %>%
  select(Code, Year, price_with_tax_weighted_by_share_of_co2) %>%
  filter(Year %in% 2010:2020)

#-------------------------------------------------------------------------------
#------------------------- 2. Data Sets of Interest ----------------------------
#-------------------------------------------------------------------------------

#------------------------------- EPIC ------------------------------------------
#medium and high-cost EETs adoption - merge and check for possibility
epic_EET <- epic %>%
  mutate(
    
    # Middle-cost EET adoption (Highly energy-efficient appliances)
    middle_EET = ifelse(C44_1 == 1, 1, 0),
    
    # High-cost EET adoption (Energy-efficient windows, Thermal insulation, Solar panels, Heat pumps)
    high_EET = ifelse(C44_3 == 1 | C44_4 == 1 | C44_6 == 1 | C44_9 == 1, 1, 0),
    
    # Middle-cost EET adoption (Highly energy-efficient appliances) not possible
    middle_EET_possible = case_when(
      C44_1 == 1 ~ 1, #if adopted = possible
      C46_1 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    
    # High-cost EET adoption (Energy-efficient windows, Thermal insulation, Solar panels, Heat pumps) not possible
    high_EET_possible = case_when(
      C44_3 == 1 | C44_4 == 1 | C44_6 == 1 | C44_9 == 1 ~ 1,  #if adopted = possible
      C46_3 == 4 | C46_4 == 4 | C46_6 == 4 | C46_9 == 4 ~ 0,  #no possible
      TRUE ~ 1  #else possible
    )
  )

#medium and high-cost EETs government support for adoption
epic_EET <- epic_EET %>%
  mutate(
    # Middle-cost EET gov support (Highly energy-efficient appliances)
    middle_EET_gov_support = case_when(
      C45_1 == 1 ~ 1, #support
      TRUE ~ 0 #else no support
    ),
    
    # High-cost EET gov support (Energy-efficient windows, Thermal insulation, Solar panels, Heat pumps)
    high_EET_gov_support = case_when(
      C45_3 == 1 | C45_4 == 1 | C45_6 == 1 | C45_9 == 1 ~ 1,  #support
      TRUE ~ 0  #else no support
    )
  )

#------------------------------- EPIC ------------------------------------------
##average EPS
EPS_avg <- EPS %>%
  group_by(REF_AREA) %>%
  summarize(avg_EPS = mean(OBS_VALUE, na.rm = TRUE))

##average carbon price
wC02price_avg <- wC02price %>%
  group_by(Code) %>%
  summarize(price_with_tax_weighted_by_share_of_co2_avg = mean(price_with_tax_weighted_by_share_of_co2, na.rm = TRUE))

#-------------------------------------------------------------------------------
#------------------------- 3. Summary statistics -------------------------------
#-------------------------------------------------------------------------------

#--------------------------- middle-cost EETs ----------------------------------
epic_middleEET <- epic_EET %>%
  filter(middle_EET_possible != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, middle_EET, middle_EET_gov_support)

#--------------------------- high-cost EETs -------------------------------------
epic_highEET <- epic_EET %>%
  filter(high_EET_possible != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_3, C44_4, C44_6, C44_9,
         C46_3, C46_4, C46_6, C46_9, C45_3, C45_4, C45_6, C45_9,
         high_EET, high_EET_possible, high_EET_gov_support)





