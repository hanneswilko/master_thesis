#-------------------------------------------------------------------------------
#------------------- Combined Descriptive Analysis  ----------------------------
#-------------------------------------------------------------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?

#Table of Content
#1. Loading data
#2. Data wrangling
#3. Descriptive Analysis

#-------------------------------------------------------------------------------
#---------------------------- 1. Loading Data ----------------------------------
#-------------------------------------------------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr",
               "srvyr", "survey", "ggsurvey", "purrr")

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
         S18, S19_1, S19_1_1, S20, B23_1, B31_1, B31_3, B31_5, B31_6, B31_7, B31_8, C37_1, C37_2, C37_3, C37_4,
         C37_5, C37_6, C37_8, C49_1, C49_2, C49_3, C44_1, C44_2, C44_3, C44_4, C44_6, C44_7,
         C44_8, C44_9, C45_1, C45_3, C45_4, C45_6, C45_7, C45_8, C45_9, C46_1,
         C46_2, C46_3, C46_4, C46_6, C46_7, C46_8, C46_9, C47_2, C47_6)

#EPS Index
EPS <- read.csv("./processed_data/OECD_EPS_data.csv")

#Emission-weighted carbon price
wC02price <- read.csv("./processed_data/OWiD_C02price_data.csv")


#-------------------------------------------------------------------------------
#------------------------- 2. Data wrangling -----------------------------------
#-------------------------------------------------------------------------------
#C44_1: Appliances
#C44_2: LEDs
#C44_3: Windows
#C44_4: Thermal insulation
#C44_6: Solar panels electricity
#C44_7: Solar panels water
#C44_8: Battery storage
#C44_9: Heat pumps

#C45_1: Government support for Appliances
#C45_3: Government support for Windows
#C45_4: Government support for Thermal insulation
#C45_6: Government support for Solar panels electricity
#C45_7: Government support for Solar panels water
#C45_8: Government support for Battery storage
#C45_9: Government support for Heat pumps

#C46_1: Why not Appliances
#C46_2: Why not LEDs
#C46_3: Why not Windows
#C46_4: Why not Thermal insulation
#C46_6: Why not Solar panels electricity
#C46_7: Why not Solar panels water
#C46_8: Why not Battery storage
#C46_9: Why not Heat pumps

#create flagging for HHs where respective EET adoption wasn't possible
epic_EET <- epic %>%
  mutate(
    # Highly energy-efficient appliances
    Appl_p = case_when(
      C44_1 == 1 ~ 1, #if adopted = possible
      C46_1 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    # Energy-efficient windows
    Window_p = case_when(
      C44_3 == 1 ~ 1, #if adopted = possible
      C46_3 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    # Thermal insulation
    Thermal_p = case_when(
      C44_4 == 1 ~ 1, #if adopted = possible
      C46_4 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    # Solar panels for electricity
    Solare_p = case_when(
      C44_6 == 1 ~ 1, #if adopted = possible
      C46_6 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    # Solar panels for water
    Solarw_p = case_when(
      C44_7 == 1 ~ 1, #if adopted = possible
      C46_7 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    # Battery storage
    Battery_p = case_when(
      C44_8 == 1 ~ 1, #if adopted = possible
      C46_8 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    # Heat pumps
    Pump_p = case_when(
      C44_9 == 1 ~ 1, #if adopted = possible
      C46_9 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    )
  )

#-------------------------------------------------------------------------------
#------------------------- 3. Descriptive Analysis -----------------------------
#-------------------------------------------------------------------------------

#-------------------------- 1. Summary table EET adoption ----------------------
glimpse(epic_EET)

#Define the countries
countries <- unique(epic_EET$Country_name)

#Define the technologies with corresponding column names
technologies <- c("Appliances", "Windows", "Thermal insulation", 
                  "Solar panels for electricity", "Solar water heating", 
                  "Battery storage", "Heat pumps")

adopt_cols <- c("C44_1", "C44_3", "C44_4", "C44_6", "C44_7", "C44_8", "C44_9")
support_cols <- c("C45_1", "C45_3", "C45_4", "C45_6", "C45_7", "C45_8", "C45_9")
possible_cols <- c("Appl_p", "Window_p", "Thermal_p", "Solare_p", "Solarw_p", "Battery_p", "Pump_p")

#Core Function
get_summary <- function(country) {
  data_country <- epic_EET %>% filter(Country_name == country)
  
  map2_dfr(seq_along(technologies), technologies, function(i, tech) {
    adopt <- data_country[[adopt_cols[i]]]
    support <- data_country[[support_cols[i]]]
    possible <- data_country[[possible_cols[i]]]
    
    # Clean valid cases
    is_possible <- possible == 1
    valid_adopt <- adopt %in% c(1, 2)
    valid_support <- support %in% c(1, 2)
    
    total_possible <- sum(is_possible, na.rm = TRUE)
    adopted <- sum(adopt == 1, na.rm = TRUE)
    not_possible <- sum(possible == 0, na.rm = TRUE)
    supported <- sum(support == 1, na.rm = TRUE)
    
    # Mean rates among possible
    mean_adopt_rate <- sum(adopt[is_possible] == 1, na.rm = TRUE) / total_possible
    mean_support_rate <- sum(support[is_possible & adopt == 1] == 1, na.rm = TRUE) / 
      sum(is_possible & adopt == 1, na.rm = TRUE)
    
    tibble(
      Country = country,
      Technology = tech,
      Adopted = adopted,
      Not_Possible = not_possible,
      Supported = supported,
      Mean_Adoption_Rate = round(mean_adopt_rate, 3),
      Mean_Support_Rate = round(mean_support_rate, 3)
    )
  })
}

#Create Summary Data Frame --> Table 1
EET_summary_df <- map_dfr(countries, get_summary)


#---------------- 2. Table: socioeconomic characteristics ----------------------

#Age
attr(epic_raw$Age_cat, "labels")
#Age_cat: 18-24 = 1, 25-34 = 2, 35-44 = 3, 45-54 = 4, 55+ = 5
summary(epic_EET$Age_cat)

#Sex
attr(epic_raw$Gender, "labels")
#Age_cat: Male = 1, Female = 2
summary(epic_EET$Age_cat)

#Income quintile
attr(epic_raw$Income, "labels")
attr(epic_raw$S12_1, "labels")
#Age_cat: 18-24 = 1, 25-34 = 2, 35-44 = 3, 45-54 = 4, 55+ = 5
summary(epic_EET$Income)

#Environmental concern
attr(epic_raw$B23_1, "label")
#B23_1: Not at all important        Not important   Somewhat important            Important       Very important    Prefer not to say 
summary(epic_EET$B23_1)
#create new variable with binary level for low and high environmental concern
epic_EET <- epic_EET %>%
  mutate(
    Env_concern = ifelse(B23_1 == 999999, NA, B23_1),
    Env_concern = case_when(
      Env_concern %in% c(4, 5) ~ 1, #high
      Env_concern %in% c(1, 2, 3) ~ 0, #low
      TRUE ~ NA_real_
    )
  )

summary(epic_EET$Env_concern)

#Education
#

#----------------- 3. Table: characteristics of dwellings ----------------------

#Energy costs
attr(epic_raw$C37_8, "label")
#C37_1: energy costs US
#C37_2: energy costs FR, NL and BE
#C37_3: energy costs UK
#C37_4: energy costs SE
#C37_5: energy costs CH
#C37_6: energy costs IL
#C37_8: energy costs CA
#creating new variable with merged info on average monthly cost
epic_EET <- epic_EET %>%
  mutate(
    Energy_costs = case_when(
      Country_name == "US" ~ C37_1,
      Country_name %in% c("FR", "NL", "BE") ~ C37_2,
      Country_name == "UK" ~ C37_3,
      Country_name == "SE" ~ C37_4,
      Country_name == "CH" ~ C37_5,
      Country_name == "IL" ~ C37_6,
      Country_name == "CA" ~ C37_8,
      TRUE ~ NA_real_
    ),
    Energy_costs = ifelse(Energy_costs == 888888, NA, Energy_costs)
  )

#Home ownership
attr(epic_raw$S5, "labels")
#S5
#Living in a residence owned = 1, Living in a residence rented = 2, Living in another type of accommodation no owned = 3
#creating new variable of home ownership with only two categories owned/not owned
epic_EET <- epic_EET %>%
  mutate(
    Home_owned = case_when(
      S5 == 1 ~ 1,           # Owned
      S5 %in% c(2, 3) ~ 0,   # Not owned
      TRUE ~ NA_real_        # Preserve NAs
    )
  )

#Dwelling
attr(epic_raw$S18, "labels")
#S18
#apartment in a building with less than 12 apartments = 1, apartment in a building with 12 or more apartments = 2
#A detached house = 3, A semi-detached/terraced house  = 4, Other = 89
#creating new variable of home ownership with only two categories owned/not owned
epic_EET <- epic_EET %>%
  mutate(
    Dwelling_house = case_when(
      S18 == 3 ~ 1, #detached house
      S18 == 4 ~ 1, #house
      S18 %in% c(1, 2, 89) ~ 0,   #apartment/other
      TRUE ~ NA_real_ #NAs
    )
  )

#Dwelling size
attr(epic_raw$S19_1, "labels")
attr(epic_raw$S19_1_1, "labels")
summary(epic_EET$S19_1)
summary(epic_EET$S19_1_1)
#S19_1: Rest & S19_1_1: US & CA
#Less than 25m2 = 1, 25m2-50m2 = 2, 51m2-75m2 = 3, 76m2-100m2 = 4, 101m2-150m2 = 5, 151m2-200m2 = 6, More than 200m2 = 7 Don’t know = 888888
#creating new variable for mergerd info auf dwelling size (m2 and ft2)
epic_EET <- epic_EET %>%
  mutate(
    Dwelling_size = case_when(
      !is.na(S19_1) & S19_1 != 888888 ~ S19_1,
      !is.na(S19_1_1) & S19_1_1 != 888888 ~ S19_1_1,
      TRUE ~ NA_integer_
    )
  )

#Rural
attr(epic_raw$S20, "labels")
#S20 - area
#major town/city = 1, suburban = 2, small town/village = 3, isolated dwelling = 4, Other = 89
#creating new variable of home ownership with only two categories owned/not owned
epic_EET <- epic_EET %>%
  mutate(
    Rural = case_when(
      S20 %in% c(3, 4, 89) ~ 1,
      S20 %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    )
  )

#creating summary table --> table 3
Dwelling_summary_df <- epic_EET %>%
  group_by(Country_name) %>%
  summarise(
    Avg_Home_Owned = mean(Home_owned, na.rm = TRUE),
    Avg_Dwelling_House = mean(Dwelling_house, na.rm = TRUE),
    Avg_Dwelling_Size = mean(Dwelling_size, na.rm = TRUE),
    Avg_Rural = mean(Rural, na.rm = TRUE),
    Avg_Energy_Costs = mean(Energy_costs, na.rm = TRUE)
  ) %>%
  arrange(Country_name)























