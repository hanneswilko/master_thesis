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
         C44_2, C44_3, C44_4, C44_6, C44_7, C44_8, C44_9, C45_1, C45_3, C45_4, C45_6, C45_7, C45_8, C45_9, C46_1,
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

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Define mapping for EETs
EETs <- tibble::tibble(
  tech_name = c("Appliances", "Windows", "Thermal insulation", 
                "Solar panels electricity", "Solar panels water", 
                "Battery storage", "Heat pumps"),
  adoption = c("C44_1", "C44_3", "C44_4", "C44_6", "C44_7", "C44_8", "C44_9"),
  support = c("C45_1", "C45_3", "C45_4", "C45_6", "C45_7", "C45_8", "C45_9"),
  possible = c("Appl_p", "Window_p", "Thermal_p", "Solare_p", "Solarw_p", "Battery_p", "Pump_p")
)

# Function to process each technology
process_eet <- function(data, tech_row) {
  data %>%
    group_by(country = .data$Country_name) %>%
    summarise(
      eet = tech_row$tech_name,
      adopted = sum(.data[[tech_row$adoption]] == 1, na.rm = TRUE),
      not_possible = sum(.data[[tech_row$possible]] == 0, na.rm = TRUE),
      mean_adoption_rate = mean(.data[[tech_row$adoption]] == 1, na.rm = TRUE),
      mean_adoption_rate_possible = mean(.data[[tech_row$adoption]][.data[[tech_row$possible]] == 1] == 1, na.rm = TRUE),
      support_count = sum(.data[[tech_row$support]] == 1, na.rm = TRUE),
      mean_support_rate = mean(.data[[tech_row$support]] == 1, na.rm = TRUE),
      mean_support_rate_possible = mean(.data[[tech_row$support]][.data[[tech_row$possible]] == 1] == 1, na.rm = TRUE),
      .groups = "drop"
    )
}

# Apply the function to each EET and bind results
summary_table <- EETs %>%
  split(.$tech_name) %>%
  map_dfr(~process_eet(epic_EET, .x))

# View result
print(summary_table)

#Check
epic_EET %>%
  filter(Country_name == "BE") %>%
  summarise(mean_adoption = mean(C44_1 == 1, na.rm = TRUE))

summary(as.factor(epic_EET$C44_1[epic_EET$Country_name == "BE"]))

summary(as.factor(epic_EET$Appl_p[epic_EET$Country_name == "BE"]))

summary(as.factor(epic_EET$C45_1[epic_EET$Country_name == "BE"]))









































