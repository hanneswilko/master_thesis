#-------------------------------------------------------------------------------
#---------------- Descriptive Analysis - unweighted Stats ----------------------
#-------------------------------------------------------------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?

#Table of Content
#1. Loading data
#2. Descriptive Analysis

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
         S18, B23_1, B31_1, B31_3, B31_5, B31_6, B31_7, B31_8, C49_1, C49_2,C49_3, C44_1,
         C44_2, C44_3, C44_4, C44_6, C44_7, C44_9, C45_1, C45_3, C45_4,C45_6, C45_7,
         C45_9, C46_1, C46_2, C46_3, C46_5, C46_6, C46_8, C47_2, C47_6)

View(epic)

#-------------------------------------------------------------------------------
#------------------------- 2. Descriptive Analysis -----------------------------
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
epic %>%
  group_by(Country_name, C44_1) %>%
  summarize(p = survey_prop())

epic_C44_1_p <- epic %>%
  group_by(Country_name, C44_1) %>%
  summarize(count = n()) %>%
  mutate(p = round((count / sum(count)) * 100, 2))

epic_C44_1_income <- epic %>%
  filter(C44_1 == 1) %>%
  group_by(Country_name, Income) %>%
  summarize(count_1 = n()) %>%
  left_join(
    epic %>%
      group_by(Country_name, Income) %>%
      summarize(total_count = n()), 
    by = c("Country_name", "Income")
  ) %>%
  mutate(proportion_1 = round((count_1 / total_count) * 100, 2))

#bar plot adoption of EET appliances per income level and country
ggplot(epic_C44_1_income, aes(x = factor(Income, levels = c(1, 2, 3, 4, 5), ordered = TRUE), 
                              y = proportion_1, 
                              fill = factor(Income, levels = c(1, 2, 3, 4, 5), ordered = TRUE))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country_name) +  # Create a facet for each country
  labs(title = "Proportion of C44_1 == 1 by Income Level and Country",
       x = "Income Level",
       y = "Proportion of C44_1 == 1 (%)") +
  scale_fill_brewer(palette = "Set2") +  # Optional: Use a color palette for better differentiation
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank())  # Remove legend title if desired




















