#-------------------------------------------------------------------------------
#------------------- Descriptive Analysis - EPIC -------------------------------
#-------------------------------------------------------------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index

#Table of Content
#1. Loading data
#2. Data wrangling
#3. Descriptive Analysis
#4. Saving plots

#-------------------------------------------------------------------------------
#---------------------------- 1. Loading Data ----------------------------------
#-------------------------------------------------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr",
               "srvyr", "survey", "ggsurvey")

#EPIC
epic <- read.csv("./processed_data/epic_data_ABC_VoI.csv") #subsample var interest of survey parts A,B,C
epic_raw <- read_sav("./raw_data/epic_data.sav") #using .sav file cuz column name attr

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
         C44_2, C44_3, C44_4, C44_6, C44_7, C44_8, C44_9, C45_1, C45_3, C45_4, C45_6, C45_7,
         C45_8, C45_9, C46_1, C46_2, C46_3, C46_4, C46_6, C46_7, C46_8, C46_9, C47_2, C47_6)

#-------------------------------------------------------------------------------
#------------------------- 2. Data wrangling -----------------------------------
#-------------------------------------------------------------------------------
#C44_1: Appliances
#C44_2: LEDs
#C44_3: Windows
#C44_4: Thermal insulation
#C44_6: Solar panels electricity
#C44_9: Heat pumps

#C45_1: Government support for Appliances
#C45_3: Government support for Windows
#C45_4: Government support for Thermal insulation
#C45_6: Government support for Solar panels electricity
#C45_9: Government support for Heat pumps

#C46_1: Why not Appliances
#C46_2: Why not LEDs
#C46_3: Why not Windows
#C46_4: Why not Thermal insulation
#C46_6: Why not Solar panels electricity
#C46_9: Why not Heat pumps

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

#--------------------------- LEDs -------------------------------------
##No government support for LEDs alias low-cost EETs (C45_2 doesn't exist)

#---------------------------- Appliances ---------------------------------------
epic_Appl <- epic_EET %>%
  filter(Appl_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_1, C46_1, C45_1, Appl_p)

##calculating the proportion of adopters with support
epic_Appl_support_prop <- epic_Appl %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(C44_1 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_1 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support
epic_Appl_support_prop_long <- epic_Appl_support_prop %>%
  select(Country_name, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##Adoption of Appliances per Country and Government support in proportions
barchart_Appl_support_prop <- ggplot(epic_Appl_support_prop_long, 
                                     aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                         y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of highly Energy-Efficient Appliances by Country", 50),
    x = "Country",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

##calculating the proportion of adopters with support per income level
epic_Appl_income_support_prop <- epic_Appl %>%
  group_by(Country_name, Income) %>%
  summarise(
    adopters = sum(C44_1 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_1 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support per income level
epic_Appl_income_support_prop_long <- epic_Appl_income_support_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))


##adoption of middle-cost EET per Country, Income Level and Government support in proportions
barchart_Appl_income_support_prop <- ggplot(epic_Appl_income_support_prop_long, aes(x = as.factor(Income), 
                                                                              y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = str_wrap("Adoption of highly Energy-Efficient Appliances by Country and Income Level",50),
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

#--------------------------- Windows -------------------------------------
epic_Window <- epic_EET %>%
  filter(Window_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_3, C46_3, C45_3, Window_p)

##calculating the proportion of adopters with support
epic_Window_support_prop <- epic_Window %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(C44_3 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_3 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support
epic_Window_support_prop_long <- epic_Window_support_prop %>%
  select(Country_name, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##Adoption of Windows per Country and Government Support in proportions
barchart_Windows_support_prop <- ggplot(epic_Window_support_prop_long, 
                                     aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                         y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of Energy-Efficient Windows by Country",50),
    x = "Country",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

##calculating the proportion of adopters with support per income level
epic_Window_income_support_prop <- epic_Window %>%
  group_by(Country_name, Income) %>%
  summarise(
    adopters = sum(C44_3 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_3 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support per income level
epic_Window_income_support_prop_long <- epic_Window_income_support_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##adoption of Windows per Country, Income Level and Government Support in proportions
barchart_Windows_income_support_prop <- ggplot(epic_Window_income_support_prop_long, aes(x = as.factor(Income), 
                                                                                    y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = str_wrap("Adoption of Energy-Efficient Windows by Country and Income Level",50),
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

#--------------------------- Thermal insulation --------------------------------
epic_Thermal <- epic_EET %>%
  filter(Thermal_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_4, C46_4, C45_4, Thermal_p)

##calculating the proportion of adopters with support
epic_Thermal_support_prop <- epic_Thermal %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(C44_4 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_4 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support
epic_Thermal_support_prop_long <- epic_Thermal_support_prop %>%
  select(Country_name, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##Adoption of Thermal insulation per Country and Government Support in proportions
barchart_Thermal_support_prop <- ggplot(epic_Thermal_support_prop_long, 
                                     aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                         y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of Thermal Insulation of Walls/Roof/Floor by Country",50),
    x = "Country",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

##calculating the proportion of adopters with support per income level
epic_Thermal_income_support_prop <- epic_Thermal %>%
  group_by(Country_name, Income) %>%
  summarise(
    adopters = sum(C44_4 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_4 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support per income level
epic_Thermal_income_support_prop_long <- epic_Thermal_income_support_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##adoption of Thermal insulation per Country, Income Level and Government Support in proportions
barchart_Thermal_income_support_prop <- ggplot(epic_Thermal_income_support_prop_long, aes(x = as.factor(Income), 
                                                                                         y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = str_wrap("Adoption of Thermal Insulation of Walls/Roof/Floor by Country and Income Level",50),
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

#--------------------- Solar panels for electricity ----------------------------
epic_Solare <- epic_EET %>%
  filter(Solare_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_6, C46_6, C45_6, Solare_p)

##calculating the proportion of adopters with support
epic_Solare_support_prop <- epic_Solare %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(C44_6 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_6 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support
epic_Solare_support_prop_long <- epic_Solare_support_prop %>%
  select(Country_name, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##Adoption of Solar panels for electricity per Country and Government Support in proportions
barchart_Solare_support_prop <- ggplot(epic_Solare_support_prop_long, 
                                        aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                            y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of Solar Panels for Electricity by Country",50),
    x = "Country",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

##calculating the proportion of adopters with support per income level
epic_Solare_income_support_prop <- epic_Solare %>%
  group_by(Country_name, Income) %>%
  summarise(
    adopters = sum(C44_6 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_6 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support per income level
epic_Solare_income_support_prop_long <- epic_Solare_income_support_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##adoption of Solar panels for electricity per Country, Income Level and Government Support in proportions
barchart_Solare_income_support_prop <- ggplot(epic_Solare_income_support_prop_long, aes(x = as.factor(Income), 
                                                                                          y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = str_wrap("Adoption of Solar Panels for Electricity by Country and Income Level",50),
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

#------------------------- Solar water heating ---------------------------------
epic_Solarw <- epic_EET %>%
  filter(Solarw_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_7, C46_7, C45_7, Solarw_p)

##calculating the proportion of adopters with support
epic_Solarw_support_prop <- epic_Solarw %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(C44_7 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_7 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support
epic_Solarw_support_prop_long <- epic_Solarw_support_prop %>%
  select(Country_name, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##Adoption of Solar water heating per Country and Government Support in proportions
barchart_Solarw_support_prop <- ggplot(epic_Solarw_support_prop_long, 
                                       aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                           y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of Solar Water Heating by Country",50),
    x = "Country",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

##calculating the proportion of adopters with support per income level
epic_Solarw_income_support_prop <- epic_Solarw %>%
  group_by(Country_name, Income) %>%
  summarise(
    adopters = sum(C44_7 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_7 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support per income level
epic_Solarw_income_support_prop_long <- epic_Solarw_income_support_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##adoption of Solar water heating per Country, Income Level and Government Support in proportions
barchart_Solarw_income_support_prop <- ggplot(epic_Solarw_income_support_prop_long, aes(x = as.factor(Income), 
                                                                                        y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = str_wrap("Adoption of Solar Water Heating by Country and Income Level",50),
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

#----------------------------- Battery storage ---------------------------------
epic_Battery <- epic_EET %>%
  filter(Battery_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_8, C46_8, C45_8, Battery_p)

##calculating the proportion of adopters with support
epic_Battery_support_prop <- epic_Battery %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(C44_8 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_8 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support
epic_Battery_support_prop_long <- epic_Battery_support_prop %>%
  select(Country_name, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##Adoption of Battery storage per Country and Government Support in proportions
barchart_Battery_support_prop <- ggplot(epic_Battery_support_prop_long, 
                                       aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                           y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of Battery Storage by Country",50),
    x = "Country",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

##calculating the proportion of adopters with support per income level
epic_Battery_income_support_prop <- epic_Battery %>%
  group_by(Country_name, Income) %>%
  summarise(
    adopters = sum(C44_8 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_8 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support per income level
epic_Battery_income_support_prop_long <- epic_Battery_income_support_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##adoption of Battery storage per Country, Income Level and Government Support in proportions
barchart_Battery_income_support_prop <- ggplot(epic_Battery_income_support_prop_long, aes(x = as.factor(Income), 
                                                                                        y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = str_wrap("Adoption of Battery Storage by Country and Income Level",50),
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )


#--------------------------- Heat pump -----------------------------------------
epic_Pump <- epic_EET %>%
  filter(Pump_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_9, C46_9, C45_9, Pump_p)

##calculating the proportion of adopters with support
epic_Pump_support_prop <- epic_Pump %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(C44_9 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_9 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support
epic_Pump_support_prop_long <- epic_Pump_support_prop %>%
  select(Country_name, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##Adoption of Heat pumps per Country and Government Support in proportions
barchart_Pump_support_prop <- ggplot(epic_Pump_support_prop_long, 
                                        aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                            y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of Heat Pumps by Country",50),
    x = "Country",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

##calculating the proportion of adopters with support per income level
epic_Pump_income_support_prop <- epic_Pump %>%
  group_by(Country_name, Income) %>%
  summarise(
    adopters = sum(C44_9 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(C45_9 == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support per income level
epic_Pump_income_support_prop_long <- epic_Pump_income_support_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "Support",
                                 "proportion_support_not_received" = "No Support"))

##adoption of Heat pumps per Country, Income Level and Government Support in proportions
barchart_Pump_income_support_prop <- ggplot(epic_Pump_income_support_prop_long, aes(x = as.factor(Income), 
                                                                                          y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = str_wrap("Adoption of Heat Pumps by Country and Income Level",50),
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +  # y-axis from 0 to 1 in 0.25 steps
  theme_minimal() +
  scale_fill_manual(values = c("No Support" = "#8da0cb", "Support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

#-------------------------------------------------------------------------------
#---------------------------- 3. Saving Graphs ---------------------------------
#-------------------------------------------------------------------------------
ggsave("./output/output_desc_stats/barchart_Appl_income_support_prop.pdf", plot = barchart_Appl_income_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/output_desc_stats/barchart_Appl_support_prop.pdf", plot = barchart_Appl_support_prop, dpi = 300, scale = 1.2)

ggsave("./output/output_desc_stats/barchart_Battery_income_support_prop.pdf", plot = barchart_Battery_income_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/output_desc_stats/barchart_Battery_support_prop.pdf", plot = barchart_Battery_support_prop, dpi = 300, scale = 1.2)

ggsave("./output/output_desc_stats/barchart_highEET_income_support_prop.pdf", plot = barchart_highEET_income_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/output_desc_stats/barchart_highEET_support_prop.pdf", plot = barchart_highEET_support_prop, dpi = 300, scale = 1.2)

ggsave("./output/output_desc_stats/barchart_Pump_income_support_prop.pdf", plot = barchart_Pump_income_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/output_desc_stats/barchart_Pump_support_prop.pdf", plot = barchart_Pump_support_prop, dpi = 300, scale = 1.2)

ggsave("./output/output_desc_stats/barchart_Solare_income_support_prop.pdf", plot = barchart_Solare_income_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/output_desc_stats/barchart_Solare_support_prop.pdf", plot = barchart_Solare_support_prop, dpi = 300, scale = 1.2)

ggsave("./output/output_desc_stats/barchart_Solarw_income_support_prop.pdf", plot = barchart_Solarw_income_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/output_desc_stats/barchart_Solarw_support_prop.pdf", plot = barchart_Solarw_support_prop, dpi = 300, scale = 1.2)

ggsave("./output/output_desc_stats/barchart_Thermal_income_support_prop.pdf", plot = barchart_Thermal_income_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/output_desc_stats/barchart_Thermal_support_prop.pdf", plot = barchart_Thermal_support_prop, dpi = 300, scale = 1.2)

ggsave("./output/output_desc_stats/barchart_Windows_income_support_prop.pdf", plot = barchart_Windows_income_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/output_desc_stats/barchart_Windows_support_prop.pdf", plot = barchart_Windows_support_prop, dpi = 300, scale = 1.2)







