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
         S18, B23_1, B31_1, B31_3, B31_5, B31_6, B31_7, B31_8, C49_1, C49_2, C49_3, C44_1,
         C44_2, C44_3, C44_4, C44_6, C44_9, C45_1, C45_3, C45_4, C45_6, C45_9, C46_1,
         C46_2, C46_3, C46_4, C46_6, C46_9, C47_2, C47_6)

attach(epic)

#-------------------------------------------------------------------------------
#------------------------- 2. Descriptive Analysis -----------------------------
#-------------------------------------------------------------------------------

#-------------- C44_6: solar panels for electricity ----------------------------
epic_C46_6exc <- epic %>%
  filter(is.na(C46_6) | C46_6 != 4)  # Keep NAs and exclude only C46_6 == 4

epic_C46_6exc_income <- epic_C46_6exc %>% 
  filter(C44_6 == 1) %>%  # Only consider households that adopted solar panels
  group_by(Country_name, Income) %>% 
  summarize(count_1 = n(), .groups = "drop") %>%  # Count number of adoptions per country and income level
  left_join(
    epic_C46_6exc %>%  # Use the filtered epic_C46_6exc to calculate total counts
      group_by(Country_name, Income) %>% 
      summarize(total_count = n(), .groups = "drop"), 
    by = c("Country_name", "Income")
  ) %>%
  mutate(proportion_1 = round((count_1 / total_count) * 100, 2))  # Calculate proportion of adoption

#bar plot adoption of EET appliances per income level and country
ggplot(epic_C46_6exc_income, aes(x = factor(Income, levels = c(1, 2, 3, 4, 5), ordered = TRUE), 
                              y = proportion_1, 
                              fill = factor(Income, levels = c(1, 2, 3, 4, 5), ordered = TRUE))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Country_name) +  # Create a facet for each country
  labs(title = "Proportion of C44_6 == 1 by Income Level and Country",
       x = "Income Level",
       y = "Proportion of C44_6 == 1 (%)") +
  scale_fill_brewer(palette = "Set2") +  # Optional: Use a color palette for better differentiation
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank())  # Remove legend title if desired

# Recode S5 and S18
epic_C46_6exc_recoded <- epic_C46_6exc %>% 
  mutate(
    # Recode S5 for homeownership (1 = yes, 0 = no)
    S5 = case_when(
      S5 == 1 ~ 1,  # Homeownership
      S5 %in% c(2, 3) ~ 0,  # No Homeownership
      TRUE ~ NA_real_  # Handle any unexpected values (replace with NA)
    ),
    
    # Recode S18 for housing type (1 = detached house, 0 = not detached house)
    S18 = case_when(
      S18 %in% c(3, 4) ~ 1,  # Detached House
      TRUE ~ 0  # Not Detached House
    )
  )

# Filter the dataset for households that have adopted solar panels (C44_6 == 1)
epic_adopted <- epic_C46_6exc_recoded %>%
  filter(C44_6 == 1)

# Summarize the data by country, income, homeownership, and housing type
summary_table <- epic_adopted %>%
  group_by(Country_name, Income, S5, S18) %>%
  summarize(
    count_adopted = n(),  # Count of households that adopted solar panels
    .groups = "drop"
  ) %>%
  left_join(
    epic_C46_6exc_recoded %>%
      filter(!is.na(C44_6)) %>%
      group_by(Country_name, Income, S5, S18) %>%
      summarize(total_count = n(), .groups = "drop"),
    by = c("Country_name", "Income", "S5", "S18")
  ) %>%
  mutate(
    proportion_adopted = round((count_adopted / total_count) * 100, 2)
  )

# View the summary table
summary_table

# Case of Sweden
summary_table_se <- summary_table %>%
  filter(Country_name == "SE")

ggplot(summary_table_se, aes(x = factor(Income), y = proportion_adopted, fill = factor(S5))) + 
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with dodge position
  facet_wrap(~S18) +  # Separate the plots by Housing Type (S18)
  labs(
    title = "Proportion of Solar Panel Adoption in Sweden by Income, Homeownership, and Housing Type",
    x = "Income Level",
    y = "Proportion Adopted (%)",
    fill = "Homeownership (1 = Yes, 0 = No)"
  ) +
  scale_fill_manual(
    values = c("0" = "orange", "1" = "blue"),  # Custom colors for homeownership (0 = No, 1 = Yes)
    labels = c("No Homeownership", "Homeownership")  # Labels for the legend
  ) +  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

summary_table %>%
  filter(Country_name %in% "SE") %>%
  View()

#--------------------------- Data wrangling EETs -------------------------------
#C44_1: Appliances
#C44_2: LEDs
#C44_3: Windows
#C44_4: Thermal insulation
#C44_6: Solar panels electricity
#C44_9: Heat pumps

#C46_1: Why not Appliances
#C46_2: Why not LEDs
#C46_3: Why not Windows
#C46_4: Why not Thermal insulation
#C46_6: Why not Solar panels electricity
#C46_9: Why not Heat pumps

epic_EET <- epic %>%
  mutate(
    # Low-cost EET adoption (LEDs)
    low_EET = ifelse(C44_2 == 1, 1, 0),
    
    # Middle-cost EET adoption (Highly energy-efficient appliances)
    middle_EET = ifelse(C44_1 == 1, 1, 0),
    
    # High-cost EET adoption (Energy-efficient windows, Thermal insulation, Solar panels, Heat pumps)
    high_EET = ifelse(C44_3 == 1 | C44_4 == 1 | C44_6 == 1 | C44_9 == 1, 1, 0),
    
    # Low-cost EET adoption (LEDs) not possible
    low_EET_possible = case_when(
      C44_2 == 1 ~ 1, #if adopted = possible
      C46_2 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    
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

#--------------------------- low-cost EETs -------------------------------------
epic_lowEET <- epic_EET %>%
  filter(low_EET_possible != 0) %>%
  select(Country_code, Country_name, Income, S5, S18, C44_2, C46_2,
         low_EET, low_EET_possible)

#Adoption (in total) of LEDs per country and income level
lowEET_country_income <- ggplot(epic_lowEET %>% filter(C44_2 == 1), aes(x = factor(Income), fill = factor(Income))) + 
  geom_bar(position = "dodge") +  # Bar plot with dodge position
  facet_wrap(~Country_name) +  # By country
  labs(
    title = "Adoption of LEDs per Country and Income Level",
    x = "Income Level",
    y = "Adopters",
    fill = "Income Level"
  ) + 
  scale_fill_brewer(palette = "Set2") +  # Custom color palette
  scale_y_continuous(
    breaks = seq(0, max(table(epic_lowEET$Income)), by = 50)  # Smaller steps for the y-axis
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),  # Rotate x-axis labels for better fit
    strip.text.x = element_text(size = 8),  # Adjust facet label size
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )

#calculate the proportion of adopters
epic_lowEET_proportion <- epic_lowEET %>%
  group_by(Country_name, Income) %>%
  summarise(
    total_adopters = sum(C44_2 == 1, na.rm = TRUE),
    total_households = n(),  # total number of households
    proportion_adopters = total_adopters / total_households
  ) %>%
  ungroup()

#Adoption (in %) of LEDs per country and income level
lowEET_country_income_proportion <- ggplot(epic_lowEET_proportion, aes(x = factor(Income), y = proportion_adopters, fill = factor(Income))) + 
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with dodge position
  facet_wrap(~Country_name) +  # By country
  labs(
    title = "Proportion of LED Adoption per Country and Income Level",
    x = "Income Level",
    y = "Proportion of Adopters",
    fill = "Income Level"
  ) + 
  scale_fill_brewer(palette = "Set2") +  # Custom color palette
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis from 0 to 1
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),  # Rotate x-axis labels for better fit
    strip.text.x = element_text(size = 8),  # Adjust facet label size
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )

#--------------------------- middle-cost EETs -------------------------------------
attr(epic_raw$C45_1, "label")
attr(epic_raw$C45_1, "labels")

#select variables presenting middle-cost EETs
epic_middleEET <- epic_EET %>%
  filter(middle_EET_possible != 0) %>%
  select(Country_code, Country_name, Income, S5, S18, C44_1, C46_1, C45_1,
         middle_EET, middle_EET_possible)

'#transform government support binary 1/0
epic_middleEET <- epic_middleEET %>%
  mutate(gov_support = case_when(
    C45_1 == 1 ~ 1,  # Government support received
    C45_1 %in% c(2, 99) ~ 0,  # No support
    TRUE ~ NA  #else 
  ))
'
#Adoption of Appliances per country and income level
middleEET_country_income <- ggplot(epic_middleEET %>% filter(C44_1 == 1), aes(x = factor(Income), fill = factor(Income))) + 
  geom_bar(position = "dodge") +  # Bar plot with dodge position
  facet_wrap(~Country_name) +  # By country
  labs(
    title = "Adoption of Appliances per Country and Income Level",
    x = "Income Level",
    y = "Adopters",
    fill = "Income Level"
  ) + 
  scale_fill_brewer(palette = "Set2") +  # Custom color palette
  scale_y_continuous(
    breaks = seq(0, max(table(epic_middleEET$Income)), by = 50)  # Smaller steps for the y-axis
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),  # Rotate x-axis labels for better fit
    strip.text.x = element_text(size = 8),  # Adjust facet label size
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )

#calculate the proportion of adopters
epic_middleEET_proportion <- epic_middleEET %>%
  group_by(Country_name, Income) %>%
  summarise(
    total_adopters = sum(C44_1 == 1, na.rm = TRUE),
    total_households = n(),  # total number of households
    proportion_adopters = total_adopters / total_households
  ) %>%
  ungroup()

#Adoption (in %) of Appliances per country and income level
middleEET_country_income_proportion <- ggplot(epic_middleEET_proportion, aes(x = factor(Income), y = proportion_adopters, fill = factor(Income))) + 
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with dodge position
  facet_wrap(~Country_name) +  # By country
  labs(
    title = "Proportion of Appliances Adoption per Country and Income Level",
    x = "Income Level",
    y = "Proportion of Adopters",
    fill = "Income Level"
  ) + 
  scale_fill_brewer(palette = "Set2") +  # Custom color palette
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis from 0 to 1
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),  # Rotate x-axis labels for better fit
    strip.text.x = element_text(size = 8),  # Adjust facet label size
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )
'
#Government support thereof
ggplot(epic_middleEET %>% filter(middle_EET == 1), aes(x = factor(Income), fill = factor(gov_support))) + 
  geom_bar(position = "stack") +  # Stacked bar chart to show total adoption with/without gov support
  facet_wrap(~Country_name) +  # By country
  labs(
    title = "Interaction Between Income Level and Government Support on Adoption",
    x = "Income Level",
    y = "Count of Adopters",
    fill = "Government Support"
  ) +
  scale_fill_manual(values = c("gray", "blue")) +  # Different colors for support and no support
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.x = element_text(size = 8),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank()
  )
'

                    










