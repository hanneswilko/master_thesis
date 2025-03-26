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

#-------------------------------------------------------------------------------
#------------------------- 2. Descriptive Analysis -----------------------------
#-------------------------------------------------------------------------------

#--------------------------- Data wrangling EETs -------------------------------
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

#create variable with merged info on low, medium and high-cost EETs adoption
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

#create variable with merged info on medium and high-cost EETs government support for adoption
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

#--------------------------- low-cost EETs -------------------------------------
epic_lowEET <- epic_EET %>%
  filter(low_EET_possible != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_2, C46_2,
         low_EET, low_EET_possible)

#adoption of low-cost EET per Country and Income level in totals
barchart_lowEET_totals <- ggplot(epic_lowEET %>% filter(C44_2 == 1), aes(x = factor(Income))) +  
  geom_bar(position = "dodge", fill = "#8da0cb") +  
  facet_wrap(~Country_name) +  # By country  
  labs(
    title = "Adoption of Low-cost Energy-efficient Technology",
    x = "Income Level",
    y = "Number of Adopters"
  ) +  
  scale_y_continuous(
    breaks = seq(0, max(table(epic_lowEET$Income)), by = 50)  # Smaller steps for the y-axis  
  ) +  
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title  
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title  
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title  
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)  
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable  
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines  
    panel.grid.minor = element_blank(),  # Remove minor gridlines  
    panel.grid.major.x = element_blank()   # Remove vertical gridlines  
  )

#calculating proportions for low-cost EET
epic_lowEET_prop <- epic_lowEET %>%
  group_by(Country_name, Income) %>%
  summarise(
    total_adopters = sum(C44_2 == 1, na.rm = TRUE),
    total_households = n(),  # total number of households
    proportion_adopters = total_adopters / total_households
  ) %>%
  ungroup()

#adoption of low-cost EET per Country and Income level in proportions
barchart_lowEET_prop <- ggplot(epic_lowEET_prop, aes(x = factor(Income), y = proportion_adopters)) +  
  geom_bar(stat = "identity", position = "dodge", fill = "#8da0cb") +  
  facet_wrap(~Country_name) +  # By country  
  labs(
    title = "Adoption of low-cost Energy-efficient Technology",
    x = "Income Level",
    y = "Proportions of Adopters"
  ) +  
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis from 0 to 1  
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title  
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title  
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title  
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)  
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable  
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines  
    panel.grid.minor = element_blank(),  # Remove minor gridlines  
    panel.grid.major.x = element_blank()   # Remove vertical gridlines  
  )

#--------------------------- middle-cost EETs -------------------------------------
epic_middleEET <- epic_EET %>%
  filter(middle_EET_possible != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_1, C46_1, C45_1,
         middle_EET, middle_EET_possible, middle_EET_gov_support)

#adoption of middle-cost EET per Country and Income level in totals
barchart_middleEET_totals <- ggplot(epic_middleEET %>% filter(C44_1 == 1), aes(x = factor(Income))) +  
  geom_bar(position = "dodge", fill = "#8da0cb") +  # Bar plot with dodge position  
  facet_wrap(~Country_name) +  # By country  
  labs(
    title = "Adoption of middle-cost Energy-efficient Technology",
    x = "Income Level",
    y = "Number of Adopters"
  ) +  
  scale_y_continuous(
    breaks = seq(0, max(table(epic_middleEET$Income)), by = 50)  # Smaller steps for the y-axis  
  ) +  
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title  
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title  
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title  
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)  
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable  
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines  
    panel.grid.minor = element_blank(),  # Remove minor gridlines  
    panel.grid.major.x = element_blank()   # Remove vertical gridlines  
  )

#calculating proportions for middle-cost EET
epic_middleEET_prop <- epic_middleEET %>%
  group_by(Country_name, Income) %>%
  summarise(
    total_adopters = sum(C44_1 == 1, na.rm = TRUE),
    total_households = n(),  # total number of households
    proportion_adopters = total_adopters / total_households
  ) %>%
  ungroup()

#adoption of middle-cost EET per Country and Income level in proportions
barchart_middleEET_prop <- ggplot(epic_middleEET_prop, aes(x = factor(Income), y = proportion_adopters)) +  
  geom_bar(stat = "identity", position = "dodge", fill = "#8da0cb") +  
  facet_wrap(~Country_name) +  # By country  
  labs(
    title = "Adoption of middle-cost Energy-efficient Technology",
    x = "Income Level",
    y = "Proportions of Adopters"
  ) +  
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis from 0 to 1  
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title  
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title  
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title  
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)  
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable  
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines  
    panel.grid.minor = element_blank(),  # Remove minor gridlines  
    panel.grid.major.x = element_blank()   # Remove vertical gridlines  
  )

#Government support
##calculating the proportion of adopters with support
epic_middleEET_govsup_prop <- epic_middleEET %>%
  group_by(Country_name, Income) %>%
  summarise(
    total_adopters = sum(C44_1 == 1, na.rm = TRUE),
    total_households = n(),
    proportion_adopters = total_adopters / total_households,
    gov_support_received = sum(middle_EET_gov_support == 1, na.rm = TRUE),
    gov_support_not_received = total_adopters - gov_support_received,
    proportion_gov_support_received = gov_support_received / total_adopters,
    proportion_gov_support_not_received = gov_support_not_received / total_adopters
  ) %>%
  ungroup()

##creating long table for total of adopters with support
epic_middleEET_govsup_totals <- epic_middleEET_govsup_prop %>%
  select(Country_name, Income, gov_support_received, gov_support_not_received) %>%
  tidyr::pivot_longer(cols = c(gov_support_received, gov_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Total_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "gov_support_received" = "Received Support",
                                 "gov_support_not_received" = "No Support"))

##adoption of middle-cost EET per Country, Income level and Government support in totals
barchart_middleEET_govsup_totals <- ggplot(epic_middleEET_govsup_totals, 
                                           aes(x = as.factor(Income), y = Total_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = "Adoption of middle-cost Energy-efficient Technology by Government Support",
    x = "Income Level",
    y = "Number of Adopters",
    fill = "Government Support"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Received Support" = "#fc8d62", "No Support" = "#8da0cb")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )

##creating long table for proportions of adopters with support
epic_middleEET_govsup_prop_long <- epic_middleEET_govsup_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_gov_support_received, proportion_gov_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_gov_support_received, proportion_gov_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_gov_support_received" = "Received Support",
                                 "proportion_gov_support_not_received" = "No Support"))


##adoption of middle-cost EET per Country, Income level and Government support in proportions
barchart_middleEET_govsup_prop <- ggplot(epic_middleEET_govsup_prop_long, aes(x = as.factor(Income), 
                                                                              y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = "Adoption of middle-cost Energy-efficient Technology by Government Support",
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Received Support" = "#fc8d62", "No Support" = "#8da0cb")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )

#--------------------------- high-cost EETs -------------------------------------
epic_highEET <- epic_EET %>%
  filter(high_EET_possible != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_3, C44_4, C44_6, C44_9,
         C46_3, C46_4, C46_6, C46_9, C45_3, C45_4, C45_6, C45_9,
         high_EET, high_EET_possible, high_EET_gov_support)

#adoption of high-cost EET per Country and Income level in totals
barchart_highEET_totals <- ggplot(epic_highEET %>% filter(high_EET == 1), aes(x = factor(Income))) +  
  geom_bar(position = "dodge", fill = "#8da0cb") +  # Bar plot with dodge position  
  facet_wrap(~Country_name) +  # By country  
  labs(
    title = "Adoption of high-cost Energy-efficient Technology",
    x = "Income Level",
    y = "Number of Adopters"
  ) +  
  scale_y_continuous(
    breaks = seq(0, max(table(epic_highEET$Income)), by = 50)  # Smaller steps for the y-axis  
  ) +  
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title  
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title  
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title  
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)  
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable  
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines  
    panel.grid.minor = element_blank(),  # Remove minor gridlines  
    panel.grid.major.x = element_blank()   # Remove vertical gridlines  
  )

#calculating proportions for high-cost EET
epic_highEET_prop <- epic_highEET %>%
  group_by(Country_name, Income) %>%
  summarise(
    total_adopters = sum(high_EET == 1, na.rm = TRUE),
    total_households = n(),  # total number of households
    proportion_adopters = total_adopters / total_households
  ) %>%
  ungroup()

#adoption of high-cost EET per Country and Income level in proportions
barchart_highEET_prop <- ggplot(epic_highEET_prop, aes(x = factor(Income), y = proportion_adopters)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#8da0cb") +  
  facet_wrap(~Country_name) +  # By country
  labs(
    title = "Adoption of high-cost Energy-efficient Technology",
    x = "Income Level",
    y = "Proportions of Adopters"
  ) +  
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis from 0 to 1
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )

#Government support
##calculating the proportion of adopters with support
epic_highEET_govsup_prop <- epic_highEET %>%
  group_by(Country_name, Income) %>%
  summarise(
    total_adopters = sum(high_EET == 1, na.rm = TRUE),
    total_households = n(),
    proportion_adopters = total_adopters / total_households,
    gov_support_received = sum(high_EET_gov_support == 1, na.rm = TRUE),
    gov_support_not_received = total_adopters - gov_support_received,
    proportion_gov_support_received = gov_support_received / total_adopters,
    proportion_gov_support_not_received = gov_support_not_received / total_adopters
  ) %>%
  ungroup()

##creating long table for total of adopters with support
epic_highEET_govsup_totals <- epic_highEET_govsup_prop %>%
  select(Country_name, Income, gov_support_received, gov_support_not_received) %>%
  tidyr::pivot_longer(cols = c(gov_support_received, gov_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Total_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "gov_support_received" = "Received Support",
                                 "gov_support_not_received" = "No Support"))

##adoption of high-cost EET per Country, Income level and Government support in totals
barchart_highEET_govsup_totals <- ggplot(epic_highEET_govsup_totals, 
                                         aes(x = as.factor(Income), y = Total_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = "Adoption of high-cost Energy-efficient Technology by Government Support",
    x = "Income Level",
    y = "Number of Adopters",
    fill = "Government Support"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Received Support" = "#fc8d62", "No Support" = "#8da0cb")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bold and centered title
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)
    axis.text.x = element_text(angle = 0, hjust = 1),  # Keep x-axis labels readable
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )


##creating long table for proportions of adopters with support
epic_highEET_govsup_prop_long <- epic_highEET_govsup_prop %>%
  select(Country_name, Income, proportion_adopters, proportion_gov_support_received, proportion_gov_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_gov_support_received, proportion_gov_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_gov_support_received" = "Received Support",
                                 "proportion_gov_support_not_received" = "No Support"))

##adoption of high-cost EET per Country, Income level and Government support in proportions
barchart_highEET_govsup_prop <- ggplot(epic_highEET_govsup_prop_long, aes(x = as.factor(Income), 
                                                                          y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country_name) +
  labs(
    title = "Adoption of high-cost Energy-efficient Technology by Government Support",
    x = "Income Level",
    y = "Proportions of Adopters",
    fill = "Government Support"
  ) +
  scale_y_continuous(limits = c(0, 1)) +  # Set y-axis from 0 to 1
  theme_minimal() +
  scale_fill_manual(values = c("Received Support" = "#fc8d62", "No Support" = "#8da0cb")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Increase size, bold, and center title
    axis.title.x = element_text(size = 12, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Bold y-axis title
    legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
    strip.text.x = element_text(size = 10, face = "bold"),  # Bold facet labels (country names)
    axis.text.x = element_text(angle = 0, hjust = 1),  # Rotate x-axis labels for better fit
    panel.grid.major.y = element_line(color = "gray", size = 0.3),  # Add horizontal gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank()   # Remove vertical gridlines
  )




#-------------------------------------------------------------------------------
#---------------------------- 3. Saving Graphs ---------------------------------
#-------------------------------------------------------------------------------
ggsave("./output/barchart_lowEET_totals.png", plot = barchart_lowEET_totals, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_lowEET_prop.png", plot = barchart_lowEET_prop, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_middleEET_totals.png", plot = barchart_middleEET_totals, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_middleEET_prop.png", plot = barchart_middleEET_prop, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_middleEET_govsup_totals.png", plot = barchart_middleEET_govsup_totals, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_middleEET_govsup_prop.png", plot = barchart_middleEET_govsup_prop, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_highEET_totals.png", plot = barchart_highEET_totals, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_highEET_prop.png", plot = barchart_highEET_prop, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_highEET_govsup_prop.png", plot = barchart_highEET_govsup_prop, width = 8, height = 6, dpi = 300)
ggsave("./output/barchart_highEET_govsup_totals.png", plot = barchart_highEET_govsup_totals, width = 8, height = 6, dpi = 300)






