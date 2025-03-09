#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: EPIC 2022
#-----------------------------Loading Data -------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr","jsonlite")

##OECD carbon rate
epic_data <- read.csv("./processed_data/epic_data_var_interest.csv")
glimpse(epic_data)
View(epic_data)

#-------------------------------------------------------------------------------
#-------- Variables of interest for core message of research project -----------
#-------------------------------------------------------------------------------
###Subsample for HHs where adoption of EETs is feasible --> see Hassett et al. (2024) pp.43

###country - ref area: Country
###weights: weight, weight_2
###income: Income (categorized)
###Age: Age_cat (categorized)

###housing characteristics
###home ownership: S5, 1 = residenced owned, 2 = rented, 3 = another type
###detached housing: S18, 1 = apartment, 2 = apartment, 3 = detached house, 4 = semi-detached, 89 = other

###Environmental preferences
###environmental attitude: B23_1, 1 = not important - 5 = very important, 999999 = NA
###Environmental impacts are overstated: B31_1, 1 = strongly disagree - 4 = agree
###Protecting environment boosts economy: B31_3, 1 = strongly disagree - 4 = agree
###Environmental issues resolved through public policies: B31_5, 1 = strongly disagree - 4 = agree
###Environmental policies should not cost extra money: B31_6, 1 = strongly disagree - 4 = agree
###Environmental issues resolved through technological progress: B31_7, 1 = strongly disagree - 4 = agree
###Environmental issues resolved through individuals voluntarily changing behaviour: B31_8, 1 = strongly disagree - 4 = agree

###Support of policy measures
###Subsidies for renovation or EET adoption: C49_1, 1 = strongly against - 5 = strongly support
###Taxing use of energy or purchase highly energy-consuming appliances: C49_2, 1 = strongly against - 5 = strongly support
###Introducing energy efficiency standards for appliances and buildings: C49_3, 1 = strongly against - 5 = strongly support

###EET adoption
###Highly energy-efficient appliances: C44_1, 1 = Yes, 2 = No, 99 = Don't know
###Low-energy light bulbs: C44_2, 1 = Yes, 2 = No, 99 = Don't know
###Energy-efficient windows: C44_3, 1 = Yes, 2 = No, 99 = Don't know
###Thermal insulation of walls/roof/floor: C44_4, 1 = Yes, 2 = No, 99 = Don't know
###Solar panels for electricity: C44_6, 1 = Yes, 2 = No, 99 = Don't know
###Solar water heating: C44_7, 1 = Yes, 2 = No, 99 = Don't know
###Heat pumps: C44_9, 1 = Yes, 2 = No, 99 = Don't know

###government support
###Highly energy-efficient appliances: C45_1, 1 = Yes, 2 = No, 99 = Don't know
###no financial support for LEDs
###Energy-efficient windows: C45_3, 1 = Yes, 2 = No, 99 = Don't know
###Thermal insulation of walls/roof/floor: C45_4, 1 = Yes, 2 = No, 99 = Don't know
###Solar panels for electricity: C45_6, 1 = Yes, 2 = No, 99 = Don't know
###Solar water heating: C45_7, 1 = Yes, 2 = No, 99 = Don't know
###Heat pumps: C45_9, 1 = Yes, 2 = No, 99 = Don't know

###Why NOT EET adoption
###Highly energy-efficient appliances: C46_1, 1 = installed >10Y, 2 = planning to 2/3Y, 3 = interest but not affordable, 4 = not possible, 5 = not interested, 6 = not aware
###no info about LEDs
###Energy-efficient windows: C46_2, 1 = installed >10Y, 2 = planning to 2/3Y, 3 = interest but not affordable, 4 = not possible, 5 = not interested, 6 = not aware
###Thermal insulation of walls/roof/floor: C46_3, 1 = installed >10Y, 2 = planning to 2/3Y, 3 = interest but not affordable, 4 = not possible, 5 = not interested, 6 = not aware
###Solar panels for electricity: C46_5, 1 = installed >10Y, 2 = planning to 2/3Y, 3 = interest but not affordable, 4 = not possible, 5 = not interested, 6 = not aware
###Solar water heating: C46_6, 1 = installed >10Y, 2 = planning to 2/3Y, 3 = interest but not affordable, 4 = not possible, 5 = not interested, 6 = not aware
###Heat pumps: C46_8, 1 = installed >10Y, 2 = planning to 2/3Y, 3 = interest but not affordable, 4 = not possible, 5 = not interested, 6 = not aware

###Important drivers for reducing energy consumption
###Higher energy prices: C47_2, 1 = not all - 5 = very important, 888888 = don't know
###Reduced cost of energy-efficient devices and renovation: C47_6, 1 = not all - 5 = very important, 888888 = don't know

#-------------------------------------------------------------------------------
#----------------------- Subsample as baseline subset --------------------------
#-------------------------------------------------------------------------------
glimpse(epic_data)

epic <- epic_data %>%
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

glimpse(epic)
View(epic)

#-------------------------------------------------------------------------------
#------------------------- Sociodemgrapic Variables ----------------------------
#-------------------------------------------------------------------------------
##Age
unique(epic$Age_cat)
summary(epic$Age_cat)
summary(as.factor(epic$Age_cat))
hist(epic$Age_cat)

epic_age <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, Age_cat)

epic_age %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(Age_cat, na.rm = TRUE),
    Q1 = quantile(Age_cat, 0.25, na.rm = TRUE),
    Median = median(Age_cat, na.rm = TRUE),
    Mean = mean(Age_cat, na.rm = TRUE),
    Q3 = quantile(Age_cat, 0.75, na.rm = TRUE),
    Max = max(Age_cat, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_age, aes(x = as.factor(Age_cat), fill = as.factor(Age_cat))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "Age_cat by Country",
    x = "Age_cat (1-5)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

##Income
unique(epic$Income)
summary(epic$Income)
summary(as.factor(epic$Income))
hist(epic$Income)

epic_Income <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, Income)

epic_Income %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(Income, na.rm = TRUE),
    Q1 = quantile(Income, 0.25, na.rm = TRUE),
    Median = median(Income, na.rm = TRUE),
    Mean = mean(Income, na.rm = TRUE),
    Q3 = quantile(Income, 0.75, na.rm = TRUE),
    Max = max(Income, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_Income, aes(x = as.factor(Income), fill = as.factor(Income))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "Income by Country",
    x = "Income (1-5)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

#-------------------------------------------------------------------------------
#------------------------- Housing characteristics -----------------------------
#-------------------------------------------------------------------------------
##home ownership S5: 1 = residenced owned, 2 = rented, 3 = another type 
unique(epic$S5)
summary(epic$S5)
summary(as.factor(epic$S5))
hist(epic$S5)

epic_S5 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, S5)

epic_S5 %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(S5, na.rm = TRUE),
    Q1 = quantile(S5, 0.25, na.rm = TRUE),
    Median = median(S5, na.rm = TRUE),
    Mean = mean(S5, na.rm = TRUE),
    Q3 = quantile(S5, 0.75, na.rm = TRUE),
    Max = max(S5, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_S5, aes(x = as.factor(S5), fill = as.factor(S5))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "S5 by Country",
    x = "S5 (1-3)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

##detached housing S18: 1 = apartment, 2 = apartment, 3 = detached house, 4 = semi-detached, 89 = other
unique(epic$S18)
summary(epic$S18[epic$S18 != 89])
summary(as.factor(epic$S18))
hist(epic$S18[epic$S18 != 89])

epic_S18 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, S18)

epic_S18 %>%
  filter(S18 != 89) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(S18, na.rm = TRUE),
    Q1 = quantile(S18, 0.25, na.rm = TRUE),
    Median = median(S18, na.rm = TRUE),
    Mean = mean(S18, na.rm = TRUE),
    Q3 = quantile(S18, 0.75, na.rm = TRUE),
    Max = max(S18, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_S18, aes(x = as.factor(S18), fill = as.factor(S18))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "S18 by Country",
    x = "S5 (1-4, 89)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

#-------------------------------------------------------------------------------
#------------------------- Environmental preferences ---------------------------
#-------------------------------------------------------------------------------
##Env. attitude B23_1: 1 = Not important - 5 = Very important, 999999 = NA
unique(epic$B23_1)
summary(epic$B23_1[epic$B23_1 != 999999])
summary(as.factor(epic$B23_1))
hist(epic$B23_1[epic$B23_1 != 999999])

epic_B23_1 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, B23_1)

epic_B23_1 %>%
  filter(B23_1 != 999999) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(B23_1, na.rm = TRUE),
    Q1 = quantile(B23_1, 0.25, na.rm = TRUE),
    Median = median(B23_1, na.rm = TRUE),
    Mean = mean(B23_1, na.rm = TRUE),
    Q3 = quantile(B23_1, 0.75, na.rm = TRUE),
    Max = max(B23_1, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_B23_1, aes(x = as.factor(B23_1), fill = as.factor(B23_1))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "B23_1 by Country",
    x = "B23_1 (1-5, 999999)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

##Environmental impacts are overstated B31_1: 1 = Strongly disagree – 5 = Strongly agree, 999999 = NA
unique(epic$B31_1)
summary(epic$B31_1[epic$B31_1 != 999999])
summary(as.factor(epic$B31_1))
hist(epic$B31_1[epic$B31_1 != 999999])

epic_B31_1 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, B31_1)

epic_B31_1 %>%
  filter(B31_1 != 999999) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(B31_1, na.rm = TRUE),
    Q1 = quantile(B31_1, 0.25, na.rm = TRUE),
    Median = median(B31_1, na.rm = TRUE),
    Mean = mean(B31_1, na.rm = TRUE),
    Q3 = quantile(B31_1, 0.75, na.rm = TRUE),
    Max = max(B31_1, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_B31_1, aes(x = as.factor(B31_1), fill = as.factor(B31_1))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "B31_1 by Country",
    x = "B31_1 (1-5, 999999)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

##Protecting the environment boosts the economy B31_3: 1 = Strongly disagree – 5 = Strongly agree, 999999 = NA
unique(epic$B31_3)
summary(epic$B31_3[epic$B31_3 != 999999])
summary(as.factor(epic$B31_3))
hist(epic$B31_3[epic$B31_3 != 999999])

epic_B31_3 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, B31_3)

epic_B31_3 %>%
  filter(B31_3 != 999999) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(B31_3, na.rm = TRUE),
    Q1 = quantile(B31_3, 0.25, na.rm = TRUE),
    Median = median(B31_3, na.rm = TRUE),
    Mean = mean(B31_3, na.rm = TRUE),
    Q3 = quantile(B31_3, 0.75, na.rm = TRUE),
    Max = max(B31_3, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_B31_3, aes(x = as.factor(B31_3), fill = as.factor(B31_3))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "B31_3 by Country",
    x = "B31_3 (1-5, 999999)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

##Environmental issues resolved through public policies B31_5: 1 = Strongly disagree – 5 = Strongly agree, 999999 = NA
unique(epic$B31_5)
summary(epic$B31_5[epic$B31_5 != 999999])
summary(as.factor(epic$B31_5))
hist(epic$B31_5[epic$B31_5 != 999999])

epic_B31_5 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, B31_5)

epic_B31_5 %>%
  filter(B31_5 != 999999) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(B31_5, na.rm = TRUE),
    Q1 = quantile(B31_5, 0.25, na.rm = TRUE),
    Median = median(B31_5, na.rm = TRUE),
    Mean = mean(B31_5, na.rm = TRUE),
    Q3 = quantile(B31_5, 0.75, na.rm = TRUE),
    Max = max(B31_5, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_B31_5, aes(x = as.factor(B31_5), fill = as.factor(B31_5))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "B31_5 by Country",
    x = "B31_5 (1-5, 999999)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

##Environmental policies should not cost extra money B31_6: 1 = Strongly disagree – 5 = Strongly agree, 999999 = NA
unique(epic$B31_6)
summary(epic$B31_6[epic$B31_6 != 999999])
summary(as.factor(epic$B31_6))
hist(epic$B31_6[epic$B31_6 != 999999])

epic_B31_6 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, B31_6)

epic_B31_6 %>%
  filter(B31_6 != 999999) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(B31_6, na.rm = TRUE),
    Q1 = quantile(B31_6, 0.25, na.rm = TRUE),
    Median = median(B31_6, na.rm = TRUE),
    Mean = mean(B31_6, na.rm = TRUE),
    Q3 = quantile(B31_6, 0.75, na.rm = TRUE),
    Max = max(B31_6, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_B31_6, aes(x = as.factor(B31_6), fill = as.factor(B31_6))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "B31_6 by Country",
    x = "B31_6 (1-5, 999999)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

##Environmental issues resolved through technological progress B31_7: 1 = Strongly disagree – 5 = Strongly agree, 999999 = NA
unique(epic$B31_7)
summary(epic$B31_7[epic$B31_7 != 999999])
summary(as.factor(epic$B31_7))
hist(epic$B31_7[epic$B31_7 != 999999])

epic_B31_7 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, B31_7)

epic_B31_7 %>%
  filter(B31_7 != 999999) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(B31_7, na.rm = TRUE),
    Q1 = quantile(B31_7, 0.25, na.rm = TRUE),
    Median = median(B31_7, na.rm = TRUE),
    Mean = mean(B31_7, na.rm = TRUE),
    Q3 = quantile(B31_7, 0.75, na.rm = TRUE),
    Max = max(B31_7, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_B31_7, aes(x = as.factor(B31_7), fill = as.factor(B31_7))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "B31_7 by Country",
    x = "B31_7 (1-5, 999999)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

##Environmental issues resolved through individuals voluntarily changing behavior B31_8: 1 = Strongly disagree – 5 = Strongly agree, 999999 = NA
unique(epic$B31_8)
summary(epic$B31_8[epic$B31_8 != 999999])
summary(as.factor(epic$B31_8))
hist(epic$B31_8[epic$B31_8 != 999999])

epic_B31_8 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, B31_8)

epic_B31_8 %>%
  filter(B31_8 != 999999) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(B31_8, na.rm = TRUE),
    Q1 = quantile(B31_8, 0.25, na.rm = TRUE),
    Median = median(B31_8, na.rm = TRUE),
    Mean = mean(B31_8, na.rm = TRUE),
    Q3 = quantile(B31_8, 0.75, na.rm = TRUE),
    Max = max(B31_8, na.rm = TRUE),
    Count = n()
  )

ggplot(epic_B31_8, aes(x = as.factor(B31_8), fill = as.factor(B31_8))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "B31_8 by Country",
    x = "B31_8 (1-5, 999999)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

#-------------------------------------------------------------------------------
#------------------------- Support of policy measures --------------------------
#-------------------------------------------------------------------------------
###Subsidies for renovation or EET adoption C49_1: 1 = strongly against - 5 = strongly support
unique(epic$C49_1)
summary(epic$C49_1)
summary(as.factor(epic$C49_1))
hist(epic$C49_1)

epic_C49_1 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C49_1)

epic_C49_1 %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C49_1, na.rm = TRUE),
    Q1 = quantile(C49_1, 0.25, na.rm = TRUE),
    Median = median(C49_1, na.rm = TRUE),
    Mean = mean(C49_1, na.rm = TRUE),
    Q3 = quantile(C49_1, 0.75, na.rm = TRUE),
    Max = max(C49_1, na.rm = TRUE),
    Count = n()
  )

epic_C49_1 %>%
  drop_na(C49_1) %>%
  ggplot(aes(x = as.factor(C49_1), fill = as.factor(C49_1))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C49_1 by Country",
    x = "C49_1 (1-5)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

###Taxing use of energy or purchase highly energy-consuming appliances C49_2: 1 = strongly against - 5 = strongly support
unique(epic$C49_2)
summary(epic$C49_2)
summary(as.factor(epic$C49_2))
hist(epic$C49_2)

epic_C49_2 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C49_2)

epic_C49_2 %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C49_2, na.rm = TRUE),
    Q1 = quantile(C49_2, 0.25, na.rm = TRUE),
    Median = median(C49_2, na.rm = TRUE),
    Mean = mean(C49_2, na.rm = TRUE),
    Q3 = quantile(C49_2, 0.75, na.rm = TRUE),
    Max = max(C49_2, na.rm = TRUE),
    Count = n()
  )

epic_C49_2 %>%
  drop_na(C49_2) %>%
  ggplot(aes(x = as.factor(C49_2), fill = as.factor(C49_2))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C49_2 by Country",
    x = "C49_2 (1-5)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

###Introducing energy efficiency standards for appliances and buildings C49_3: 1 = strongly against - 5 = strongly support
unique(epic$C49_3)
summary(epic$C49_3)
summary(as.factor(epic$C49_3))
hist(epic$C49_3)

epic_C49_3 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C49_3)

epic_C49_3 %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C49_3, na.rm = TRUE),
    Q1 = quantile(C49_3, 0.25, na.rm = TRUE),
    Median = median(C49_3, na.rm = TRUE),
    Mean = mean(C49_3, na.rm = TRUE),
    Q3 = quantile(C49_3, 0.75, na.rm = TRUE),
    Max = max(C49_3, na.rm = TRUE),
    Count = n()
  )

epic_C49_3 %>%
  drop_na(C49_3) %>%
  ggplot(aes(x = as.factor(C49_3), fill = as.factor(C49_3))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C49_3 by Country",
    x = "C49_3 (1-5)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

#-------------------------------------------------------------------------------
#---------------------------- EET Adoption -------------------------------------
#-------------------------------------------------------------------------------
###Highly energy-efficient appliances C44_1: 1 = Yes, 2 = No, 99 = Don't know
unique(epic$C44_1)
summary(epic$C44_1[epic$C44_1 != 99])
summary(as.factor(epic$C44_1[epic$C44_1 != 99]))
hist(epic$C44_1[epic$C44_1 != 99])

epic_C44_1 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C44_1)

epic_C44_1 %>%
  filter(C44_1 != 99) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C44_1, na.rm = TRUE),
    Q1 = quantile(C44_1, 0.25, na.rm = TRUE),
    Median = median(C44_1, na.rm = TRUE),
    Mean = mean(C44_1, na.rm = TRUE),
    Q3 = quantile(C44_1, 0.75, na.rm = TRUE),
    Max = max(C44_1, na.rm = TRUE),
    Count = n()
  )

epic_C44_1 %>%
  drop_na(C44_1) %>%
  ggplot(aes(x = as.factor(C44_1), fill = as.factor(C44_1))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C44_1 by Country",
    x = "C44_1 1/0/99",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

###Low-energy light bulbs C44_2: 1 = Yes, 2 = No, 99 = Don't know
unique(epic$C44_2)
summary(epic$C44_2[epic$C44_2 != 99])
summary(as.factor(epic$C44_2[epic$C44_2 != 99]))
hist(epic$C44_2[epic$C44_2 != 99])

epic_C44_2 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C44_2)

epic_C44_2 %>%
  filter(C44_2 != 99) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C44_2, na.rm = TRUE),
    Q1 = quantile(C44_2, 0.25, na.rm = TRUE),
    Median = median(C44_2, na.rm = TRUE),
    Mean = mean(C44_2, na.rm = TRUE),
    Q3 = quantile(C44_2, 0.75, na.rm = TRUE),
    Max = max(C44_2, na.rm = TRUE),
    Count = n()
  )

epic_C44_2 %>%
  drop_na(C44_2) %>%
  ggplot(aes(x = as.factor(C44_2), fill = as.factor(C44_2))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C44_2 by Country",
    x = "C44_2 1/0/99",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

###Energy-efficient windows C44_3: 1 = Yes, 2 = No, 99 = Don't know
unique(epic$C44_3)
summary(epic$C44_3[epic$C44_3 != 99])
summary(as.factor(epic$C44_3[epic$C44_3 != 99]))
hist(epic$C44_3[epic$C44_3 != 99])

epic_C44_3 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C44_3)

epic_C44_3 %>%
  filter(C44_3 != 99) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C44_3, na.rm = TRUE),
    Q1 = quantile(C44_3, 0.25, na.rm = TRUE),
    Median = median(C44_3, na.rm = TRUE),
    Mean = mean(C44_3, na.rm = TRUE),
    Q3 = quantile(C44_3, 0.75, na.rm = TRUE),
    Max = max(C44_3, na.rm = TRUE),
    Count = n()
  )

epic_C44_3 %>%
  drop_na(C44_3) %>%
  ggplot(aes(x = as.factor(C44_3), fill = as.factor(C44_3))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C44_3 by Country",
    x = "C44_3 1/0/99",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

###Thermal insulation of walls/roof/floor C44_4: 1 = Yes, 2 = No, 99 = Don't know
unique(epic$C44_4)
summary(epic$C44_4[epic$C44_4 != 99])
summary(as.factor(epic$C44_4[epic$C44_4 != 99]))
hist(epic$C44_4[epic$C44_4 != 99])

epic_C44_4 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C44_4)

epic_C44_4 %>%
  filter(C44_4 != 99) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C44_4, na.rm = TRUE),
    Q1 = quantile(C44_4, 0.25, na.rm = TRUE),
    Median = median(C44_4, na.rm = TRUE),
    Mean = mean(C44_4, na.rm = TRUE),
    Q3 = quantile(C44_4, 0.75, na.rm = TRUE),
    Max = max(C44_4, na.rm = TRUE),
    Count = n()
  )

epic_C44_4 %>%
  drop_na(C44_4) %>%
  ggplot(aes(x = as.factor(C44_4), fill = as.factor(C44_4))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C44_4 by Country",
    x = "C44_4 1/0/99",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

###Solar panels for electricity C44_6: 1 = Yes, 2 = No, 99 = Don't know
unique(epic$C44_6)
summary(epic$C44_6[epic$C44_6 != 99])
summary(as.factor(epic$C44_6[epic$C44_6 != 99]))
hist(epic$C44_6[epic$C44_6 != 99])

epic_C44_6 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C44_6)

epic_C44_6 %>%
  filter(C44_6 != 99) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C44_6, na.rm = TRUE),
    Q1 = quantile(C44_6, 0.25, na.rm = TRUE),
    Median = median(C44_6, na.rm = TRUE),
    Mean = mean(C44_6, na.rm = TRUE),
    Q3 = quantile(C44_6, 0.75, na.rm = TRUE),
    Max = max(C44_6, na.rm = TRUE),
    Count = n()
  )

epic_C44_6 %>%
  drop_na(C44_6) %>%
  ggplot(aes(x = as.factor(C44_6), fill = as.factor(C44_6))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C44_6 by Country",
    x = "C44_6 1/0/99",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

###Solar water heating C44_7: 1 = Yes, 2 = No, 99 = Don't know
unique(epic$C44_7)
summary(epic$C44_7[epic$C44_7 != 99])
summary(as.factor(epic$C44_7[epic$C44_7 != 99]))
hist(epic$C44_7[epic$C44_7 != 99])

epic_C44_7 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C44_7)

epic_C44_7 %>%
  filter(C44_7 != 99) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C44_7, na.rm = TRUE),
    Q1 = quantile(C44_7, 0.25, na.rm = TRUE),
    Median = median(C44_7, na.rm = TRUE),
    Mean = mean(C44_7, na.rm = TRUE),
    Q3 = quantile(C44_7, 0.75, na.rm = TRUE),
    Max = max(C44_7, na.rm = TRUE),
    Count = n()
  )

epic_C44_7 %>%
  drop_na(C44_7) %>%
  ggplot(aes(x = as.factor(C44_7), fill = as.factor(C44_7))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C44_7 by Country",
    x = "C44_7 1/0/99",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

###Heat pumps: C44_9, 1 = Yes, 2 = No, 99 = Don't know
unique(epic$C44_9)
summary(epic$C44_9[epic$C44_9 != 99])
summary(as.factor(epic$C44_9[epic$C44_9 != 99]))
hist(epic$C44_9[epic$C44_9 != 99])

epic_C44_9 <- epic %>%
  select(X, weight, weight_2, Country_code, Country_name, C44_9)

epic_C44_9 %>%
  filter(C44_9 != 99) %>%
  group_by(Country_name) %>%
  summarise(
    Min = min(C44_9, na.rm = TRUE),
    Q1 = quantile(C44_9, 0.25, na.rm = TRUE),
    Median = median(C44_9, na.rm = TRUE),
    Mean = mean(C44_9, na.rm = TRUE),
    Q3 = quantile(C44_9, 0.75, na.rm = TRUE),
    Max = max(C44_9, na.rm = TRUE),
    Count = n()
  )

epic_C44_9 %>%
  drop_na(C44_9) %>%
  ggplot(aes(x = as.factor(C44_9), fill = as.factor(C44_9))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "C44_9 by Country",
    x = "C44_9 1/0/99",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

#-------------------------------------------------------------------------------
#---------------------------- Government support -------------------------------
#-------------------------------------------------------------------------------
###Highly energy-efficient appliances C45_1: 1 = Yes, 2 = No, 99 = Don't know

###Energy-efficient windows C45_3: 1 = Yes, 2 = No, 99 = Don't know

###Thermal insulation of walls/roof/floor C45_4: 1 = Yes, 2 = No, 99 = Don't know#

###Solar panels for electricity C45_6: 1 = Yes, 2 = No, 99 = Don't know

###Solar water heating C45_7: 1 = Yes, 2 = No, 99 = Don't know

###Heat pumps C45_9: 1 = Yes, 2 = No, 99 = Don't know

sum(epic$weight)
length(epic$X)
sum(epic$weight_2)










