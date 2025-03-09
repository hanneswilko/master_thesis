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







#----------------------- Environtmental preferences ----------------------------
unique(epic_data$B23_1)
hist(epic_data$B23_1[epic_data$B23_1 != 999999])
summary(epic_data$B23_1[epic_data$B23_1 != 999999])
summary(as.factor(epic_data$B23_1))
envpref <- epic_data %>%
  select(Country, B23_1) %>%
  mutate(B23_1 = case_when(
    B23_1 == 999999 ~ NA_real_,  # Assigns NA to 999999
    TRUE ~ B23_1  # Keeps all other values unchanged
  ))

envpref_filtered <- envpref %>%
  filter(!is.na(B23_1))  # Excludes NA values

# Create the histogram
ggplot(envpref_filtered, aes(x = as.factor(B23_1), fill = as.factor(B23_1))) +
  geom_bar(color = "black") +
  facet_wrap(~ Country_name) +  # Creates separate histograms per country
  scale_fill_brewer(palette = "Set2") +  # Adds color distinction
  labs(
    title = "Distribution of B23_1 Values by Country",
    x = "B23_1 Value (1-5)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since X-axis already shows values
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

