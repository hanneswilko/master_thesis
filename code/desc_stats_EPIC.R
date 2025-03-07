#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: EPIC 2022

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr","jsonlite")

##OECD carbon rate
epic_data <- read.csv("./processed_data/epic_data_var_interest.csv")
glimpse(epic_data)
View(epic_data)
###Variables of interest for core message of research project
###Subsample for HHs where adoption of EETs is feasible --> see Hassett et al. (2024) pp.43

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

###Why not EET


###Drivers for adopting EET




###home ownership: S5, 1 = residenced owned, 2 = rented, 3 = another type
###detached housing: S18, 1 = apartment, 2 = apartment, 3 = detached house, 4 = semi-detached, 89 = other

###environmental attitude: B23_1, 1 = not important - 5 = very important, 999999 = NA
###Environmental impacts are overstated: B31_1, 1 = strongly disagree - 4 = agree
###Protecting environment boosts economy: B31_3, 1 = strongly disagree - 4 = agree
###Environmental issues resolved through public policies: B31_5, 1 = strongly disagree - 4 = agree
###Environmental policies should not cost extra money: B31_6, 1 = strongly disagree - 4 = agree
###Environmental issues resolved through technological progress: B31_7, 1 = strongly disagree - 4 = agree
###Environmental issues resolved through individuals voluntarily changing behaviour: B31_8, 1 = strongly disagree - 4 = agree

###country - ref area: Country
###weights: weight, weight_2
###income: Income (categorized)
###Age: Age_cat (categorized)

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

# Add a new column 'Country_Abbrev' based on 'Country' codes
envpref <- envpref %>%
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

