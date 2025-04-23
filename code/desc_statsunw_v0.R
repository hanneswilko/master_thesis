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
#4. Objective summary statistics
#5. 5. Saving Graphs

#-------------------------------------------------------------------------------
#---------------------------- 1. Loading Data ----------------------------------
#-------------------------------------------------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr",
               "srvyr", "survey", "ggsurvey", "purrr", "kableExtra")

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
  select(ID, Country_code, Country_name, Age_cat, Income, Gender, S5, REGION_UK, REGION_SE3,
         REGION_US2, REGION_NL2, REGION_CH, REGION_FR2, REGION_CA, REGION_BE, REGION_IL,
         S9_US, S9_UK, S9_FR, S9_SE, S9_CH, S9_NL, S9_CA, S9_BE, S9_IL,
         S18, S19_1, S19_1_1, S20, B23_1, B31_1, B31_3, B31_5, B31_6, B31_7, B31_8, C37_1, C37_2, C37_3, C37_4,
         C37_5, C37_6, C37_8, C49_1, C49_2, C49_3, C50, C44_1, C44_2, C44_3, C44_4, C44_6, C44_7,
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
    total <- total_possible + not_possible
    
    # Mean rates among possible
    mean_adopt_rate <- sum(adopt[is_possible] == 1, na.rm = TRUE) / total_possible
    mean_support_rate <- sum(support[is_possible & adopt == 1] == 1, na.rm = TRUE) / 
      sum(is_possible & adopt == 1, na.rm = TRUE)
    
    tibble(
      Country = country,
      Technology = tech,
      Possible = total_possible,
      Adopted = adopted,
      Supported = supported,
      Mean_Adopted = round(mean_adopt_rate, 2),
      Mean_Supported = round(mean_support_rate, 2)
    )
  })
}

#Create Summary Data Frame --> Table 1
EET_summary_df <- map_dfr(countries, get_summary)

EET_summary_df %>%
  arrange(Technology, Country) %>%
  select(-Technology) %>%
  kbl(
    caption = "Rates of Technology adoption across countries.",
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE   #THIS enables multi-page support
  ) %>%
  kable_styling(latex_options = c("striped"),  font_size = 10) %>% # 'hold_position' not needed with longtable
  pack_rows(index = table(EET_summary_df$Technology) %>% as.list())

#---------------- 2. Table: socioeconomic characteristics ----------------------

#Age
attr(epic_raw$Age_cat, "labels")
#Age_cat: 18-24 = 1, 25-34 = 2, 35-44 = 3, 45-54 = 4, 55+ = 5
summary(epic$Age_cat)

#Sex
attr(epic_raw$Gender, "labels")
#Gender: Male = 1, Female = 2
summary(epic$Gender)

epic <- epic %>%
  mutate(
    Female = case_when(
      Gender == 2 ~ 1,
      Gender == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

#Income quintile
attr(epic_raw$Income, "labels")
attr(epic_raw$S12_1, "labels")
#Age_cat: 18-24 = 1, 25-34 = 2, 35-44 = 3, 45-54 = 4, 55+ = 5
summary(epic$Income)

#Environmental concern
attr(epic_raw$B23_1, "label")
#B23_1: Not at all important        Not important   Somewhat important            Important       Very important    Prefer not to say 
summary(epic$B23_1)
#create new variable with binary level for low and high environmental concern
epic <- epic %>%
  mutate(
    Env_concern = ifelse(B23_1 == 999999, NA, B23_1),
    Env_concern = case_when(
      Env_concern %in% c(4, 5) ~ 1, #high
      Env_concern %in% c(1, 2, 3) ~ 0, #low
      TRUE ~ NA_real_
    )
  )

summary(epic$Env_concern)

#Education
attr(epic_raw$S9_CH, "labels")
#S9_X
summary(as.factor(epic$S9_SE))

epic <- epic %>%
  mutate(
    # US
    Education_US = case_when(
      S9_US %in% 1:9 ~ 0,  # lower education
      S9_US %in% 10:13 ~ 1, # higher education (Bachelor's, Master's, PhD)
      TRUE ~ NA_real_
    ),
    
    # UK
    Education_UK = case_when(
      S9_UK %in% 1:5 ~ 0,  # up to secondary education
      S9_UK %in% 6:7 ~ 1,  # Bachelor's degree (NVQ4, HNC/HND), Post-graduate diploma
      TRUE ~ NA_real_
    ),
    
    # FR
    Education_FR = case_when(
      S9_FR %in% 1:5 ~ 0,  # Primary, Lower Secondary, or Vocational Upper Secondary education
      S9_FR %in% 6:7 ~ 1,  # Higher education (Bachelor's and Master's)
      TRUE ~ NA_real_
    ),
    
    # SE
    Education_SE = case_when(
      S9_SE %in% 1:3 ~ 0,   # Compulsory or General Upper Secondary education
      S9_SE %in% 4 ~ 1,     # Technical/practical/occupational/research higher education
      TRUE ~ NA_real_
    ),
    
    # CH
    Education_CH = case_when(
      S9_CH %in% 1:5 ~ 0,  # Primary to Vocational Secondary education
      S9_CH %in% 6:8 ~ 1,  # University degree, Master's, or Doctorate
      TRUE ~ NA_real_
    ),
    
    # NL
    Education_NL = case_when(
      S9_NL %in% 1:2 ~ 1,  # HBO, University degree (Bachelor's, Master's, PhD)
      S9_NL %in% 3:7 ~ 0,  
      TRUE ~ NA_real_
    ),
    
    # CA
    Education_CA = case_when(
      S9_CA %in% 1:6 ~ 0,  # Primary, some high school, or graduated high school
      S9_CA %in% 7:8 ~ 1,  # University undergraduate degree or higher
      TRUE ~ NA_real_
    ),
    
    # BE
    Education_BE = case_when(
      S9_BE %in% 1:6 ~ 0,  # Primary, lower secondary, or professional upper secondary education
      S9_BE %in% 7:10 ~ 1, # Bachelor's, Master's, or Doctorate
      TRUE ~ NA_real_
    ),
    
    # IL
    Education_IL = case_when(
      S9_IL %in% 1:3 ~ 0,  # No formal education or High school diploma
      S9_IL %in% 4:5 ~ 1,  # Bachelor's degree or above
      TRUE ~ NA_real_
    ),
    
    # Merge into one variable
    Edu_level = coalesce(
      Education_US, Education_UK, Education_FR, Education_SE,
      Education_CH, Education_NL, Education_CA, Education_BE, Education_IL
    )
  )

summary(as.factor(epic$Edu_level[epic$Country_name == "SE"]))

#creating summary table --> table 2
Socioeco_summary_df <- epic %>%
  group_by(Country_name) %>%
  summarise(
    Avg_Age_cat = round(mean(Age_cat, na.rm = TRUE),2),
    Avg_Edu_level = round(mean(Edu_level, na.rm = TRUE),2),
    Avg_Income = round(mean(Income, na.rm = TRUE),2),
    Avg_Female = round(mean(Female, na.rm = TRUE),2),
    Avg_Env_concern = round(mean(Env_concern, na.rm = TRUE),2)
  ) %>%
  arrange(Country_name)

Socioeco_summary_df %>%
  kbl(
    caption = "Socio-economic characteristics of households.",
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped"),
    font_size = 10
  )

#----------------- 3. Table: characteristics of dwellings ----------------------

#Energy costs
attr(epic_raw$C37_1, "labels")
#C37_1: energy costs US
#C37_2: energy costs FR, NL and BE
#C37_3: energy costs UK
#C37_4: energy costs SE
#C37_5: energy costs CH
#C37_6: energy costs IL
#C37_8: energy costs CA
#creating new variable with merged info on average monthly cost
epic <- epic %>%
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
epic <- epic %>%
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
epic <- epic %>%
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
summary(epic$S19_1)
summary(epic$S19_1_1)
#S19_1: Rest & S19_1_1: US & CA
#Less than 25m2 = 1, 25m2-50m2 = 2, 51m2-75m2 = 3, 76m2-100m2 = 4, 101m2-150m2 = 5, 151m2-200m2 = 6, More than 200m2 = 7 Don’t know = 888888
#creating new variable for mergerd info auf dwelling size (m2 and ft2)
epic <- epic %>%
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
epic <- epic %>%
  mutate(
    Rural = case_when(
      S20 %in% c(3, 4, 89) ~ 1,
      S20 %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    )
  )

#creating summary table --> table 3
Dwelling_summary_df <- epic %>%
  group_by(Country_name) %>%
  summarise(
    Avg_Home_Owned = round(mean(Home_owned, na.rm = TRUE),2),
    Avg_Dwelling_House = round(mean(Dwelling_house, na.rm = TRUE),2),
    Avg_Dwelling_Size = round(mean(Dwelling_size, na.rm = TRUE),2),
    Avg_Rural = round(mean(Rural, na.rm = TRUE),2),
    Avg_Energy_Costs = round(mean(Energy_costs, na.rm = TRUE),2)
  ) %>%
  arrange(Country_name)

Dwelling_summary_df %>%
  kbl(
    caption = "Characteristics of dwellings.",
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped"),
    font_size = 10
  )

#------------- Additional Environmental Preference Information -----------------
#Environmental issues should be resolved mainly through public policies
attr(epic_raw$B31_5, "label")
#Strongly disagree = 1, Disagree = 2, Neither agree or disagree = 3, Agree = 4, Strongly agree = 5, Prefer not to say = 999999
summary(epic$B31_5[epic_EET$B31_5 != 999999])

#Environmental policies introduced by the government should not cost me extra money
attr(epic_raw$B31_6, "label")
#Strongly disagree = 1, Disagree = 2, Neither agree or disagree = 3, Agree = 4, Strongly agree = 5, Prefer not to say = 999999
summary(epic$B31_6[epic$B31_6 != 999999])

#In support of providing subsidies to households for purchasing energy-efficient appliances or investing in renewable energy equipment
attr(epic_raw$C49_1, "labels")
#Strongly against = 1, against = 2, indifferent = 3, support = 4, Strongly support = 5
summary(epic$C49_1)

#In support of Taxing energy use or the purchase of appliances and equipment that use a lot of energy
attr(epic_raw$C49_2, "labels")
#Strongly against = 1, against = 2, indifferent = 3, support = 4, Strongly support = 5
summary(epic$C49_2)

#In support of introducing energy efficiency standards for appliances and buildings that manufacturers have to comply with
attr(epic_raw$C49_3, "labels")
#Strongly against = 1, against = 2, indifferent = 3, support = 4, Strongly support = 5
summary(epic$C49_3)

#In support of low-income households receiving government support to help them pay for energy equipment
attr(epic_raw$C50, "labels")
#Yes = 1, No = 2, Don´t know = 3 
summary(epic$C50[epic$C50 != 3])

#---------------------- 4. Table: Env policy preferences -----------------------
epic <- epic %>%
  mutate(
    Env_policy_public = case_when(
      B31_5 %in% c(4, 5) ~ 1,  # Agree or Strongly agree
      B31_5 %in% c(1, 2, 3) ~ 0,
      B31_5 == 999999 ~ NA_real_
    ),
    Env_policy_costs = case_when(
      B31_6 %in% c(1, 2) ~ 1,  # Strongly disagree or Disagree = willing to pay
      B31_6 %in% c(3, 4, 5) ~ 0,  # neutral or against paying
      B31_6 == 999999 ~ NA_real_
    ),
    Supp_Subsidy = case_when(
      C49_1 %in% c(4, 5) ~ 1,  
      C49_1 %in% 1:3 ~ 0  
    ),
    Supp_Tax = case_when(
      C49_2 %in% c(4, 5) ~ 1,  
      C49_2 %in% 1:3 ~ 0  
    ),
    Supp_Standards = case_when(
      C49_3 %in% c(4, 5) ~ 1,  
      C49_3 %in% 1:3 ~ 0  
    ),
    Supp_liH = case_when(
      C50 %in% 1 ~ 1,  
      C50 %in% 2:3 ~ 0  
    ),
  )

#creating summary table --> table 4
Env_policy_summary_df <- epic %>%
  group_by(Country_name) %>%
  summarise(
    Avg_Env_policy_public = round(mean(Env_policy_public, na.rm = TRUE),2),
    Avg_Env_policy_costs = round(mean(Env_policy_costs, na.rm = TRUE),2),
    Avg_Supp_Subsidy = round(mean(Supp_Subsidy, na.rm = TRUE),2),
    Avg_Supp_Tax = round(mean(Supp_Tax, na.rm = TRUE),2),
    Avg_Supp_Standards = round(mean(Supp_Standards, na.rm = TRUE),2),
    Avg_Supp_liH = round(mean(Supp_liH, na.rm = TRUE),2)
  ) %>%
  arrange(Country_name)

Env_policy_summary_df %>%
  kbl(
    caption = "Respondents’ attitudes regarding environmental policy.",
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped"),
    font_size = 10
  )

#-------------------------------------------------------------------------------
#--------------------- 4. Objective summary statistics -------------------------
#-------------------------------------------------------------------------------

#------------------------------ Data wrangling ---------------------------------
##EPS
EPS_sub <- EPS %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  filter(TIME_PERIOD %in% 2010:2020)
###Average EPS per country
EPS_sub_avg <- EPS_sub %>%
  group_by(REF_AREA) %>%
  summarize(avg_EPS = round(mean(OBS_VALUE, na.rm = TRUE),2)) #use summarize to collapse data

##emissions-weighted carbon price - expressed in 2021USD/tCO2
wC02price_sub <- wC02price %>%
  select(Code, Year, price_with_tax_weighted_by_share_of_co2) %>%
  filter(Year %in% 2010:2020)
###Average Price per country
wC02price_sub_avg <- wC02price_sub %>%
  group_by(Code) %>%
  summarize(price_with_tax_weighted_by_share_of_co2_avg = round(mean(price_with_tax_weighted_by_share_of_co2, na.rm = TRUE),2)) #use summarize to collapse data

#Merged Info about EETs
epic_EET <- epic_EET %>%
  mutate(
    # Low-cost EET adoption (LEDs)
    low_EET = ifelse(C44_2 == 1, 1, 0),
    
    # Middle-cost EET adoption (Highly energy-efficient appliances)
    middle_EET = ifelse(C44_1 == 1, 1, 0),
    
    # High-cost EET adoption (Energy-efficient windows, Thermal insulation, Solar panels, Batter storage, Heat pumps)
    high_EET = ifelse(C44_3 == 1 | C44_4 == 1 | C44_6 == 1 | C44_7 == 1 | C44_8 == 1 | C44_9 == 1, 1, 0),
    
    # Low-cost EET adoption (LEDs) not possible
    low_EET_p = case_when(
      C44_2 == 1 ~ 1, #if adopted = possible
      C46_2 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    
    # Middle-cost EET adoption (Highly energy-efficient appliances) not possible
    middle_EET_p = case_when(
      C44_1 == 1 ~ 1, #if adopted = possible
      C46_1 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    ),
    
    # High-cost EET adoption (Energy-efficient windows, Thermal insulation, Solar panels, Heat pumps) not possible
    high_EET_p = case_when(
      C44_3 == 1 | C44_4 == 1 | C44_6 == 1 | C44_7 == 1 | C44_8 == 1 | C44_9 == 1 ~ 1,  #if adopted = possible
      C46_3 == 4 | C46_4 == 4 | C46_6 == 4 | C46_7 == 1 | C46_8 == 1 | C46_9 == 4 ~ 0,  #no possible
      TRUE ~ 1  #else possible
    )
  )

#create variable with merged info on medium and high-cost EETs government support for adoption
epic_EET <- epic_EET %>%
  mutate(
    # Middle-cost EET gov support (Highly energy-efficient appliances)
    middle_EET_support = case_when(
      C45_1 == 1 ~ 1, #support
      TRUE ~ 0 #else no support
    ),
    
    # High-cost EET gov support (Energy-efficient windows, Thermal insulation, Solar panels, Heat pumps)
    high_EET_support = case_when(
      C45_3 == 1 | C45_4 == 1 | C45_6 == 1 | C45_7 == 1 | C45_8 == 1 | C45_9 == 1 ~ 1,  #support
      TRUE ~ 0  #else no support
    )
  )

#---------------------- Adoption of Appliances per EPS category ----------------
#middle-cost EETs (Appliances)
epic_Appl <- epic_EET %>%
  filter(Appl_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_1, C46_1, C45_1,
         middle_EET, middle_EET_p, middle_EET_support)

##calculating the proportion of adopters with support
epic_Appl_support_prop <- epic_Appl %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(C44_1 == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(middle_EET_support == 1, na.rm = TRUE),
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
                                 "proportion_support_received" = "support",
                                 "proportion_support_not_received" = "no support"))

# Merge the data on the country name
epic_Appl_EPS_wC02 <- epic_Appl_support_prop_long %>%
  left_join(wC02price_sub_avg, by = c("Country_name" = "Code")) %>%
  left_join(EPS_sub_avg, by = c("Country_name" = "REF_AREA"))


summary(epic_Appl_EPS_wC02$avg_EPS)

#Categorize EPS Index for better visualization
epic_Appl_EPS_wC02 <- epic_Appl_EPS_wC02 %>%
  mutate(
    EPS_category = ntile(avg_EPS, 3),  # Tertiles: 1 = low, 2 = medium, 3 = high
    EPS_category = case_when(
      EPS_category == 1 ~ "low stringency",
      EPS_category == 2 ~ "medium stringency",
      EPS_category == 3 ~ "high stringency"
    ),
    EPS_category = factor(EPS_category, levels = c("low stringency", "medium stringency", "high stringency"))
  )

epic_Appl_EPS_wC02 %>%
  group_by(EPS_category) %>%
  summarise(min_EPS = min(avg_EPS), max_EPS = max(avg_EPS))

#Appliance adoption per EPS category
barchart_Appl_EPS_support_prop <- ggplot(epic_Appl_EPS_wC02, 
                                     aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                         y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of highly energy-efficient Appliances by Environmental Policy Stringency Index", 50),
    x = "Country",
    y = "Proportion of Adopters",
    fill = "Government support"
  ) +
  facet_wrap(~ EPS_category, scales = "free_x") +  #Facet by EPS Category!
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  theme_minimal() +
  scale_fill_manual(values = c("no support" = "#8da0cb", "support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

#Scatterplot: relationship of EPS and appliances adoption rates
scatter_EPS_adoption_by_appl <- ggplot(epic_Appl_EPS_wC02, 
                                   aes(x = avg_EPS, y = proportion_adopters)) +
  geom_text(aes(label = Country_name), size = 4, fontface = "bold", alpha = 0.9, show.legend = FALSE) +  # use country labels
  geom_smooth(method = "lm", color = "#8da0cb", se = FALSE, linewidth = 1) +  # regression line
  scale_y_continuous(limits = c(0.4, 0.8), breaks = seq(0.4, 0.8, by = 0.1)) +
  scale_x_continuous(limits = c(0.8, 4.5), breaks = seq(1, 5, by = 1)) +
  labs(
    title = "Adoption of highly energy-efficient Appliances by EPS Index",
    x = "Environmental Policy Stringency (EPS) Index",
    y = "Proportion of Adopters"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    axis.text = element_text(size = 10)
  )

#------------------ Adoption of high-cost EETs per EPS category ----------------
#high-cost EETs (Windows, Thermal insulation, Solar panels e, Solar water heating, Battery storage, Heat pumps)
epic_highEET <- epic_EET %>%
  filter(high_EET_p != 0) %>% #filter for cases where adoption is possible
  select(Country_code, Country_name, Income, S5, S18, C44_3, C44_4, C44_6, C44_7,
         C44_8, C44_9, C46_3, C46_4, C46_6, C46_9, C45_3, C45_4, C45_6, C45_7,
         C45_8, C45_9, high_EET, high_EET_p, high_EET_support)

##calculating the proportion of adopters with support
epic_highEET_support_prop <- epic_highEET %>%
  group_by(Country_name) %>%
  summarise(
    adopters = sum(high_EET == 1, na.rm = TRUE),
    total_hh = n(),
    proportion_adopters = adopters / total_hh,
    support_received = sum(high_EET_support == 1, na.rm = TRUE),
    support_not_received = adopters - support_received,
    proportion_support_received = support_received / adopters,
    proportion_support_not_received = support_not_received / adopters
  ) %>%
  ungroup()

##creating long table for proportions of adopters with support
epic_highEET_support_prop_long <- epic_highEET_support_prop %>%
  select(Country_name, proportion_adopters, proportion_support_received, proportion_support_not_received) %>%
  tidyr::pivot_longer(cols = c(proportion_support_received, proportion_support_not_received), 
                      names_to = "Support_Status", 
                      values_to = "Proportion_Support") %>%
  mutate(Support_Status = recode(Support_Status, 
                                 "proportion_support_received" = "support",
                                 "proportion_support_not_received" = "no support"))

# Merge the data on the country name
epic_highEET_EPS_wC02 <- epic_highEET_support_prop_long %>%
  left_join(wC02price_sub_avg, by = c("Country_name" = "Code")) %>%
  left_join(EPS_sub_avg, by = c("Country_name" = "REF_AREA"))

summary(epic_highEET_EPS_wC02$avg_EPS)

#Categorize EPS Index for better visualization
epic_highEET_EPS_wC02 <- epic_highEET_EPS_wC02 %>%
  mutate(
    EPS_category = ntile(avg_EPS, 3),  # Tertiles: 1 = low, 2 = medium, 3 = high
    EPS_category = case_when(
      EPS_category == 1 ~ "low stringency",
      EPS_category == 2 ~ "medium stringency",
      EPS_category == 3 ~ "high stringency"
    ),
    EPS_category = factor(EPS_category, levels = c("low stringency", "medium stringency", "high stringency"))
  )

epic_highEET_EPS_wC02 %>%
  group_by(EPS_category) %>%
  summarise(min_EPS = min(avg_EPS), max_EPS = max(avg_EPS))

#Appliance adoption per EPS category
barchart_highEET_EPS_support_prop <- ggplot(epic_highEET_EPS_wC02, 
                                         aes(x = reorder(Country_name, -proportion_adopters * Proportion_Support), 
                                             y = proportion_adopters * Proportion_Support, fill = Support_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = str_wrap("Adoption of high-cost EETs by Environmental Policy Stringency Index", 50),
    x = "Country",
    y = "Proportion of Adopters",
    fill = "Government support"
  ) +
  facet_wrap(~ EPS_category, scales = "free_x") +  #Facet by EPS Category!
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  theme_minimal() +
  scale_fill_manual(values = c("no support" = "#8da0cb", "support" = "#fc8d62")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 12, face = "bold"),
    strip.text.x = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

#----------------- Relationship of adoption rates and EPS index ----------------
# Merge the data on the country name
EET_summary_df_EPS_wC02 <- EET_summary_df %>%
  left_join(wC02price_sub_avg, by = c("Country" = "Code")) %>%
  left_join(EPS_sub_avg, by = c("Country" = "REF_AREA"))

summary(epic_highEET_EPS_wC02$avg_EPS)

#Categorize EPS Index for better visualization
EET_summary_df_EPS_wC02 <- EET_summary_df_EPS_wC02 %>%
  mutate(
    EPS_category = ntile(avg_EPS, 3),  # Tertiles: 1 = low, 2 = medium, 3 = high
    EPS_category = case_when(
      EPS_category == 1 ~ "low stringency",
      EPS_category == 2 ~ "medium stringency",
      EPS_category == 3 ~ "high stringency"
    ),
    EPS_category = factor(EPS_category, levels = c("low stringency", "medium stringency", "high stringency"))
  )

EET_summary_df_EPS_wC02 %>%
  group_by(EPS_category) %>%
  summarise(min_EPS = min(avg_EPS), max_EPS = max(avg_EPS))

# Scatterplot: EPS vs Adoption Rate, colored by Technology
scatter_EPS_adoption_by_tech <- ggplot(EET_summary_df_EPS_wC02, 
                                       aes(x = avg_EPS, y = Mean_Adopted, color = Technology)) +
  geom_text(aes(label = Country), size = 4, fontface = "bold", alpha = 0.9, show.legend = FALSE) +  # use country labels
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, aes(group = Technology, color = Technology), na.rm = TRUE) +  # regression by tech
  scale_y_continuous(limits = c(0, 0.80), breaks = seq(0, 0.80, by = 0.20)) +
  scale_x_continuous(limits = c(0.8, 4.5), breaks = seq(1, 5, by = 1)) +
  scale_color_manual(values = c(
    "Appliances" = "#8da0cb",
    "Windows" = "#fc8d62",
    "Thermal insulation" = "#66c2a5",
    "Solar panels for electricity" = "#e78ac3",
    "Solar water heating" = "#a6d854",
    "Battery storage" = "#ffd92f",
    "Heat pumps" = "#e5c494"
  )) +
  labs(
    title = "Adoption of Energy-Efficient Technologies by EPS Index",
    x = "Environmental Policy Stringency (EPS) Index",
    y = "Proportion of Adopters",
    color = "Technology"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10)
  )

#Subsample - EETs excluding Solar water heating and battery storage
EET_summary_df_EPS_wC02_sub <- EET_summary_df_EPS_wC02 %>%
  filter(Technology %in% c("Appliances", "Windows", "Thermal insulation",
                           "Solar panels for electricity", "Heat pumps"))

# Scatterplot: EPS vs Adoption Rate, colored by Technology
scatter_EPS_adoption_by_tech_sub <- ggplot(EET_summary_df_EPS_wC02_sub, 
                                       aes(x = avg_EPS, y = Mean_Adopted, color = Technology)) +
  geom_text(aes(label = Country), size = 4, fontface = "bold", alpha = 0.9, show.legend = FALSE) +  # use country labels
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1, aes(group = Technology, color = Technology), na.rm = TRUE) +  # regression by tech
  scale_y_continuous(limits = c(0, 0.80), breaks = seq(0, 0.80, by = 0.20)) +
  scale_x_continuous(limits = c(0.8, 4.5), breaks = seq(1, 5, by = 1)) +
  scale_color_manual(values = c(
    "Appliances" = "#8da0cb",
    "Windows" = "#fc8d62",
    "Thermal insulation" = "#66c2a5",
    "Solar panels for electricity" = "#e78ac3",
    "Heat pumps" = "#a6d854"
  )) +
  labs(
    title = "Adoption of Energy-Efficient Technologies by EPS Index",
    x = "Environmental Policy Stringency (EPS) Index",
    y = "Proportion of Adopters",
    color = "Technology"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10)
  )

#------------- 5. Table: Env Policy, Env Preferences, Gov Support --------------
#create variable with merged info on medium and high-cost EETs government support for adoption
epic <- epic %>%
  mutate(
    #Summary variable on EET gov support (if at least for one adoption)
    EET_support = case_when(
      C45_1 == 1 | C45_3 == 1 | C45_4 == 1 | C45_6 == 1 | C45_7 == 1 | C45_8 == 1 | C45_9 == 1 ~ 1,  #support
      TRUE ~ 0  #else no support
    )
  )

#create summary table
Policy_stats_summary_df <- epic %>%
  group_by(Country_name) %>%
  summarise(
    Avg_EET_support = round(mean(EET_support, na.rm = TRUE),2),
  ) %>%
  arrange(Country_name)

# Merge the data on the country name
Policy_stats_summary_df <- Policy_stats_summary_df %>%
  left_join(wC02price_sub_avg, by = c("Country_name" = "Code")) %>%
  left_join(EPS_sub_avg, by = c("Country_name" = "REF_AREA"))

Policy_stats_summary_df %>%
  kbl(
    caption = "Environmental Policy Indicators",
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped"),
    font_size = 10
  )

#---------------------- 6. Table: HH per country -------------------------------
# Aggregate data to count number of households per country
households_per_country_df <- epic %>%
  group_by(Country_name) %>%
  summarise(Households = n()) %>%
  arrange(Country_name)

households_per_country_df %>%
  kbl(
    caption = "Number of Households per Country",
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped"),
    font_size = 10
  )

#-------------------------------------------------------------------------------
#---------------------------- 5. Saving Graphs ---------------------------------
#-------------------------------------------------------------------------------
ggsave("./output/barchart_Appl_EPS_support_prop.pdf", plot = barchart_Appl_EPS_support_prop, dpi = 300, scale = 1.2)
ggsave("./output/barchart_highEET_EPS_support_prop.pdf", plot = barchart_highEET_EPS_support_prop, dpi = 300, scale = 1.2)

ggsave("./output/scatter_EPS_adoption_by_appl.pdf", plot = scatter_EPS_adoption_by_appl, dpi = 300, scale = 1.2)
ggsave("./output/scatter_EPS_adoption_by_tech.pdf", plot = scatter_EPS_adoption_by_tech, dpi = 300, scale = 1.2)
ggsave("./output/scatter_EPS_adoption_by_tech_sub.pdf", plot = scatter_EPS_adoption_by_tech_sub, dpi = 300, scale = 1.2)














