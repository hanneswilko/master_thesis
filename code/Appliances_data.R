#-------------------------------------------------------------------------------
#-------------------- Data - Appliances - Model setup --------------------------
#-------------------------------------------------------------------------------
# 1. Loading Packages
# 2. Loading Data
# 3. Cleaning Data
# 4. Saving Data

#-------------------------------------------------------------------------------
#------------------------ 1. Loading Packages ----------------------------------
#-------------------------------------------------------------------------------
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr",
               "srvyr", "survey", "ggsurvey", "purrr", "kableExtra")

#-------------------------------------------------------------------------------
#-------------------------- 1. Loading Data ------------------------------------
#-------------------------------------------------------------------------------
#EPIC
epic <- read.csv("./processed_data/epic_data_ABC_VoI.csv") #sample: var interest of survey parts A,B,C

#EPS Index
EPS <- read.csv("./processed_data/OECD_EPS_data.csv")

#-------------------------------------------------------------------------------
#-------------------------- 3. Cleaning Data -----------------------------------
#-------------------------------------------------------------------------------

#C44_1: Appliances
#C45_1: Government support for Appliances
#C46_1: Why not Appliances

#Age_cat: 18-24 = 1, 25-34 = 2, 35-44 = 3, 45-54 = 4, 55+ = 5
#Gender: Male = 1, Female = 2
#Income: Income categorical (quintiles)
#S9_X: Education

#S5: Homeownership - Living in a residence owned = 1, Living in a residence rented = 2, Living in another type of accommodation no owned = 3
#S18: Dwelling characteristics - #apartment in a building with less than 12 apartments = 1, apartment in a building with 12 or more apartments = 2
#     a detached house = 3, A semi-detached/terraced house  = 4, Other = 89
#S19_X: Dwelling size - Less than 25m2 = 1, 25m2-50m2 = 2, 51m2-75m2 = 3, 76m2-100m2 = 4, 101m2-150m2 = 5, 151m2-200m2 = 6,
#       More than 200m2 = 7 Don’t know = 888888
#S20: Area - major town/city = 1, suburban = 2, small town/village = 3, isolated dwelling = 4, Other = 89
#C37_X: Energy costs - categorical

#B23_1: 1 = Not at all important, 2 = Not important, 3 = Somewhat important, 4 = Important, 5 = Very important, 999999 = Prefer not to say 
#B31_5: Environmental issues should be resolved mainly through public policies
#B31_6: Environmental policies introduced by the government should not cost me extra money
#C49_1: In support of providing subsidies to households for purchasing energy-efficient appliances or investing in renewable energy equipment
#C49_2: In support of Taxing energy use or the purchase of appliances and equipment that use a lot of energy
#C49_3: In support of introducing energy efficiency standards for appliances and buildings that manufacturers have to comply with
#C50: In support of low-income households receiving government support to help them pay for energy equipment

#--------------------------- 3.1 EPIC Data -------------------------------------
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
  select(ID, weight, weight_2, Country_code, Country_name, Age_cat, Income, Gender, S5, REGION_UK, REGION_SE3,
         REGION_US2, REGION_NL2, REGION_CH, REGION_FR2, REGION_CA, REGION_BE, REGION_IL,
         S9_US, S9_UK, S9_FR, S9_SE, S9_CH, S9_NL, S9_CA, S9_BE, S9_IL, S18, S19_1, S19_1_1,
         S20, B23_1, B31_5, B31_6, C37_1, C37_2, C37_3, C37_4,
         C37_5, C37_6, C37_8, C49_1, C49_2, C49_3, C50, C44_1, C45_1, C46_1)

#create flag for HHs where respective EET adoption wasn't possible
epic_Appliances <- epic %>%
  mutate(
    # Highly energy-efficient appliances
    install_pos = case_when(
      C44_1 == 1 ~ 1, #if adopted = possible
      C46_1 == 4 ~ 0, #no possible
      TRUE ~ 1 #else possible
    )
  )

#filter for cases where adoption is possible
epic_Appliances <- epic_Appliances %>%
  filter(install_pos != 0) %>% #filter for cases where adoption is possible
  select(ID, weight, weight_2, Country_code, Country_name, Age_cat, Gender, Income,
         S9_US, S9_UK, S9_FR, S9_SE, S9_CH, S9_NL, S9_CA, S9_BE, S9_IL, S5, S18, S19_1, S19_1_1,
         S20, B23_1, B31_5, B31_6, C37_1, C37_2, C37_3, C37_4,
         C37_5, C37_6, C37_8, C49_1, C49_2, C49_3, C50, C44_1, C45_1, C46_1)

#------------------------- Socioeconomic Variables -----------------------------
#Age_cat
#Gender
epic_Appliances <- epic_Appliances %>%
  mutate(
    Female = case_when(
      Gender == 2 ~ 1,
      Gender == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

#Income quintiles
#S9_X: Education
epic_Appliances <- epic_Appliances %>%
  mutate(
    # US
    Higher_edu_US = case_when(
      S9_US %in% 1:9 ~ 0,  # lower education
      S9_US %in% 10:13 ~ 1, # higher education (Bachelor's, Master's, PhD)
      TRUE ~ NA_real_
    ),
    
    # UK
    Higher_edu_UK = case_when(
      S9_UK %in% 1:5 ~ 0,  # up to secondary education
      S9_UK %in% 6:7 ~ 1,  # Bachelor's degree (NVQ4, HNC/HND), Post-graduate diploma
      TRUE ~ NA_real_
    ),
    
    # FR
    Higher_edu_FR = case_when(
      S9_FR %in% 1:5 ~ 0,  # Primary, Lower Secondary, or Vocational Upper Secondary education
      S9_FR %in% 6:7 ~ 1,  # Higher education (Bachelor's and Master's)
      TRUE ~ NA_real_
    ),
    
    # SE
    Higher_edu_SE = case_when(
      S9_SE %in% 1:3 ~ 0,   # Compulsory or General Upper Secondary education
      S9_SE %in% 4 ~ 1,     # Technical/practical/occupational/research higher education
      TRUE ~ NA_real_
    ),
    
    # CH
    Higher_edu_CH = case_when(
      S9_CH %in% 1:5 ~ 0,  # Primary to Vocational Secondary education
      S9_CH %in% 6:8 ~ 1,  # University degree, Master's, or Doctorate
      TRUE ~ NA_real_
    ),
    
    # NL
    Higher_edu_NL = case_when(
      S9_NL %in% 1:2 ~ 1,  # HBO, University degree (Bachelor's, Master's, PhD)
      S9_NL %in% 3:7 ~ 0,  
      TRUE ~ NA_real_
    ),
    
    # CA
    Higher_edu_CA = case_when(
      S9_CA %in% 1:6 ~ 0,  # Primary, some high school, or graduated high school
      S9_CA %in% 7:8 ~ 1,  # University undergraduate degree or higher
      TRUE ~ NA_real_
    ),
    
    # BE
    Higher_edu_BE = case_when(
      S9_BE %in% 1:6 ~ 0,  # Primary, lower secondary, or professional upper secondary education
      S9_BE %in% 7:10 ~ 1, # Bachelor's, Master's, or Doctorate
      TRUE ~ NA_real_
    ),
    
    # IL
    Higher_edu_IL = case_when(
      S9_IL %in% 1:3 ~ 0,  # No formal education or High school diploma
      S9_IL %in% 4:5 ~ 1,  # Bachelor's degree or above
      TRUE ~ NA_real_
    ),
    
    # Merge into one variable
    Higher_edu = coalesce(
      Higher_edu_US, Higher_edu_UK, Higher_edu_FR, Higher_edu_SE,
      Higher_edu_CH, Higher_edu_NL, Higher_edu_CA, Higher_edu_BE, Higher_edu_IL
    )
  )

#------------------ Dwelling Characteristics Variables -------------------------
#Energy costs
#C37_1: energy costs US
#C37_2: energy costs FR, NL and BE
#C37_3: energy costs UK
#C37_4: energy costs SE
#C37_5: energy costs CH
#C37_6: energy costs IL
#C37_8: energy costs CA
#creating new variable with merged info on average monthly cost
epic_Appliances <- epic_Appliances %>%
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
#S5
#creating new variable of home ownership with only two categories owned/not owned
epic_Appliances <- epic_Appliances %>%
  mutate(
    Home_ownership = case_when(
      S5 == 1 ~ 1,           # Owned
      S5 %in% c(2, 3) ~ 0,   # Not owned
      TRUE ~ NA_real_        # Preserve NAs
    )
  )

#Dwelling
#S18
#creating new variable of home ownership with only two categories owned/not owned
epic_Appliances <- epic_Appliances %>%
  mutate(
    Dwelling_house = case_when(
      S18 == 3 ~ 1, #house
      S18 == 4 ~ 1, #house
      S18 %in% c(1, 2, 89) ~ 0,   #apartment/other
      TRUE ~ NA_real_ #NAs
    )
  )

#Dwelling size
#S19_X
#creating new variable for mergerd info auf dwelling size (m2 and ft2)
epic_Appliances <- epic_Appliances %>%
  mutate(
    Dwelling_size = case_when(
      !is.na(S19_1) & S19_1 != 888888 ~ S19_1,
      !is.na(S19_1_1) & S19_1_1 != 888888 ~ S19_1_1,
      TRUE ~ NA_integer_
    )
  )

#Rural
#S20 - area
#creating new variable of home ownership with only two categories owned/not owned
epic_Appliances <- epic_Appliances %>%
  mutate(
    Rural = case_when(
      S20 %in% c(3, 4, 89) ~ 1,
      S20 %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    )
  )

#------------------------- Environmental Preference  ---------------------------
#Environmental concern
#B23_1
#create new variable with binary level for low and high environmental concern
epic_Appliances <- epic_Appliances %>%
  mutate(
    Env_concern = ifelse(B23_1 == 999999, NA, B23_1),
    Env_concern = case_when(
      Env_concern %in% c(4, 5) ~ 1, #high
      Env_concern %in% c(1, 2, 3) ~ 0, #low
      TRUE ~ NA_real_
    )
  )

#Environmental issues should be resolved mainly through public policies
##Strongly disagree = 1, Disagree = 2, Neither agree or disagree = 3, Agree = 4, Strongly agree = 5, Prefer not to say = 999999
#Environmental policies introduced by the government should not cost me extra money
##Strongly disagree = 1, Disagree = 2, Neither agree or disagree = 3, Agree = 4, Strongly agree = 5, Prefer not to say = 999999
#In support of providing subsidies to households for purchasing energy-efficient appliances or investing in renewable energy equipment
##Strongly against = 1, against = 2, indifferent = 3, support = 4, Strongly support = 5
#In support of Taxing energy use or the purchase of appliances and equipment that use a lot of energy
##Strongly against = 1, against = 2, indifferent = 3, support = 4, Strongly support = 5
#In support of introducing energy efficiency standards for appliances and buildings that manufacturers have to comply with
##Strongly against = 1, against = 2, indifferent = 3, support = 4, Strongly support = 5
#In support of low-income households receiving government support to help them pay for energy equipment
##Yes = 1, No = 2, Don´t know = 3 

epic_Appliances <- epic_Appliances %>%
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
    Env_policy_subsidy = case_when(
      C49_1 %in% c(4, 5) ~ 1,  
      C49_1 %in% 1:3 ~ 0  
    ),
    Env_policy_tax = case_when(
      C49_2 %in% c(4, 5) ~ 1,  
      C49_2 %in% 1:3 ~ 0  
    ),
    Env_policy_standards = case_when(
      C49_3 %in% c(4, 5) ~ 1,  
      C49_3 %in% 1:3 ~ 0  
    ),
    Env_policy_liH = case_when(
      C50 %in% 1 ~ 1,  
      C50 %in% 2:3 ~ 0  
    ),
  )

#---------------------------- Selected Data Set  -------------------------------

#---------------------------- 3.2 EPS Data -------------------------------------


























