#---------------------------- Libraries ----------------------------------------
# Load libraries
pacman::p_load(tidyr, dplyr, stringr, openxlsx, zoo, reshape2, data.table,
               ggplot2, forcats, tidytext, tidyverse, corrplot, kableExtra)

#------------------------------- Data ------------------------------------------
pams <- read.csv("./raw_data/IEA_PAMS.csv") #IEA policies overview
pine <- read.csv("./raw_data/OECD_PINE.csv") #OECD policies overview
rise <- read.csv("./raw_data/WB_RISE.csv") #World Banks policies overview

View(pams)
View(pine)
View(rise)

unique(pams$ISO3)
unique(pine$CountryCode)
unique(rise$REF_AREA)

pams <- pams %>%
  mutate(REF_AREA = case_when(
    ISO3 == "USA" ~ "US",
    ISO3 == "GBR" ~ "UK",
    ISO3 == "FRA" ~ "FR",
    ISO3 == "NLD" ~ "NL",
    ISO3 == "SWE" ~ "SE",
    ISO3 == "CHE" ~ "CH",
    ISO3 == "ISR" ~ "IL",
    ISO3 == "CAN" ~ "CA",
    ISO3 == "BEL" ~ "BE",
    TRUE ~ NA_character_  # Assign NA if no match
  ))

pine <- pine %>%
  mutate(REF_AREA = case_when(
    CountryCode == "USA" ~ "US",
    CountryCode == "GBR" ~ "UK",
    CountryCode == "FRA" ~ "FR",
    CountryCode == "NLD" ~ "NL",
    CountryCode == "SWE" ~ "SE",
    CountryCode == "CHE" ~ "CH",
    CountryCode == "ISR" ~ "IL",
    CountryCode == "CAN" ~ "CA",
    CountryCode == "BEL" ~ "BE",
    TRUE ~ NA_character_  # Assign NA if no match
  )) 

rise <- rise %>%
  mutate(REF_AREA = case_when(
    REF_AREA == "USA" ~ "US",
    REF_AREA == "GBR" ~ "UK",
    REF_AREA == "FRA" ~ "FR",
    REF_AREA == "NLD" ~ "NL",
    REF_AREA == "SWE" ~ "SE",
    REF_AREA == "CHE" ~ "CH",
    REF_AREA == "ISR" ~ "IL",
    REF_AREA == "CAN" ~ "CA",
    REF_AREA == "BEL" ~ "BE",
    TRUE ~ REF_AREA
  ))

#-------------------------------- PAMS - IEA -----------------------------------
glimpse(pams)
unique(pams$year) #2010-2020
unique(pams$status) #In force and Ended
unique(pams$type) #2010-2020

pams_summary <- pams %>%
  select(-source, -country, -ISO3) %>%
  filter(year %in% 2010:2022, status %in% c("In force", "Ended"), jurisdiction == "National",
         topic == "Buildings") %>%
  mutate(
    type_summary = case_when(
      grepl("Regulations|Ban|Codes|Permitting regimes|Obligation|Plan|Framework legislation", type, ignore.case = TRUE) ~ "Regulation",
      grepl("Government spending|Incentives and investments|Decent jobs|Universal energy access", type, ignore.case = TRUE) ~ "Financial Incentives",
      grepl("Carbon pricing|Emissions trading", type, ignore.case = TRUE) ~ "Carbon pricing",
      TRUE ~ "Other"
    )
  )

#-------------------------------- PINE - OECD ----------------------------------
glimpse(pine)

pine <- pine %>%
  select(where(~ !all(is.na(.))))

pine <- pine %>%
  select(CountryCode, InstrumentType, InstrumentType_Detail, InstrumentName_en,
         InstrumentDescription, Status, Status_EffectiveFirstEntry_Year, Status_Ended_Year, 
         Domain_EnergyEfficiency, GeographicScope, Reference_Name_1)

unique(pine$CountryCode)
unique(pine$Status) #Inactive and Active
unique(pine$Status_EffectiveFirstEntry_Year) #2010-2020
unique(pine$Status_Ended_Year) #2010-2020
unique(pine$GeographicScope) #"National / Federal"
unique(pine$Domain_EnergyEfficiency) #Yes
unique(pine$InstrumentType_Detail) #2010-2020

pine_summary <- pine %>%
  select(-Reference_Name_1) %>%
  filter(Status_EffectiveFirstEntry_Year %in% 2010:2020, GeographicScope == "National / Federal")

#-------------------------------- PINE - OECD ----------------------------------
glimpse(rise)

rise <- rise %>%
  select(REF_AREA, REF_AREA_LABEL, INDICATOR_LABEL,
         UNIT_MEASURE, TIME_PERIOD, OBS_VALUE)

unique(rise$REF_AREA) #SWE, GBR, USA, ISR, NLD, CAN, BEL, CHE, FRA
unique(rise$INDICATOR_LABEL)
unique(rise$TIME_PERIOD) #2010-2023

rise_summary <- rise %>%
  filter(REF_AREA %in% c("SE", "UK", "US", "IL", "NL", "CA", "BE", "CH", "FR"),
         TIME_PERIOD %in% 2010:2020,
         INDICATOR_LABEL %in% c("Building Energy Codes", "Energy Efficiency Governance",
                                "Energy Labeling Systems", "Financing Mechanisms for Energy Efficiency",
                                "Incentives & Mandates: Energy Utility Programs", "Minimum Energy Efficiency Performance Standards",
                                "Heating and Cooling Renewables"))

rise_avg <- rise_summary %>%
  group_by(REF_AREA, INDICATOR_LABEL) %>%
  summarize(avg_score = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")

#-------------------------------- Output ---------------------------------------
wb <- createWorkbook()

addWorksheet(wb, "pams_summary")
writeData(wb, "pams_summary", pams_summary)

addWorksheet(wb, "pine_summary")
writeData(wb, "pine_summary", pine_summary)

addWorksheet(wb, "rise_avg")
writeData(wb, "rise_avg", rise_avg)

# Save the workbook
saveWorkbook(wb, "./output/climate_policy_summary.xlsx", overwrite = TRUE)








