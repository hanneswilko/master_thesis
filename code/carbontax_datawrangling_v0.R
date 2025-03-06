#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr","jsonlite")

##-----------------------------Our World in Data--------------------------------
#fetch the data
carbonprice_OWiD_raw <- read.csv("https://ourworldindata.org/grapher/emissions-weighted-carbon-price.csv?v=1&csvType=full&useColumnShortNames=true")

head(carbonprice_OWiD_raw)
class(carbonprice_OWiD_raw)
glimpse(carbonprice_OWiD_raw)
View(carbonprice_OWiD_raw)

###Carbon Price
unique(carbonprice_OWiD_raw$Code)
unique(carbonprice_OWiD_raw$Year)
carbonprice_OWiD_clean <- carbonprice_OWiD_raw %>%
  filter(Code %in% c("USA", "GBR", "FRA", "NLD", "SWE",
                         "CHE", "ISR", "CAN", "BEL")) %>%
  filter(Year %in% 2010:2021)

glimpse(carbonprice_OWiD_clean)
unique(carbonprice_OWiD_clean$Entity)
View(carbonprice_OWiD_clean)

####save subset of interest as .csv
write.csv(carbonprice_OWiD_clean, "./processed_data/OWiD_C02price_data.csv", row.names = FALSE)

##------------------------------OECD--------------------------------------------
###Carbon Rate
carbonrate_OECD_raw <- read.csv("./raw_data/OECD_carbonrates.csv")
head(carbonrate_OECD_raw)
class(carbonrate_OECD_raw)
glimpse(carbonrate_OECD_raw)
View(carbonrate_OECD_raw)

unique(carbonrate_OECD_raw$REF_AREA)
unique(carbonrate_OECD_raw$MEASURE)
carbonrate_OECD_clean <- carbonrate_OECD_raw %>%
  select(REF_AREA, FREQ, MEASURE, TIME_PERIOD, PRICE_BASE,
         BASE_PER, CURRENCY, OBS_VALUE) %>%
  filter(REF_AREA %in% c("USA", "GBR", "FRA", "NLD",
                         "SWE", "CHE", "ISR", "CAN", "BEL"))

glimpse(carbonrate_OECD_clean)
View(carbonrate_OECD_clean)

####save subset of interest as .csv
write.csv(carbonrate_OECD_clean, "./processed_data/OECD_CO2rate_data.csv", row.names = FALSE)

###EPS
EPS_OECD_raw <- read.csv("./raw_data/OECD_EPS.csv")
head(EPS_OECD_raw)
class(EPS_OECD_raw)
glimpse(EPS_OECD_raw)
View(EPS_OECD_raw)

unique(EPS_OECD_raw$REF_AREA)
EPS_OECD_clean <- EPS_OECD_raw %>%
  select(REF_AREA, FREQ, MEASURE, CLIM_POL, TIME_PERIOD, OBS_VALUE) %>%
  filter(REF_AREA %in% c("USA", "GBR", "FRA", "NLD",
                         "SWE", "CHE", "ISR", "CAN", "BEL"))

glimpse(EPS_OECD_clean)
View(EPS_OECD_clean)

####save subset of interest as .csv
write.csv(EPS_OECD_clean, "./processed_data/OECD_EPS_data.csv", row.names = FALSE)



