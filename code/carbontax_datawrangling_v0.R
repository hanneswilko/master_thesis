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

##------------------------------OECD--------------------------------------------
carbonrate_OECD_raw <- read.csv("./raw_data/OECD_carbonrates.csv")
head(carbonrate_OECD_raw)
class(carbonrate_OECD_raw)
glimpse(carbonrate_OECD_raw)
View(carbonrate_OECD_raw)

EPS_OECD_raw <- read.csv("./raw_data/OECD_EPS.csv")
head(EPS_OECD_raw)
class(EPS_OECD_raw)
glimpse(EPS_OECD_raw)
View(EPS_OECD_raw)
