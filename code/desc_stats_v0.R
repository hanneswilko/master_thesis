#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?
##OECD: EPIC 2022

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr","jsonlite")

#load data
## EPIC
epic_data <- read.csv("./processed_data/epic_data_var_interest.csv")
glimpse(epic_data)

## OECD carbon rate
C02rate_data <- read.csv("./processed_data/OCED_CO2rate_data.csv")
glimpse(C02rate_data)

## OECD EPS
EPS_data <- read.csv("./processed_data/OCED_EPS_data.csv")
glimpse(EPS_data)

## Our World in Data - weighted carbon price
wC02price_data <- read.csv("./processed_data/OWiD_C02price_data.csv")
glimpse(wC02price_data)

#------------------------Environmental Stringency-------------------------------
##EPS
###time frame: 2010-2020 (2020 latest observations)
EPS_data_sub <- EPS_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  filter(TIME_PERIOD %in% 2010:2020)

summary(EPS_data_sub)

###Box Plot for EPS per country
ggplot(EPS_data_sub, aes(x = REF_AREA, y = OBS_VALUE)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.colour = "black", outlier.size = 2) +
  labs(title = "Distribution of EPS by Country (2010-2020)", 
       y = "EPS", 
       x = "Country") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black", size = 12),  # Y-axis text formatting
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),  # X-axis formatting
    axis.title = element_text(size = 14, face = "bold", color = "black")  # Bold axis labels
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))  # Format y-axis to 2 decimals

###Average EPS per country
EPS_sub_avg <- EPS_data_sub %>%
  group_by(REF_AREA) %>%
  summarize(avg_EPS = mean(OBS_VALUE, na.rm = TRUE)) #use summarize to collapse data
View(EPS_sub_avg)

ggplot(EPS_sub_avg, aes(x = REF_AREA, y = avg_EPS)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Average EPS by Country (2010-2020)",
       x = "Country",
       y = "Mean EPS") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black", size = 12),  # Y-axis text formatting
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),  # X-axis formatting
    axis.title = element_text(size = 14, face = "bold", color = "black")  # Bold axis labels
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))  # Format y-axis to 2 decimals

#--------------------------- OECD Carbon Rate ----------------------------------
##Net effective carbon rate and carbon tax
###time frame: 2010-2020 (2020 latest observations)
glimpse(C02rate_data)
unique(C02rate_data$TIME_PERIOD)
unique(C02rate_data$CURRENCY)
unique(C02rate_data$BASE_PER)

##Subset Carbon tax
###CARBTAX
C02rate_data_sub <- C02rate_data %>%
  select(-FREQ) %>%
  filter(MEASURE %in% "CARBTAX")

glimpse(C02rate_data_sub)











