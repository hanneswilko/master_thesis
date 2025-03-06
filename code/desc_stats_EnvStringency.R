#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?

#-----------------------------Data wrangling------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr","jsonlite")

## OECD carbon rate
C02rate_data <- read.csv("./processed_data/OECD_CO2rate_data.csv")
glimpse(C02rate_data)

## OECD EPS
EPS_data <- read.csv("./processed_data/OECD_EPS_data.csv")
glimpse(EPS_data)
unique(EPS_data$TIME_PERIOD)

## Our World in Data - weighted carbon price
wC02price_data <- read.csv("./processed_data/OWiD_C02price_data.csv")
glimpse(wC02price_data)

## OECD PPP rates
OECD_PPP_data <- read.csv("./raw_data/OECD_PPP_2023.csv")
glimpse(OECD_PPP_data)
unique(OECD_PPP_data$CURRENCY)

OECD_PPP <- OECD_PPP_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  filter(REF_AREA %in% c("USA", "GBR", "FRA", "NLD", "SWE",
                         "CHE", "ISR", "CAN", "BEL")) %>%
  rename(PPP_value = OBS_VALUE)
glimpse(OECD_PPP)

#------------------------Environmental Stringency-------------------------------
##EPS
###time frame: 2010-2020 (2020 latest observations)
EPS_data_sub <- EPS_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  filter(TIME_PERIOD %in% 2010:2020)

summary(EPS_data_sub)

###Box Plot for EPS per country
EPS_boxplot <- ggplot(EPS_data_sub, aes(x = REF_AREA, y = OBS_VALUE)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.colour = "black", outlier.size = 2) +
  labs(title = "OECD Environmental Policy Stringency Index (2010-2020)", 
       y = "EPS", 
       x = "Country") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black", size = 12),  # Y-axis text formatting
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),  # X-axis formatting
    axis.title = element_text(size = 14, face = "bold", color = "black")  # Bold axis labels
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))  # Format y-axis to 2 decimals

###Average EPS per country
EPS_sub_avg <- EPS_data_sub %>%
  group_by(REF_AREA) %>%
  summarize(avg_EPS = mean(OBS_VALUE, na.rm = TRUE)) #use summarize to collapse data
View(EPS_sub_avg)

EPS_hist <- ggplot(EPS_sub_avg, aes(x = REF_AREA, y = avg_EPS)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "OCED Environmental Policy Stringency Index (2010-2020)",
       x = "Country",
       y = "Mean EPS") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black", size = 12),  # Y-axis text formatting
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),  # X-axis formatting
    axis.title = element_text(size = 14, face = "bold", color = "black")  # Bold axis labels
  ) +
  scale_y_continuous(
    breaks = seq(0, 5, by = 1),  # Adjusted breaks for better readability
    limits = c(0, 5),  # Adjusted scale (modify based on your data range)
    labels = scales::number_format(accuracy = 0.1)  # Keeps two decimal places
  )

#--------------------------- OECD Carbon Rate ----------------------------------
##Net effective carbon rate and carbon tax
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

### Merge the two data frames by REF_AREA (country) and TIME_PERIOD (year)
C02rate_data_sub_PPP <- C02rate_data_sub %>%
  left_join(OECD_PPP, by = c("REF_AREA", "TIME_PERIOD"))

C02rate_data_sub_PPP <- C02rate_data_sub_PPP %>%
  mutate(
    comparative_tax_usd = OBS_VALUE / PPP_value
  ) # Add a new column with the PPP-adjusted tax in USD

###Box Plot for Carbon Tax Rate per country
###no meaningful visualization to distribution across countries with extreme values

###Average Carbon Tax Rate per country
C02rate_data_sub_PPPavg <- C02rate_data_sub_PPP %>%
  group_by(REF_AREA) %>%
  summarize(comparative_tax_usd_avg = mean(comparative_tax_usd, na.rm = TRUE)) #use summarize to collapse data
View(C02rate_data_sub_PPPavg)

C02rate_hist <- ggplot(C02rate_data_sub_PPPavg, aes(x = REF_AREA, y = comparative_tax_usd_avg)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(
    title = "Carbon Tax Burden (2018, 2021, 2023)",
    x = "Country",
    y = "Mean Carbon Tax Rate\n(PPP-adjusted in 2023 USD/tCO2)"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black", size = 10),  # Reduced Y-axis label size
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),  # X-axis formatting
    axis.title = element_text(size = 12, face = "bold", color = "black")  # Reduced title size for axes
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 10),  # Adjusted breaks for better readability
    limits = c(0, 50),  # Adjusted scale (modify based on your data range)
  )

#--------------------------- OWiD Carbon Rate weighted -------------------------
##Net effective carbon rate and carbon tax
###data: 
###obs_value: emissions-weighted carbon price - expressed in 2021USD/tCO2
wC02price_data_sub <- wC02price_data %>%
  select(Code, Year, price_with_tax_weighted_by_share_of_co2) %>%
  filter(Year %in% 2010:2020)

###Box Plot for Emissions-Weighted Carbon Price per country
###no meaningful visualization to distribution across countries with extreme values

###Average EPS per country
wC02price_data_sub_avg <- wC02price_data_sub %>%
  group_by(Code) %>%
  summarize(price_with_tax_weighted_by_share_of_co2_avg = mean(price_with_tax_weighted_by_share_of_co2, na.rm = TRUE)) #use summarize to collapse data
View(wC02price_data_sub_avg)

wC02rate_hist <- ggplot(wC02price_data_sub_avg, aes(x = Code, y = price_with_tax_weighted_by_share_of_co2_avg)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Emissions-Weighted Carbon Price (2010-2020)",
       x = "Country",
       y = "Mean Weigthed Carbon Price\n(2021 USD/tCO2)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black", size = 12),  # Y-axis text formatting
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),  # X-axis formatting
    axis.title = element_text(size = 14, face = "bold", color = "black")  # Bold axis labels
  ) +
  scale_y_continuous(
    breaks = seq(0, 70, by = 10),  # Adjusted breaks for better readability
    limits = c(0, 70),  # Adjusted scale (modify based on your data range)
  )

#--------------------------- Saving Graphs -------------------------------------

ggsave("./output/EPS_boxplot.png", plot = EPS_boxplot, width = 8, height = 6, dpi = 300)
ggsave("./output/EPS_hist.png", plot = EPS_hist, width = 8, height = 6, dpi = 300)
ggsave("./output/C02rate_hist.png", plot = C02rate_hist, width = 8, height = 6, dpi = 300)
ggsave("./output/wC02rate_hist.png", plot = wC02rate_hist, width = 8, height = 6, dpi = 300)





