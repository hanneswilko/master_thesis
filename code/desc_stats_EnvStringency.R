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

OECD_PPP <- OECD_PPP %>%
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
    TRUE ~ NA_character_  # Assign NA if no match
  ))

glimpse(OECD_PPP)

#------------------------Environmental Stringency-------------------------------
##EPS
###time frame: 2010-2020 (2020 latest observations)
EPS_data_sub <- EPS_data %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  filter(TIME_PERIOD %in% 2010:2020)

summary(EPS_data_sub)

EPS_boxplot <- ggplot(EPS_data_sub, aes(x = REF_AREA, y = OBS_VALUE)) +
  geom_boxplot(fill = "#8da0cb", outlier.colour = "black", outlier.size = 2) +  # Consistent blue fill
  labs(
    title = "OECD Environmental Policy Stringency Index (2010-2020)", 
    y = "EPS index", 
    x = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5), 
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))  # Format y-axis with 1 decimal

###Average EPS per country
EPS_sub_avg <- EPS_data_sub %>%
  group_by(REF_AREA) %>%
  summarize(avg_EPS = mean(OBS_VALUE, na.rm = TRUE)) #use summarize to collapse data
View(EPS_sub_avg)

EPS_hist <- ggplot(EPS_sub_avg, aes(x = REF_AREA, y = avg_EPS)) +
  geom_bar(stat = "identity", fill = "#8da0cb") +  # Updated fill color
  labs(
    title = "OECD Environmental Policy Stringency Index (2010-2020)",
    x = "Country",
    y = "Average EPS index"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5), 
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, 5, by = 1),  # Adjusted breaks for better readability
    limits = c(0, 5),  # Adjusted scale (modify based on your data range)
    labels = scales::number_format(accuracy = 0.1)  # Keeps one decimal place
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
  geom_bar(stat = "identity", fill = "#8da0cb") +  # Updated fill color
  labs(
    title = "OECD carbon tax burden (2018, 2021, 2023)",
    x = "Country",
    y = "Average carbon tax rate" # (PPP-adjusted in 2023 USD/tCO2)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5), 
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 10),  # Adjusted breaks for better readability
    limits = c(0, 50)  # Adjusted scale (modify based on your data range)
  )


#--------------------------- OWiD Carbon Rate weighted -------------------------
###obs_value: emissions-weighted carbon price - expressed in 2021USD/tCO2
wC02price_data_sub <- wC02price_data %>%
  select(Code, Year, price_with_tax_weighted_by_share_of_co2) %>%
  filter(Year %in% 2010:2020)

glimpse(wC02price_data)

###Box Plot for Emissions-Weighted Carbon Price per country
wC02rate_boxplot <- ggplot(wC02price_data_sub, aes(x = Code, y = price_with_tax_weighted_by_share_of_co2)) +
  geom_boxplot(fill = "#8da0cb", outlier.colour = "black", outlier.size = 2) +
  labs(
    title = "Emissions-weighted carbon price (2010-2020)", 
    y = "Weighted carbon price", 
    x = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5), 
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))


###Average Price per country
wC02price_data_sub_avg <- wC02price_data_sub %>%
  group_by(Code) %>%
  summarize(price_with_tax_weighted_by_share_of_co2_avg = mean(price_with_tax_weighted_by_share_of_co2, na.rm = TRUE)) #use summarize to collapse data
View(wC02price_data_sub_avg)

wC02rate_hist <- ggplot(wC02price_data_sub_avg, aes(x = Code, y = price_with_tax_weighted_by_share_of_co2_avg)) +
  geom_bar(stat = "identity", fill = "#8da0cb") +  # Matching color scheme
  labs(title = "Emissions-weighted carbon price (2010-2020)",
       x = "Country",
       y = "Average weighted carbon price") + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5), 
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, 70, by = 10),  # Adjusted breaks for better readability
    limits = c(0, 70)  # Adjusted scale (modify based on your data range)
  )

#--------------------------- Saving Graphs -------------------------------------
ggsave("./output/EPS_boxplot.pdf", plot = EPS_boxplot, dpi = 300, scale = 1.2)
ggsave("./output/EPS_hist.pdf", plot = EPS_hist, dpi = 300, scale = 1.2)

ggsave("./output/C02rate_hist.pdf", plot = C02rate_hist, dpi = 300, scale = 1.2)

ggsave("./output/wC02rate_boxplot.pdf", plot = wC02rate_boxplot, dpi = 300, scale = 1.2)
ggsave("./output/wC02rate_hist.pdf", plot = wC02rate_hist, dpi = 300, scale = 1.2)



