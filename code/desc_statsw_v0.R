#-------------------------Environmental Stringency Data-------------------------
#Data sources:
##OECD: Net effective carbon rates
##OECD: OECD Environmental Policy Stringency Index
##Our World in Data: Which countries have put a price on carbon?
#-----------------------------Loading Data -------------------------------------
#packages
pacman::p_load("dplyr","ggplot2","tidyverse","haven","data.table","tidyr",
               "srvyr", "survey", "ggsurvey")

#EPIC
epic <- read.csv("./processed_data/epic_data_var_interest.csv")

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
  select(X, weight, weight_2, Country_code, Country_name, Age_cat, Income, S5,
         S18, B23_1, B31_1, B31_3, B31_5, B31_6, B31_7, B31_8, C49_1, C49_2,
         C49_3, C44_1, C44_2, C44_3, C44_4, C44_6, C44_7, C44_9, C45_1, C45_3, C45_4,
         C45_6, C45_7, C45_9, C46_1, C46_2, C46_3, C46_5, C46_6, C46_8, C47_2, C47_6)

glimpse(epic)
View(epic)

#-------------------------------------------------------------------------------
#------------------------------- Weights ---------------------------------------
#-------------------------------------------------------------------------------
summary(epic$weight)
summary(epic$weight_2)

#design weights
epic %>%
  group_by(Country_name) %>%
  summarize(min_weight = min(weight),
            max_weight = max(weight))
epic %>%
  group_by(Country_name) %>%
  summarize(n = n(),
            mean = mean(weight))

#post-stratification weights
epic %>%
  group_by(Country_name) %>%
  summarize(min_weight = min(weight_2),
            max_weight = max(weight_2))
epic %>%
  group_by(Country_name) %>%
  summarize(n = n(),
            mean = mean(weight_2))

#survey design
epic %>%
  as_survey(weights = weight_2) %>%
  group_by(Country_name) %>%
  summarize(n = survey_total())

##no weights vs. weights
mean(epic$Income, na.rm = T)

epic %>%
  as_survey(weights = weight_2) %>%
  summarize(mean_income = survey_mean(Income, na.rm = T))

mean(epic$C45_6[epic$C45_6 != 888888], na.rm = T)

epic %>%
  mutate(C45_6 = ifelse(C45_6 == 888888, NA, C45_6)) %>%
  as_survey(weights = weight_2) %>%
  summarize(mean_income = survey_mean(C45_6, na.rm = TRUE))

epic %>%
  mutate(C45_6 = ifelse(C45_6 == 888888, NA, C45_6)) %>%
  as_survey(weights = c(weight, weight_2)) %>% #design + post-strat weights
  summarize(mean_income = survey_mean(C45_6, na.rm = TRUE))

#plotting outcome with and without weights
##without weights
solarpan <- epic %>%
  mutate(C45_6 = ifelse(C45_6 == 888888, NA, C45_6)) %>%
  filter(!is.na(C45_6) & C45_6 == 1) %>%  
  group_by(Country_name, Income) %>%
  summarize(count = n(), .groups = "drop")f

ggplot(solarpan, aes(x = Country_name, y = count, fill = as.factor(Income))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Solar Panel Adoption by Country and Income Level",
       x = "Country",
       y = "Number of Adopters",
       fill = "Income Level") +
  theme_minimal()

##with weights - weight
solarpan_w0 <- epic %>%
  as_survey(weights = weight) %>%
  mutate(C45_6 = ifelse(C45_6 == 888888, NA, C45_6)) %>%
  filter(!is.na(C45_6) & C45_6 == 1) %>%  
  group_by(Country_name, Income) %>%
  summarize(count = n(), .groups = "drop")

ggplot(solarpan_w0, aes(x = Income, y = count)) +
  geom_bar(aes(fill = Income), stat = "identity", position = "dodge") +
  facet_wrap(~Country_name) +
  theme_bw()

##with weights - weight_2
solarpan_w1 <- epic %>%
  as_survey(weights = weight_2) %>%
  mutate(C45_6 = ifelse(C45_6 == 888888, NA, C45_6)) %>%
  filter(!is.na(C45_6) & C45_6 == 1) %>%  
  group_by(Country_name, Income) %>%
  summarize(count = n(), .groups = "drop")

ggplot(solarpan_w1, aes(x = Income, y = count)) +
  geom_bar(aes(fill = Income), stat = "identity", position = "dodge") +
  facet_wrap(~Country_name) +
  theme_bw()

##with both weights
solarpan_w2 <- epic %>%
  as_survey(weights = c(weight, weight_2)) %>%
  mutate(C45_6 = ifelse(C45_6 == 888888, NA, C45_6)) %>%
  filter(!is.na(C45_6) & C45_6 == 1) %>%  
  group_by(Country_name, Income) %>%
  summarize(count = n(), .groups = "drop")

ggplot(solarpan_w2, aes(x = Income, y = count)) +
  geom_bar(aes(fill = Income), stat = "identity", position = "dodge") +
  facet_wrap(~Country_name) +
  theme_bw()

























