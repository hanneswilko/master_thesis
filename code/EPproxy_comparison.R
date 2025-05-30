#---------------------------- Libraries ----------------------------------------
# Load libraries
pacman::p_load(tidyr, dplyr, oenbr, stringr, openxlsx, zoo, reshape2, data.table,
               ggplot2, forcats, tidytext, tidyverse, corrplot)

#------------------------------- Data ------------------------------------------
EPS <- read.csv("G:/FINMA/01_Mitarbeiter/temporäre Mitarbeiter/FH- Praktikanten/WilkovitsHannes/Training/R_Scripts/EPS.csv")
CAPFM <- read.csv("G:/FINMA/01_Mitarbeiter/temporäre Mitarbeiter/FH- Praktikanten/WilkovitsHannes/Training/R_Scripts/CAPFM.csv")

View(EPS)
View(CAPFM)
glimpse(EPS)
glimpse(CAPFM)

#-------------------------------- EPS ------------------------------------------
EPS_sub <- EPS %>%
  filter(TIME_PERIOD %in% 2010:2020) %>%
  select(REF_AREA, TIME_PERIOD, OBS_VALUE) %>%
  arrange(REF_AREA, TIME_PERIOD)

EPS_avg <- EPS_sub %>%
  group_by(REF_AREA) %>% 
  summarize(avg_EPS = mean(OBS_VALUE, na.rm = T))

#------------------------------ CAPFM ------------------------------------------
CAPFM_sub <- CAPFM %>%
  filter(TIME_PERIOD %in% 2010:2020) %>%
  select(REF_AREA, Climate.actions.and.policies, TIME_PERIOD, OBS_VALUE) %>%
  arrange(REF_AREA, TIME_PERIOD)

glimpse(CAPFM_sub)
unique(CAPFM_sub$Climate.actions.and.policies)

CAPFM_wide <- CAPFM_sub %>%
  pivot_wider(
    names_from = Climate.actions.and.policies,
    values_from = OBS_VALUE
  )

CAPFM_avg <- CAPFM_wide %>%
  group_by(REF_AREA) %>% 
  summarize(
    avg_Buildings_MbI = mean(`Buildings - Market-based instruments`, na.rm = TRUE),
    avg_Buildings_NMbI = mean(`Buildings - Non market-based instruments`, na.rm = TRUE),
    avg_Sectoral_policies = mean(`Sectoral policies`, na.rm = TRUE),
    avg_CrossSectoral_policies = mean(`Cross-sectoral policies`, na.rm = TRUE)
  )

#------------------------------ Comparison -------------------------------------
glimpse(CAPFM_avg)
glimpse(EPS_avg)

#normalization of indicators since different scalas (EPS: 0-6 vs CAPFM 0-10)
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

CAPFM_avg_norm <- CAPFM_avg %>%
  mutate(
    avg_Buildings_MbI = normalize(avg_Buildings_MbI),
    avg_Buildings_NMbI = normalize(avg_Buildings_NMbI),
    avg_Sectoral_policies = normalize(avg_Sectoral_policies),
    avg_CrossSectoral_policies = normalize(avg_CrossSectoral_policies)
  )

EPS_avg_norm <- EPS_avg %>%
  mutate(
    avg_EPS = normalize(avg_EPS)
  )

#merge sets
Policy_indicators <- CAPFM_avg_norm %>%
  inner_join(EPS_avg_norm, by = "REF_AREA")

View(Policy_indicators)
glimpse(Policy_indicators)

#Visual analysis ---------------------------------------------------------------
Policy_indicators_long <- Policy_indicators %>% #data in long format for ggplot
  pivot_longer(
    cols = -REF_AREA,
    names_to = "Indicator",
    values_to = "Value"
  )

##Policy indicators per country
ggplot(Policy_indicators_long, aes(x = Indicator, y = Value, fill = Indicator)) +
  geom_col() +
  facet_wrap(~ REF_AREA, ncol = 4) +
  labs(
    title = "Normalized Indicators per Country",
    x = NULL,
    y = "Normalized Value (0-1)",
    fill = "Indicator"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "right"    # legend on the right side
  )

##Countries per Policy indicator
Policy_indicators_long2 <- Policy_indicators_long %>%
  mutate(REF_AREA = reorder_within(REF_AREA, Value, Indicator, FUN = mean, order_by = -Value))

ggplot(Policy_indicators_long2, aes(x = REF_AREA, y = Value, fill = Indicator)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Indicator, scales = "free_x", ncol = 2) +
  scale_x_reordered() +  # important to handle reordered factors inside facets
  labs(
    title = "Normalized Indicator Values by Country",
    x = "Country",
    y = "Normalized Value (0-1)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 11)
  )

#Statistical analysis ----------------------------------------------------------

##Simple correlation matrix
# Select only the numeric columns for correlation
num_data <- Policy_indicators %>%
  select(-REF_AREA) %>%
  na.omit()  # remove rows with NaN

# Calculate correlation matrix
cor_mat <- cor(num_data)

# Plot correlation matrix
corrplot(cor_mat, method = "color", addCoef.col = "black", tl.col = "black", 
         number.cex = 0.7, title = "Correlation Between Policy Indicators", mar = c(0,0,1,0))

##Correlations between EPS and rest
# Select relevant columns, drop rows with NaNs for simplicity
df <- Policy_indicators %>%
  select(REF_AREA, avg_EPS, avg_Buildings_MbI, avg_Buildings_NMbI, avg_Sectoral_policies, avg_CrossSectoral_policies) %>%
  na.omit()

# Calculate correlations of avg_EPS with all others
correlations <- df %>%
  summarise(
    cor_Buildings_MbI = cor(avg_EPS, avg_Buildings_MbI),
    cor_Buildings_NMbI = cor(avg_EPS, avg_Buildings_NMbI),
    cor_Sectoral_policies = cor(avg_EPS, avg_Sectoral_policies),
    cor_CrossSectoral_policies = cor(avg_EPS, avg_CrossSectoral_policies)
  )

print(correlations)













