#---------------------------- Libraries ----------------------------------------
# Load libraries
pacman::p_load(tidyr, dplyr, stringr, openxlsx, zoo, reshape2, data.table,
               ggplot2, forcats, tidytext, tidyverse, corrplot, kableExtra)

#------------------------------- Data ------------------------------------------
EPS <- read.csv("./processed_data/OECD_EPS_data.csv")
CAPFM <- read.csv("./raw_data/CAPFM.csv")

View(EPS)
View(CAPFM)
unique(CAPFM$REF_AREA)
glimpse(EPS)
glimpse(CAPFM)

CAPFM <- CAPFM %>%
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
  select(REF_AREA, CLIM_ACT_POL, TIME_PERIOD, OBS_VALUE) %>%
  arrange(REF_AREA, TIME_PERIOD)

glimpse(CAPFM_sub)
unique(CAPFM_sub$CLIM_ACT_POL)

CAPFM_wide <- CAPFM_sub %>%
  pivot_wider(
    names_from = CLIM_ACT_POL,
    values_from = OBS_VALUE
  )

CAPFM_avg <- CAPFM_wide %>%
  group_by(REF_AREA) %>% 
  summarize(
    avg_Buildings_MbI = mean(LEV2_SEC_B_MBI, na.rm = TRUE),
    avg_Buildings_NMbI = mean(LEV2_SEC_B_NMBI, na.rm = TRUE),
    avg_Sectoral_policies = mean(LEV1_SEC, na.rm = TRUE),
    avg_CrossSectoral_policies = mean(LEV1_CROSS_SEC, na.rm = TRUE)
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

Policy_indicators <- Policy_indicators %>%
  rename(Country = REF_AREA) %>%
  rename_with(~ sub("^avg_", "", .x), .cols = starts_with("avg_"))

##table
Policy_indicators_ranked <- Policy_indicators %>%
  rowwise() %>%
  mutate(
    Overall = mean(c_across(c(Buildings_MbI, Buildings_NMbI, Sectoral_policies, CrossSectoral_policies, EPS)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Rank = rank(-Overall, ties.method = "min")
  ) %>%
  arrange(Rank)

Policy_indicators_ranked <- Policy_indicators_ranked %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

#Visual analysis ---------------------------------------------------------------
Policy_indicators_long <- Policy_indicators %>% 
  pivot_longer(
    cols = -Country,
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  filter(!(Country == "US" & is.na(Value))) %>%
  mutate(
    Indicator = case_when(
      Indicator == "Buildings_MbI" ~ "Buildings MbI",
      Indicator == "Buildings_NMbI" ~ "Buildings NMbI",
      Indicator == "Sectoral_policies" ~ "Sectoral Policies",
      Indicator == "CrossSectoral_policies" ~ "Cross-Sectoral Policies",
      Indicator == "EPS" ~ "EPS Index",
      TRUE ~ Indicator
    ),
    # Keep full country string for reordering
    Country_reorder = reorder_within(Country, Value, Indicator)
  )

# Plot with reordered factor
barchart_Policy_indicators_comparison <- ggplot(Policy_indicators_long, 
                                                aes(x = Country_reorder, y = Value)) +
  geom_col(fill = "#8da0cb", show.legend = FALSE) +
  facet_wrap(~ Indicator, scales = "free_x", ncol = 2) +
  scale_x_reordered(labels = function(x) str_remove(x, "___.*")) + # clean labels here
  labs(
    title = "Normalized Policy Indicators by Country",
    x = "Country",
    y = "Normalized Value (0-1)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(color = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    strip.text.x = element_text(size = 10, face = "bold"),
    panel.grid.major.y = element_line(color = "gray", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

#Statistical analysis ----------------------------------------------------------
##Simple correlation matrix
num_data <- Policy_indicators %>%
  select(-Country) %>%
  na.omit()  # remove rows with NaN

# Calculate correlation matrix
cor_mat <- cor(num_data, use = "complete.obs", method = "pearson")
cor_mat_spearman <- cor(num_data, use = "complete.obs", method = "spearman")

## rename policy indicators
cormat_names <- c(
  "Buildings MbI", 
  "Buildings NMbI", 
  "Sectoral Policies", 
  "Cross-Sectoral Policies", 
  "EPS Index"
)

rownames(cor_mat) <- cormat_names
colnames(cor_mat) <- cormat_names

# Plot Pearson correlation matrix
png("./output/output_desc_stats/corrplot_policy_indicators.pdf", width = 800, height = 600)

corrplot_policy_indicators <- corrplot(
  cor_mat,
  method = "color",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,           # 1) Tilt column labels 45 degrees
  number.cex = 0.7,
  title = "Correlation between Policy Indicators",
  mar = c(0, 0, 2, 0),   # 2) Increase top margin (last number is top space)
  title.cex = 1.2        # Optional: make title a bit bigger
)


dev.off()

# Plot Spearman correlation matrix
corrplot_spearman_policy_indicators <- corrplot(
  cor_mat_spearman, method = "color", addCoef.col = "black", tl.col = "black", 
  number.cex = 0.7, title = "Spearman Correlation Between Policy Indicators", mar = c(0,0,1,0)
)

##Correlations between EPS and rest
df <- Policy_indicators %>%
  select(Country, EPS, Buildings_MbI, Buildings_NMbI, Sectoral_policies, CrossSectoral_policies) %>%
  na.omit()

# Calculate correlations of avg_EPS with all others
correlations <- df %>%
  summarise(
    cor_Buildings_MbI = cor(EPS, Buildings_MbI),
    cor_Buildings_NMbI = cor(EPS, Buildings_NMbI),
    cor_Sectoral_policies = cor(EPS, Sectoral_policies),
    cor_CrossSectoral_policies = cor(EPS, CrossSectoral_policies)
  ) %>%
  mutate(Indicator = "EPS", .before = 1)

correlations <- correlations %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

#--------------------------- Saving Graphs -------------------------------------
ggsave("./output/output_desc_stats/barchart_Policy_indicators_comparison.pdf", plot = barchart_Policy_indicators_comparison, dpi = 300, scale = 1.2)

#--------------------------- Ouput Tables --------------------------------------

Policy_indicators_ranked %>%
  kbl(
    caption = "Normalized Policy Indicators ranked",
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped"),
    font_size = 10
  )

correlations %>%
  kbl(
    caption = "EPS Correlation with other Policy Indicators",
    format = "latex",
    booktabs = TRUE
  ) %>%
  kable_styling(
    latex_options = c("striped"),
    font_size = 10
  )








