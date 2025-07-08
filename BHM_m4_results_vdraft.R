#-------------------------------------------------------------------------------
#--------------- Bayesian hierarchical model - Heatpumps -------------------------
#-------------------------------------------------------------------------------
# 1. Loading Packages
# 2. Loading Data
# 3. BHM evaluation
# 4. BHM comparison
# 5. BHM results

#-------------------------------------------------------------------------------
#------------------------ 1. Loading Packages ----------------------------------
#-------------------------------------------------------------------------------
pacman::p_load("bayesrules", "tidyverse", "janitor", "rstanarm",
               "bayesplot", "tidybayes", "broom.mixed", "modelr",
               "e1071", "forcats", "dplyr", "ggplot2", "loo", "readr",
               "knitr", "kableExtra", "purrr")

#-------------------------------------------------------------------------------
#-------------------------- 2. Loading Data ------------------------------------
#-------------------------------------------------------------------------------
#data technology
windows <- read.csv("./processed_data/windows.csv")
appliances <- read.csv("./processed_data/appliances.csv")
insulation <- read.csv("./processed_data/insulation.csv")
solare <- read.csv("./processed_data/solare.csv")
heatpumps <- read.csv("./processed_data/heatpumps.csv")

#data model m4
windows_m4 <- read_rds("./output/models_rds/fitWindows_m4.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Income
appliances_m4 <- read_rds("./output/models_rds/fitAppliances_m4.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Income
insulation_m4 <- read_rds("./output/models_rds/fitInsulation_m4.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Income
solare_m4 <- read_rds("./output/models_rds/fitSolare_m4.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Income
heatpumps_m4 <- read_rds("./output/models_rds/fitHeatpumps_m4.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Income

models <- list(
  windows_m4 = windows_m4,
  appliances_m4 = appliances_m4,
  insulation_m4 = insulation_m4,
  solare_m4 = solare_m4,
  heatpumps_m4 = heatpumps_m4
)

models_data <- list(
  windows_m4 = windows,
  appliances_m4 = appliances,
  insulation_m4 = insulation,
  solare_m4 = solare,
  heatpumps_m4 = heatpumps
)

m4_array <- as.array(windows_m4) #any tech_m4 model - just to separate for type of effect
m4_all_pars <- dimnames(m4_array)$parameters

m4_fixed_pars <- m4_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m4_all_pars)]
m4_random_pars <- m4_all_pars[grepl("^(b\\[|Sigma|cor_)", m4_all_pars)]

#-------------------------------------------------------------------------------
#--------------------------- 3. BHM results ------------------------------------
#-------------------------------------------------------------------------------

#-------------------------- Posterior analysis ---------------------------------
tidy_rounded <- function(model, effect) {
  tidy(model, effects = effect, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

#Output per estimates ----------------------------------------------------------
extract_model_info <- function(model) {
  list(
    fixed = tidy_rounded(model, "fixed"),
    ran_vals = tidy_rounded(model, "ran_vals"),
    ran_pars = tidy_rounded(model, "ran_pars"),
    ci = posterior_interval(model, prob = 0.95) %>%
      as.data.frame() %>%
      round(2) %>%
      rownames_to_column(var = "Parameters") %>%
      select(Parameters, everything())
  )
}

model_outputs <- map(models, extract_model_info)

build_long_wide <- function(component, id_cols) {
  long_df <- imap_dfr(model_outputs, function(model_out, model_name) {
    df <- model_out[[component]]
    df$Model <- model_name
    df
  })
  
  long_df %>%
    pivot_wider(
      names_from = Model,
      values_from = setdiff(names(long_df), c(id_cols, "Model")),
      names_sep = "_"
    )
}

fixed_effects <- build_long_wide("fixed", c("term"))
ran_effects <- build_long_wide("ran_vals", c("term", "group", "level"))
ran_pars_df <- build_long_wide("ran_pars", c("term", "group"))
cintervals <- build_long_wide("ci", c("Parameters"))

#Fixed Effects
fixed_effects_labels <- c("(Intercept)" = "Intercept", "Age_cat25-34" = "Age: 25-34", "Age_cat35-44" = "Age: 35-44", "Age_cat45-54" = "Age: 45-54",
                          "Age_cat55+"   = "Age: 55+", "Female" = "Female", "Higher_edu" = "Higher education", "Home_ownership" = "Homeowner",
                          "Dwelling_house" = "D-Type: House", "Dwelling_size151–200 m²" = "D-size: 151–200 m²", "Dwelling_size26–50 m²" = "D-Size: 26–50 m²",
                          "Dwelling_size51–75 m²" = "D-Size: 51–75 m²", "Dwelling_size76–100 m²" = "D-Size: 76–100 m²", "Dwelling_sizeDon't know" = "D-Size: Don't know",
                          "Dwelling_sizeLess than 25 m²" = "D-Size: <25 m²", "Dwelling_sizeMore than 200 m²" = "D-Size: >200 m²", "Rural" = "D-Location: Rural",
                          "Env_concern" = "Environmental concern", "Gov_support" = "Government support",
                          "EPS" = "EPS index", "Incomequintile 2" = "Income q2", "Incomequintile 3" = "Income q3", "Incomequintile 4" = "Income q4",
                          "Incomequintile 5" = "Income q5", "EPS:Incomequintile 2" = "EPS × Income q2", "EPS:Incomequintile 3" = "EPS × Income q3",
                          "EPS:Incomequintile 4" = "EPS × Income q4", "EPS:Incomequintile 5" = "EPS × Income q5"
)

fixed_effects_df <- {
  fixed_wide_clean <- fixed_effects %>%
    select(term, matches("^(estimate|std\\.error)_"))
  
  tech_names <- fixed_wide_clean %>%
    select(-term) %>%
    names() %>%
    str_remove("^(estimate_|std\\.error_)") %>%
    unique()
  
  ordered_cols <- unlist(lapply(tech_names, function(tech) {
    c(paste0("estimate_", tech), paste0("std.error_", tech))
  }))
  
  fixed_wide_clean %>%
    select(term, all_of(ordered_cols))
}

fixed_effects_df <- fixed_effects_df %>%
  mutate(term = recode(term, !!!fixed_effects_labels))

#Random Effects
random_effect_labels <- c(
  # Intercepts by country
  "b[(Intercept) Country_name:BE]" = "Intercept (BE)",
  "b[(Intercept) Country_name:CA]" = "Intercept (CA)",
  "b[(Intercept) Country_name:CH]" = "Intercept (CH)",
  "b[(Intercept) Country_name:FR]" = "Intercept (FR)",
  "b[(Intercept) Country_name:IL]" = "Intercept (IL)",
  "b[(Intercept) Country_name:NL]" = "Intercept (NL)",
  "b[(Intercept) Country_name:SE]" = "Intercept (SE)",
  "b[(Intercept) Country_name:UK]" = "Intercept (UK)",
  "b[(Intercept) Country_name:US]" = "Intercept (US)",
  
  # EPS slopes by country
  "b[EPS Country_name:BE]" = "EPS effect (BE)",
  "b[EPS Country_name:CA]" = "EPS effect (CA)",
  "b[EPS Country_name:CH]" = "EPS effect (CH)",
  "b[EPS Country_name:FR]" = "EPS effect (FR)",
  "b[EPS Country_name:IL]" = "EPS effect (IL)",
  "b[EPS Country_name:NL]" = "EPS effect (NL)",
  "b[EPS Country_name:SE]" = "EPS effect (SE)",
  "b[EPS Country_name:UK]" = "EPS effect (UK)",
  "b[EPS Country_name:US]" = "EPS effect (US)",
  
  # Sigma covariance components
  "Sigma[Country_name:(Intercept),(Intercept)]" = "Var(Intercept)",
  "Sigma[Country_name:EPS,(Intercept)]" = "Cov(EPS, Intercept)",
  "Sigma[Country_name:EPS,EPS]" = "Var(EPS)"
)

ran_effects_df <- {
  ran_vals_wide_clean <- ran_effects %>%
    select(level, term, matches("^(estimate|std\\.error)_"))
  
  tech_names <- ran_vals_wide_clean %>%
    select(-level, -term) %>%
    names() %>%
    str_remove("^(estimate_|std\\.error_)") %>%
    unique()
  
  ordered_cols <- unlist(lapply(tech_names, function(tech) {
    c(paste0("estimate_", tech), paste0("std.error_", tech))
  }))
  
  ran_vals_wide_clean %>%
    select(level, term, all_of(ordered_cols))
}

#Confidence Intervals
ci_labels <- c(fixed_effects_labels, random_effect_labels)

cintervals_df <- {
  ci_wide_clean <- cintervals %>%
    select(Parameters, matches("^(2.5%|97.5%)_"))
  
  tech_names <- ci_wide_clean %>%
    select(-Parameters) %>%
    names() %>%
    str_remove("^(2.5%_|97.5%_)") %>%
    unique()
  
  ordered_cols <- unlist(lapply(tech_names, function(tech) {
    c(paste0("2.5%_", tech), paste0("97.5%_", tech))
  }))
  
  ci_wide_clean %>%
    select(Parameters, all_of(ordered_cols))
}

cintervals_df <- cintervals_df %>%
  mutate(Parameters = ifelse(!is.na(ci_labels[Parameters]), ci_labels[Parameters], Parameters))

#Within- and between-group variability
ran_pars_df <- ran_pars_df %>%
  mutate(term = case_when(
    term == "sd_(Intercept).Country_name" ~ "Var(Intercept)",
    term == "sd_EPS.Country_name" ~ "Var(EPS)",
    term == "cor_(Intercept).EPS.Country_name" ~ "Cov(EPS, Intercept)",
    TRUE ~ term  # fallback if no match
  )) %>%
  select(-group)

#Mean & CI per estimates -------------------------------------------------------
#Fixed effects with CI
fixed_ci_df <- fixed_effects_df %>%
  select(-starts_with("std.error")) %>%  # Drop std.error columns
  left_join(
    cintervals_df,
    by = c("term" = "Parameters")
  )

#Random effects with CI
cintervals_df_ran <- cintervals_df %>%
  filter(str_detect(Parameters, "^Intercept \\(.+\\)$|^EPS effect \\(.+\\)$"))

ran_ci_df <- cbind(ran_effects_df, cintervals_df_ran)

ran_ci_df <- ran_ci_df %>%
  select(-starts_with("std.error"), -Parameters)

techs <- c("windows", "appliances", "insulation", "solare", "heatpumps")

ordered_cols <- c("term")
for (tech in techs) {
  ordered_cols <- c(
    ordered_cols,
    paste0("estimate_", tech, "_m4"),
    paste0("2.5%_", tech, "_m4"),
    paste0("97.5%_", tech, "_m4")
  )
}

fixed_ci_df <- fixed_ci_df %>%
  select(all_of(ordered_cols))

ran_ci_df <- ran_ci_df %>%
  select(level, all_of(ordered_cols))

#-------------------------------------------------------------------------------
#------------------------------- 4. Output -------------------------------------
#-------------------------------------------------------------------------------

#--------------------------- EPS slope Plots -----------------------------------

eps_slope_df <- ran_ci_df %>% filter(term == "EPS")

eps_slope_df <- eps_slope_df %>%
  select(level, term, starts_with("estimate_"), starts_with("2.5%_"), starts_with("97.5%_")) %>%
  pivot_longer(
    cols = -c(level, term),
    names_to = c(".value", "tech"),
    names_pattern = "(estimate|2.5%|97.5%)_(.*)"
  )

colnames(eps_slope_df) <- c("Country", "Term", "Technology", "Mean", "CI Lower", "CI Upper")

eps_slope_df <- eps_slope_df %>%
  mutate(
    Technology = recode(
      Technology,
      "windows_m4" = "Windows",
      "appliances_m4" = "Appliances",
      "insulation_m4" = "Thermal insulation",
      "solare_m4" = "Solar panels",
      "heatpumps_m4" = "Heat pumps"
    )
  )

# Plotting function
plot_tech_effects <- function(tech_name) {
  eps_slope_df %>%
    filter(Term == "EPS", Technology == tech_name) %>%
    ggplot(aes(x = reorder(Country, Mean), y = Mean)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = `CI Lower`, ymax = `CI Upper`), width = 0.2, linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black") +
    labs(
      title = paste("Estimated Effect of EPS Slope on", str_to_title(tech_name), "Adoption by Country"),
      x = "Country",
      y = "Mean Estimated Effect"
    ) +
    theme_minimal(base_size = 14)
}

# Example: Plot for Windows
plot_tech_effects("Solar panels")

#---------------- Fixed effects Plots (except EPS and Gov support) -------------
fixed_long <- fixed_ci_df %>%
  filter(!term %in% c("Intercept", "EPS index", "Government support")) %>%
  select(term, starts_with("estimate_"), starts_with("2.5%_"), starts_with("97.5%_")) %>%
  pivot_longer(
    cols = -term,
    names_to = c(".value", "Technology"),
    names_pattern = "(estimate|2.5%|97.5%)_(.*)"
  )

colnames(fixed_long) <- c("Term", "Technology", "Mean", "CI Lower", "CI Upper")

fixed_long <- fixed_long %>%
  mutate(
    Technology = recode(
      Technology,
      "windows_m4" = "Windows",
      "appliances_m4" = "Appliances",
      "insulation_m4" = "Thermal insulation",
      "solare_m4" = "Solar panels",
      "heatpumps_m4" = "Heat pumps"
    )
  )

plot_fixed_effects <- function(tech_name) {
  fixed_long %>%
    filter(Technology == tech_name) %>%
    ggplot(aes(x = reorder(Term, Mean), y = Mean)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = `CI Lower`, ymax = `CI Upper`), width = 0.2, linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black") +
    labs(
      title = paste("Estimated Effect of Predictors on", tech_name, "Adoption"),
      x = NULL,
      y = "Mean Estimated Effect"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_fixed_effects("Solar panels")

#---------------- EPS and Gov support fixed effects Plots ----------------------
EPS_GOV_df <- fixed_ci_df %>%
  filter(term %in% c("EPS index", "Government support")) %>%
  pivot_longer(
    cols = -term,
    names_to = c(".value", "Technology"),
    names_pattern = "(estimate|2.5%|97.5%)_(.*)"
  )

colnames(EPS_GOV_df) <- c("Term", "Technology", "Mean", "CI Lower", "CI Upper")

EPS_GOV_df <- EPS_GOV_df %>%
  mutate(
    Technology = recode(
      Technology,
      "windows_m4" = "Windows",
      "appliances_m4" = "Appliances",
      "insulation_m4" = "Thermal insulation",
      "solare_m4" = "Solar panels",
      "heatpumps_m4" = "Heat pumps"
  ))

plot_EPS_GOV_effect <- function(predictor_label) {
  EPS_GOV_df %>%
    filter(Term == predictor_label) %>%
    ggplot(aes(x = reorder(Technology, Mean), y = Mean)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = `CI Lower`, ymax = `CI Upper`), width = 0.2, linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black") +
    labs(
      title = paste("Estimated Effect of", predictor_label, "on Technology Adoption"),
      x = "Technology",
      y = "Mean Estimated Effect"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


plot_EPS_GOV_effect("Government support")
plot_EPS_GOV_effect("EPS index")

#Saving plots ------------------------------------------------------------------
ggsave("./output/output_m4/EPS_effects_plot.pdf", plot = EPS_effects_plot, width = 9, height = 6)
ggsave("./output/output_m4/Govsupport_effects_plot.pdf", plot = Govsupport_effects_plot, width = 9, height = 6)

ggsave("./output/output_m4/EPS_slope_windows_plot.pdf", plot = EPS_slope_windows_plot, width = 10, height = 6)
ggsave("./output/output_m4/EPS_slope_appliances_plot.pdf", plot = EPS_slope_appliances_plot, width = 10, height = 6)
ggsave("./output/output_m4/EPS_slope_insulation_plot.pdf", plot = EPS_slope_insulation_plot, width = 11, height = 6)
ggsave("./output/output_m4/EPS_slope_solare_plot.pdf", plot = EPS_slope_solare_plot, width = 10, height = 6)
ggsave("./output/output_m4/EPS_slope_heatpumps_plot.pdf", plot = EPS_slope_heatpumps_plot, width = 10, height = 6)

ggsave("./output/output_m4/Fixed_effects_windows_plot.pdf", plot = Fixed_effects_windows_plot, width = 9, height = 6)
ggsave("./output/output_m4/Fixed_effects_appliances_plot.pdf", plot = Fixed_effects_appliances_plot, width = 9, height = 6)
ggsave("./output/output_m4/Fixed_effects_insulation_plot.pdf", plot = Fixed_effects_insulation_plot, width = 9, height = 6)
ggsave("./output/output_m4/Fixed_effects_solare_plot.pdf", plot = Fixed_effects_solare_plot, width = 9, height = 6)
ggsave("./output/output_m4/Fixed_effects_heatpumps_plot.pdf", plot = Fixed_effects_heatpumps_plot, width = 9, height = 6)


























