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

#-------------------------------------------------------------------------------
#------------------------- 3. BHM evaluation -----------------------------------
#-------------------------------------------------------------------------------
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

#Prior summary -----------------------------------------------------------------
windows_m4_prior_summary <- prior_summary(windows_m4) #reg. = 3
appliances_m4_prior_summary <- prior_summary(appliances_m4) #reg. = 3
insulation_m4_prior_summary <- prior_summary(insulation_m4) #reg. = 3
solare_m4_prior_summary <- prior_summary(solare_m4) #reg. = 3
heatpumps_m4_prior_summary <- prior_summary(heatpumps_m4) #decov(reg. = 5, conc. = 1, shape = 1, scale = 1)

#MCMC diagnostics --------------------------------------------------------------
##Effective sample size ratio
neff_fixed_list <- list()
neff_random_list <- list()

for (model_name in names(models)) {
  neff <- neff_ratio(models[[model_name]])
  
  # Separate based on parameter names
  neff_fixed <- neff[names(neff) %in% m4_fixed_pars]
  neff_random <- neff[names(neff) %in% m4_random_pars]
  
  # Fixed effects summary
  neff_fixed_list[[model_name]] <- data.frame(
    Model = model_name,
    Type = "Fixed",
    Min = round(min(neff_fixed), 2),
    Q1 = round(quantile(neff_fixed, 0.25), 2),
    Median = round(median(neff_fixed), 2),
    Mean = round(mean(neff_fixed), 2),
    Q3 = round(quantile(neff_fixed, 0.75), 2),
    Max = round(max(neff_fixed), 2),
    stringsAsFactors = FALSE
  )
  
  # Random effects summary
  neff_random_list[[model_name]] <- data.frame(
    Model = model_name,
    Type = "Random",
    Min = round(min(neff_random), 2),
    Q1 = round(quantile(neff_random, 0.25), 2),
    Median = round(median(neff_random), 2),
    Mean = round(mean(neff_random), 2),
    Q3 = round(quantile(neff_random, 0.75), 2),
    Max = round(max(neff_random), 2),
    stringsAsFactors = FALSE
  )
}

neff_summary_df <- do.call(rbind, c(neff_fixed_list, neff_random_list))
rownames(neff_summary_df) <- NULL

##Rhat statistics
rhat_fixed_list <- list()
rhat_random_list <- list()

for (model_name in names(models)) {
  rhat_vals <- rhat(models[[model_name]])
  
  # Separate based on parameter type
  rhat_fixed <- rhat_vals[names(rhat_vals) %in% m4_fixed_pars]
  rhat_random <- rhat_vals[names(rhat_vals) %in% m4_random_pars]
  
  # Fixed effects summary
  rhat_fixed_list[[model_name]] <- data.frame(
    Model = model_name,
    Type = "Fixed",
    Min = sprintf("%.4f", min(rhat_fixed)),
    Q1 = sprintf("%.4f", quantile(rhat_fixed, 0.25)),
    Median = sprintf("%.4f", median(rhat_fixed)),
    Mean = sprintf("%.4f", mean(rhat_fixed)),
    Q3 = sprintf("%.4f", quantile(rhat_fixed, 0.75)),
    Max = sprintf("%.4f", max(rhat_fixed)),
    stringsAsFactors = FALSE
  )
  
  # Random effects summary
  rhat_random_list[[model_name]] <- data.frame(
    Model = model_name,
    Type = "Random",
    Min = sprintf("%.4f", min(rhat_random)),
    Q1 = sprintf("%.4f", quantile(rhat_random, 0.25)),
    Median = sprintf("%.4f", median(rhat_random)),
    Mean = sprintf("%.4f", mean(rhat_random)),
    Q3 = sprintf("%.4f", quantile(rhat_random, 0.75)),
    Max = sprintf("%.4f", max(rhat_random)),
    stringsAsFactors = FALSE
  )
}

rhat_summary_df <- do.call(rbind, c(rhat_fixed_list, rhat_random_list))
rownames(rhat_summary_df) <- NULL

##Posterior predictive checks - posterior predictive p-value
#function for p-value extraction
pppval_list <- lapply(names(models), function(name) {
  model <- models[[name]]
  data <- models_data[[name]]
  
  Adoption <- data$Adoption
  Adoption_rep <- posterior_predict(model, draws = 1000)
  
  pppval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
  
  data.frame(
    Model = name,
    pppval = pppval
  )
})

# Combine into one data frame
ppc_pval_df <- do.call(rbind, pppval_list)

#Posterior classification ------------------------------------------------------
set.seed(84735)
windows_m4_postclass <- classification_summary(model = windows_m4, data = windows, cutoff = 0.5)
appliances_m4_postclass <- classification_summary(model = appliances_m4, data = appliances, cutoff = 0.5)
insulation_m4_postclass <- classification_summary(model = insulation_m4, data = insulation, cutoff = 0.5)
solare_m4_postclass <- classification_summary(model = solare_m4, data = solare, cutoff = 0.5)
heatpumps_m4_postclass <- classification_summary(model = heatpumps_m4, data = heatpumps, cutoff = 0.5)

accuracy_list <- list(
  "Windows m4" = windows_m4_postclass$accuracy_rates,
  "Appliances m4" = appliances_m4_postclass$accuracy_rates,
  "Thermal Insulation m4" = insulation_m4_postclass$accuracy_rates,
  "Solar panels m4" = solare_m4_postclass$accuracy_rates,
  "Heat pumps m4" = heatpumps_m4_postclass$accuracy_rates
)

models_postclass_accuracy <- bind_rows(lapply(names(accuracy_list), function(model) {
  data.frame(
    Metric = rownames(accuracy_list[[model]]),
    Rate = sprintf("%.3f", unlist(accuracy_list[[model]])),
    Model = model,
    row.names = NULL
  )
}))

# Pivot to wide format
postclass_accuracy_df <- models_postclass_accuracy %>%
  pivot_wider(names_from = Model, values_from = Rate) %>%
  select(Metric, "Windows m4", "Appliances m4", "Thermal Insulation m4", 
         "Solar panels m4", "Heat pumps m4")

#-------------------------------------------------------------------------------
#--------------------------- 5. BHM results ------------------------------------
#-------------------------------------------------------------------------------
#-------------------------- Posterior analysis ---------------------------------
tidy_rounded <- function(model, effect) {
  tidy(model, effects = effect, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

get_probabilities <- function(stan_summary, variables) {
  mat <- as.matrix(stan_summary)
  
  results <- lapply(variables, function(var) {
    m <- mat[var, "mean"]
    s <- mat[var, "sd"]
    prob_lt_0 <- pnorm(0, mean = m, sd = s)
    prob_gt_0 <- pnorm(0, mean = m, sd = s, lower.tail = FALSE)
    
    data.frame(
      Variable = var,
      Mean = m,
      SD = s,
      Prob_LT_0 = prob_lt_0,
      Prob_GT_0 = prob_gt_0,
      Prob_Direction = ifelse(prob_gt_0 > 0.95 | prob_lt_0 > 0.95, TRUE, FALSE)
    )
  })
  
  do.call(rbind, results)
}

#Output tables of estimates ----------------------------------------------------
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
ran_pars_df

ran_pars_df <- ran_pars_df %>%
  mutate(Parameters = ifelse(!is.na(ci_labels[Parameters]), ci_labels[Parameters], Parameters))

#Probability estimate is non-zero ----------------------------------------------
#fixed effects
prob_fixed_windows <- get_probabilities(windows_m4$stan_summary, m4_fixed_pars)
prob_fixed_appliances <- get_probabilities(appliances_m4$stan_summary, m4_fixed_pars)
prob_fixed_insulation <- get_probabilities(insulation_m4$stan_summary, m4_fixed_pars)
prob_fixed_solare <- get_probabilities(solare_m4$stan_summary, m4_fixed_pars)
prob_fixed_heatpumps <- get_probabilities(heatpumps_m4$stan_summary, m4_fixed_pars)

prob_fixed_windows[, sapply(prob_fixed_windows, is.numeric)] <- round(prob_fixed_windows[, sapply(prob_fixed_windows, is.numeric)], 2)
prob_fixed_appliances[, sapply(prob_fixed_appliances, is.numeric)] <- round(prob_fixed_appliances[, sapply(prob_fixed_appliances, is.numeric)], 2)
prob_fixed_insulation[, sapply(prob_fixed_insulation, is.numeric)] <- round(prob_fixed_insulation[, sapply(prob_fixed_insulation, is.numeric)], 2)
prob_fixed_solare[, sapply(prob_fixed_solare, is.numeric)] <- round(prob_fixed_solare[, sapply(prob_fixed_solare, is.numeric)], 2)
prob_fixed_heatpumps[, sapply(prob_fixed_heatpumps, is.numeric)] <- round(prob_fixed_heatpumps[, sapply(prob_fixed_heatpumps, is.numeric)], 2)

prob_models <- list(
  windows = prob_fixed_windows,
  appliances = prob_fixed_appliances,
  insulation = prob_fixed_insulation,
  solare = prob_fixed_solare,
  heatpumps = prob_fixed_heatpumps
)

prob_models <- imap(prob_models, function(df, name) {
  df %>%
    rename_with(~ paste0(.x, "_", name), .cols = -Variable)
})

prob_fixed_df <- reduce(prob_models, full_join, by = "Variable")

prob_fixed_df <- prob_fixed_df %>%
  mutate(Variable = recode(Variable, !!!fixed_effects_labels))

#random effects
prob_random_windows <- get_probabilities(windows_m4$stan_summary, m4_random_pars)
prob_random_appliances <- get_probabilities(appliances_m4$stan_summary, m4_random_pars)
prob_random_insulation <- get_probabilities(insulation_m4$stan_summary, m4_random_pars)
prob_random_solare <- get_probabilities(solare_m4$stan_summary, m4_random_pars)
prob_random_heatpumps <- get_probabilities(heatpumps_m4$stan_summary, m4_random_pars)

prob_random_windows[, sapply(prob_random_windows, is.numeric)] <- round(prob_random_windows[, sapply(prob_random_windows, is.numeric)], 2)
prob_random_appliances[, sapply(prob_random_appliances, is.numeric)] <- round(prob_random_appliances[, sapply(prob_random_appliances, is.numeric)], 2)
prob_random_insulation[, sapply(prob_random_insulation, is.numeric)] <- round(prob_random_insulation[, sapply(prob_random_insulation, is.numeric)], 2)
prob_random_solare[, sapply(prob_random_solare, is.numeric)] <- round(prob_random_solare[, sapply(prob_random_solare, is.numeric)], 2)
prob_random_heatpumps[, sapply(prob_random_heatpumps, is.numeric)] <- round(prob_random_heatpumps[, sapply(prob_random_heatpumps, is.numeric)], 2)

prob_models <- list(
  windows = prob_random_windows,
  appliances = prob_random_appliances,
  insulation = prob_random_insulation,
  solare = prob_random_solare,
  heatpumps = prob_random_heatpumps
)

prob_models <- imap(prob_models, function(df, name) {
  df %>%
    rename_with(~ paste0(.x, "_", name), .cols = -Variable)
})

prob_random_df <- reduce(prob_models, full_join, by = "Variable")

prob_random_df <- prob_random_df %>%
  mutate(Variable = recode(Variable, !!!random_effect_labels))

#-------------------------------------------------------------------------------
#------------------------------- Output ----------------------------------------
#-------------------------------------------------------------------------------
prob_fixed_df2 <- prob_fixed_df %>%
  select(Variable, Prob_LT_0_windows, Prob_GT_0_windows, Prob_LT_0_appliances,
         Prob_GT_0_appliances, Prob_LT_0_insulation, Prob_GT_0_insulation,
         Prob_LT_0_solare, Prob_GT_0_solare, Prob_LT_0_heatpumps, Prob_GT_0_heatpumps)

prob_random_df2 <- prob_random_df %>%
  select(Variable, Prob_LT_0_windows, Prob_GT_0_windows, Prob_LT_0_appliances,
         Prob_GT_0_appliances, Prob_LT_0_insulation, Prob_GT_0_insulation,
         Prob_LT_0_solare, Prob_GT_0_solare, Prob_LT_0_heatpumps, Prob_GT_0_heatpumps) %>%
  filter(!Variable %in% c("Var(Intercept)", "Cov(EPS, Intercept)",
                          "Var(EPS)"))

#--------------------------- Output Tables --------------------------------------
# List of your data frames:
tables_list <- list(
  m4_neff_summary_df = neff_summary_df,
  m4_rhat_summary_df = rhat_summary_df,
  m4_ppc_pval_df = ppc_pval_df,
  m4_postclass_accuracy_df = postclass_accuracy_df,
  m4_fixed_effects_df = fixed_effects_df,
  m4_ran_effects_df = ran_effects_df,
  m4_ran_pars_df = ran_pars_df,
  m4_cintervals_df = cintervals_df,
  m4_prob_fixed_df2 = prob_fixed_df2,
  m4_prob_random_df2 = prob_random_df2
)

# Output directory:
output_dir <- "./output/tables_tex/m4_tables"

# Function to write each table as separate .tex file:
write_tables_to_tex <- function(tables, outdir) {
  walk2(tables, names(tables), ~ {
    tex_code <- .x %>%
      kbl(
        caption = paste("Dummy caption for", .y),
        format = "latex",
        booktabs = TRUE,
        digits = 2,
        linesep = ""
      ) %>%
      kable_styling(latex_options = c("striped"), font_size = 10) %>%
      as.character()
    
    file_path <- file.path(outdir, paste0(.y, ".tex"))
    writeLines(tex_code, con = file_path)
  })
}

# Run the function
write_tables_to_tex(tables_list, output_dir)

#--------------------------- Output Plots ---------------------------------------
custom_bin_colors <- c(
  "(0.00-0.20)" = "#d9d9d9",  
  "(0.21-0.40)" = "#abd9e9",
  "(0.41-0.60)" = "#74add1",
  "(0.61-0.80)" = "#4575b4",
  "(0.81-1.00)" = "#fdae61"
)

#Estimated effect of EPS slope on technology adoption by country ---------------------
technologies <- list(
  "Windows" = c("Prob_LT_0_windows", "Prob_GT_0_windows", "Mean_windows"),
  "Appliances" = c("Prob_LT_0_appliances", "Prob_GT_0_appliances", "Mean_appliances"),
  "Thermal Insulation" = c("Prob_LT_0_insulation", "Prob_GT_0_insulation", "Mean_insulation"),
  "Solar Panels" = c("Prob_LT_0_solare", "Prob_GT_0_solare", "Mean_solare"),
  "Heat Pumps" = c("Prob_LT_0_heatpumps", "Prob_GT_0_heatpumps", "Mean_heatpumps")
)

eps_slope_effects_base <- prob_random_df %>%
  filter(str_detect(Variable, "^EPS effect \\(")) %>%
  mutate(Country = str_extract(Variable, "(?<=\\().+(?=\\))"))

plot_eps_slope_by_tech <- function(cols, tech_name) {
  lt_col <- cols[1]
  gt_col <- cols[2]
  mean_col <- cols[3]
  
  df <- eps_slope_effects_base %>%
    mutate(
      abs_effect_prob = pmax(.data[[lt_col]], .data[[gt_col]]),
      effect = .data[[mean_col]],
      prob_bin = cut(
        abs_effect_prob,
        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
        labels = names(custom_bin_colors),
        include.lowest = TRUE
      )
    )
  
  # Compute y-axis limits with forced inclusion of zero
  y_min <- min(df$effect, na.rm = TRUE)
  y_max <- max(df$effect, na.rm = TRUE)
  y_margin <- 0.05 * (y_max - y_min)
  y_min <- min(y_min, 0)
  y_max <- max(y_max, 0)
  
  ggplot(df, aes(x = fct_reorder(Country, effect), y = effect, color = prob_bin)) +
    geom_segment(aes(xend = fct_reorder(Country, effect), y = 0, yend = effect), color = "grey30", linetype = "dashed") +
    geom_point(size = 5) +
    geom_hline(yintercept = 0, color = "grey30", linewidth = 0.7) +
    scale_y_continuous(
      limits = c(y_min - y_margin, y_max + y_margin),
      breaks = pretty(c(y_min, y_max), n = 6)
    ) +
    scale_color_manual(values = custom_bin_colors, name = expression("Probability |effect|" != 0)) +
    labs(
      title = paste("Estimated Effect of EPS slope on", tech_name, "Adoption by Country"),
      x = "Country",
      y = "Mean Estimated Effect"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
      legend.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10)
    )
}

# Generate and store plots
eps_slope_plots <- imap(technologies, plot_eps_slope_by_tech)

#Estimated effect of fixed-effect predictors across technology ---------------------
fixed_effects_base <- prob_fixed_df %>%
  filter(Variable %in% c("Age: 45–54", "Age: 55+", "Higher education", "Income q4",
                         "Income q5", "Environmental concern", "Homeowner",
                         "D-Type: House", "EPS × Income q2", "EPS × Income q3",
                         "EPS × Income q4", "EPS × Income q5"))

plot_fixed_by_tech <- function(cols, tech_name) {
  lt_col <- cols[1]
  gt_col <- cols[2]
  mean_col <- cols[3]
  
  df <- fixed_effects_base %>%
    mutate(
      abs_effect_prob = pmax(.data[[lt_col]], .data[[gt_col]]),
      effect = .data[[mean_col]],
      prob_bin = cut(
        abs_effect_prob,
        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
        labels = names(custom_bin_colors),
        include.lowest = TRUE
      )
    )
  
  y_min <- min(df$effect, na.rm = TRUE)
  y_max <- max(df$effect, na.rm = TRUE)
  y_margin <- 0.05 * (y_max - y_min)
  
  ggplot(df, aes(x = fct_reorder(Variable, effect), y = effect, color = prob_bin)) +
    # Vertical lines from 0 to each point
    geom_segment(aes(xend = fct_reorder(Variable, effect), y = 0, yend = effect), color = "grey30", linetype = "dashed") +
    # Points
    geom_point(size = 5) +
    # Horizontal zero line
    geom_hline(yintercept = 0, color = "grey30", linewidth = 0.7) +
    # Y-axis settings
    scale_y_continuous(
      limits = c(y_min - y_margin, y_max + y_margin),
      breaks = pretty(c(y_min, y_max), n = 6)
    ) +
    scale_color_manual(values = custom_bin_colors, name = expression("Probability |effect|" != 0)) +
    labs(
      title = paste("Estimated Effect of Predictors on", tech_name, "Adoption"),
      x = "Predictor",
      y = "Mean Estimated Effect"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
      legend.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10)
    )
}

# Generate and store plots
fixed_effects_plots <- imap(technologies, plot_fixed_by_tech)

#Estimated effect of Gov_support and EPS across technology ---------------------
# General function to plot fixed effects (EPS or Gov_support)
plot_fixed_effect_across_tech <- function(effect_var, df, custom_colors) {
  plot_df <- df %>%
    filter(Variable == effect_var) %>%
    pivot_longer(
      cols = matches("^(Mean|Prob_LT_0|Prob_GT_0)_"),
      names_to = c(".value", "Technology"),
      names_pattern = "^(Mean|Prob_LT_0|Prob_GT_0)_(.+)$"
    ) %>%
    mutate(
      Technology = recode(Technology,
                          "appliances" = "Appliances",
                          "windows" = "Windows",
                          "heatpumps" = "Heat pumps",
                          "solare" = "Solar panels",
                          "insulation" = "Thermal insulation"
      ),
      abs_effect_prob = pmax(Prob_LT_0, Prob_GT_0),
      prob_bin = cut(
        abs_effect_prob,
        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
        labels = names(custom_colors),
        include.lowest = TRUE
      )
    )
  
  y_min <- min(plot_df$Mean, na.rm = TRUE)
  y_max <- max(plot_df$Mean, na.rm = TRUE)
  y_margin <- 0.05 * (y_max - y_min)
  
  # Force y-axis to include 0
  y_min <- min(y_min, 0)
  y_max <- max(y_max, 0)
  
  ggplot(plot_df, aes(x = fct_reorder(Technology, Mean), y = Mean, color = prob_bin)) +
    geom_segment(aes(xend = fct_reorder(Technology, Mean), y = 0, yend = Mean), color = "grey30", linetype = "dashed") +
    geom_point(size = 5) +
    geom_hline(yintercept = 0, color = "grey30", linewidth = 0.7) +
    scale_y_continuous(
      limits = c(y_min - y_margin, y_max + y_margin),
      breaks = pretty(c(y_min, y_max), n = 6)
    ) +
    scale_color_manual(values = custom_colors, name = expression("Probability |effect|" != 0)) +
    labs(
      title = paste("Estimated Effect of", effect_var, "on Technology Adoption"),
      x = "Technology",
      y = "Mean Estimated Effect"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
      legend.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10)
    )
}

EPS_effects_plot <- plot_fixed_effect_across_tech("EPS index", prob_fixed_df, custom_bin_colors)
Govsupport_effects_plot <- plot_fixed_effect_across_tech("Government support", prob_fixed_df, custom_bin_colors)

EPS_slope_windows_plot <- eps_slope_plots$Windows
EPS_slope_appliances_plot <- eps_slope_plots$Appliances
EPS_slope_insulation_plot <- eps_slope_plots$`Thermal Insulation`
EPS_slope_solare_plot <- eps_slope_plots$`Solar Panels`
EPS_slope_heatpumps_plot <- eps_slope_plots$`Heat Pumps`

Fixed_effects_windows_plot <- fixed_effects_plots$Windows
Fixed_effects_appliances_plot <- fixed_effects_plots$Appliances
Fixed_effects_insulation_plot <- fixed_effects_plots$`Thermal Insulation`
Fixed_effects_solare_plot <- fixed_effects_plots$`Solar Panels`
Fixed_effects_heatpumps_plot <- fixed_effects_plots$`Heat Pumps`

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


























