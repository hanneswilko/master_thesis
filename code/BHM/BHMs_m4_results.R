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
##effective sample size ratio
###function for neff extraction
neff_df_list <- lapply(names(models), function(name) {
  neff <- neff_ratio(models[[name]])
  data.frame(
    Parameter = names(neff),
    Neff = neff,
    Model = name,
    stringsAsFactors = FALSE
  )
})

neff_summary <- do.call(rbind, neff_df_list)
neff_summary_df <- neff_summary %>%
  group_by(Model) %>%
  summarize(
    Min    = round(min(Neff), 2),
    Q1     = round(quantile(Neff, 0.25), 2),
    Median = round(median(Neff), 2),
    Mean   = round(mean(Neff), 2),
    Q3     = round(quantile(Neff, 0.75), 2),
    Max    = round(max(Neff), 2),
    .groups = "drop"
  )

##Rhat statistics
###function for rhat extraction
rhat_df_list <- lapply(names(models), function(name) {
  rhat <- rhat(models[[name]])
  data.frame(
    Parameter = names(rhat),
    Rhat = rhat,
    Model = name,
    stringsAsFactors = FALSE
  )
})

rhat_summary <- do.call(rbind, rhat_df_list)
rhat_summary_df <- rhat_summary %>%
  group_by(Model) %>%
  summarize(
    Min    = sprintf("%.4f", min(Rhat)),
    Q1     = sprintf("%.4f", quantile(Rhat, 0.25)),
    Median = sprintf("%.4f", median(Rhat)),
    Mean   = sprintf("%.4f", mean(Rhat)),
    Q3     = sprintf("%.4f", quantile(Rhat, 0.75)),
    Max    = sprintf("%.4f", max(Rhat)),
    .groups = "drop"
  )

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

ran_effects_df <- {
  ran_vals_wide_clean <- ran_effects %>%
    select(term, matches("^(estimate|std\\.error)_"))
  
  tech_names <- ran_vals_wide_clean %>%
    select(-term) %>%
    names() %>%
    str_remove("^(estimate_|std\\.error_)") %>%
    unique()
  
  ordered_cols <- unlist(lapply(tech_names, function(tech) {
    c(paste0("estimate_", tech), paste0("std.error_", tech))
  }))
  
  ran_vals_wide_clean %>%
    select(term, all_of(ordered_cols))
}

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

#random effects
prob_random_windows <- get_probabilities(windows_m4$stan_summary, m4_random_pars)
prob_random_appliances <- get_probabilities(appliances_m4$stan_summary, m4_random_pars)
prob_random_insulation <- get_probabilities(insulation_m4$stan_summary, m4_random_pars)
prob_random_solare <- get_probabilities(solare_m4$stan_summary, m4_random_pars)
prob_random_heatpumps <- get_probabilities(heatpumps_m4$stan_summary, m4_random_pars)

# Round only numeric columns
prob_random_windows[, sapply(prob_random_windows, is.numeric)] <- round(prob_random_windows[, sapply(prob_random_windows, is.numeric)], 2)
prob_random_appliances[, sapply(prob_random_appliances, is.numeric)] <- round(prob_random_appliances[, sapply(prob_random_appliances, is.numeric)], 2)
prob_random_insulation[, sapply(prob_random_insulation, is.numeric)] <- round(prob_random_insulation[, sapply(prob_random_insulation, is.numeric)], 2)
prob_random_solare[, sapply(prob_random_solare, is.numeric)] <- round(prob_random_solare[, sapply(prob_random_solare, is.numeric)], 2)
prob_random_heatpumps[, sapply(prob_random_heatpumps, is.numeric)] <- round(prob_random_heatpumps[, sapply(prob_random_heatpumps, is.numeric)], 2)

#combine data sets
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

# Now reduce into a single data frame by joining on 'Variable'
prob_random_df <- reduce(prob_models, full_join, by = "Variable")

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
  filter(!Variable %in% c("Sigma[Country_name:(Intercept),(Intercept)]", "Sigma[Country_name:EPS,(Intercept)]",
                          "Sigma[Country_name:EPS,EPS]"))

#--------------------------- Output Tables --------------------------------------
# List of your data frames:
tables_list <- list(
  neff_summary_df = neff_summary_df,
  rhat_summary_df = rhat_summary_df,
  ppc_pval_df = ppc_pval_df,
  postclass_accuracy_df = postclass_accuracy_df,
  fixed_effects_df = fixed_effects_df,
  ran_effects_df = ran_effects_df,
  ran_pars_df = ran_pars_df,
  cintervals_df = cintervals_df,
  prob_fixed_df2 = prob_fixed_df2,
  prob_random_df2 = prob_random_df2
)

# Output directory:
output_dir <- "./output/tables_tex/heatpumps_tables"

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
# Step 1: Filter rows for EPS by country
eps_effects <- prob_random_df %>%
  filter(str_detect(Variable, "b\\[EPS Country_name:")) %>%
  mutate(
    Country = str_extract(Variable, "(?<=Country_name:)[A-Z]+"),
    abs_effect_prob = pmax(Prob_LT_0_windows, Prob_GT_0_windows),  # WINDOWS
    effect = Mean_windows  # choose the effect column per technology
  )

# Step 2: Bin probabilities
eps_effects <- eps_effects %>%
  mutate(
    prob_bin = cut(abs_effect_prob,
                   breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 1),
                   labels = c("(.00–.10)", "(.11–.20)", "(.21–.30)", "(.31–.40)",
                              "(.41–.50)", "(.51–.70)", "(.71–1.00)"),
                   include.lowest = TRUE)
  )


# Step 3: Create the plot
library(ggplot2)
library(dplyr)
library(forcats)

# Updated color palette — high contrast
custom_bin_colors <- c(
  "(.00–.10)" = "#e0e0e0",  # light gray
  "(.11–.20)" = "#fdae61",  # orange
  "(.21–.30)" = "#abd9e9",  # light cyan
  "(.31–.40)" = "#74add1",  # light blue
  "(.41–.50)" = "#4575b4",  # medium blue
  "(.51–.70)" = "#313695",  # deep blue
  "(.71–1.00)" = "#542788"  # purple
)

# Create the updated plot
ggplot(eps_effects, aes(x = fct_reorder(Country, effect), y = effect, color = prob_bin)) +
  geom_point(size = 5) +
  scale_color_manual(
    values = custom_bin_colors,
    name = "Probability |effect| ≠ 0"
  ) +
  labs(
    title = "Estimated Effect of EPS on Adoption by Country",
    x = "Country",
    y = "Mean Estimated Effect"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10)
  )





























