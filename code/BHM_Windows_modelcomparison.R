#-------------------------------------------------------------------------------
#--------------- Bayesian hierarchical model - Windows -------------------------
#-------------------------------------------------------------------------------
# 1. Loading Packages
# 2. Loading Data
# 3. BHM evaluation
# 4. BHM comparison
# 5. BHM results

#-------------------------------------------------------------------------------
#------------------------ 1. Loading Packages ----------------------------------
#-------------------------------------------------------------------------------

#install.packages(c("bayesrules", "tidyverse", "janitor", "rstanarm",
#                   "bayesplot", "tidybayes", "broom.mixed", "modelr",
#                   "e1071", "forcats"), 
#                 dependencies = TRUE)

pacman::p_load("bayesrules", "tidyverse", "janitor", "rstanarm",
               "bayesplot", "tidybayes", "broom.mixed", "modelr",
               "e1071", "forcats", "dplyr", "ggplot2", "loo", "readr",
               "knitr", "kableExtra", "purrr")

#-------------------------------------------------------------------------------
#-------------------------- 2. Loading Data ------------------------------------
#-------------------------------------------------------------------------------
#data windows
windows <- read.csv("./processed_data/windows.csv")

#BHM windows models
m1 <- read_rds("./output/models_rds/fitWindows_m1.rds") # varying intercept
m2 <- read_rds("./output/models_rds/fitWindows_m2.rds") # varying intercept + group level predictor
m3 <- read_rds("./output/models_rds/fitWindows_m3.rds") # varying intercept & slope (1+EPS|country)
m3.1 <- read_rds("./output/models_rds/fitWindows_m3.1.rds") # varying intercept & slope (EPS|country)
m4 <- read_rds("./output/models_rds/fitWindows_m4.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Income
m4.1 <- read_rds("./output/models_rds/fitWindows_m4.1.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Gov_support

#-------------------------------------------------------------------------------
#------------------------- 3. BHM evaluation -----------------------------------
#-------------------------------------------------------------------------------

#------------------------------ Model 1 ----------------------------------------
# varying intercept (random effects)
names(m1)

#priors ------------------------------------------------------------------------
m1_prior_summary <- prior_summary(m1)
m1$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m1_array <- as.array(m1)
m1_all_pars <- dimnames(m1_array)$parameters

m1_fixed_pars <- m1_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m1_all_pars)]
m1_random_pars <- m1_all_pars[grepl("^(b\\[|Sigma|cor_)", m1_all_pars)]

#Diagnostic Plots 
m1_mcmc_trace <- mcmc_trace(m1)

m1_mcmc_dens_overlay <- mcmc_dens_overlay(m1)

# ACF for fixed effects
m1_mcmc_acf_fixed <- mcmc_acf_bar(m1_array, pars = m1_fixed_pars, lags = 10)
# ACF for random effects
m1_mcmc_acf_random <- mcmc_acf_bar(m1_array, pars = m1_random_pars, lags = 10)

#Summary
m1_neff <- neff_ratio(m1)
m1_rhat <- rhat(m1)
summary(m1)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m1,draws=1000)
m1_ppc <- ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
m1_pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))

#------------------------------ Model 2 ----------------------------------------
# varying intercept (random effects)
names(m2)

#priors ------------------------------------------------------------------------
m2_prior_summary <- prior_summary(m2)
m2$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m2_array <- as.array(m2)
m2_all_pars <- dimnames(m2_array)$parameters

m2_fixed_pars <- m2_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m2_all_pars)]
m2_random_pars <- m2_all_pars[grepl("^(b\\[|Sigma|cor_)", m2_all_pars)]

#Diagnostic Plots 
m2_mcmc_trace <- mcmc_trace(m2) +
  ggtitle("MCMC trace plots for model m2") + 
  theme(
    strip.text = element_text(size = 6), # Adjust facet label text size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)         
  )

m2_mcmc_dens_overlay <- mcmc_dens_overlay(m2) +
  ggtitle("MCMC density plots for model m2") + 
  theme(
    strip.text = element_text(size = 6), # Adjust facet label text size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)         
  )

# ACF for fixed effects
m2_mcmc_acf_fixed <- mcmc_acf_bar(m2_array, pars = m2_fixed_pars, lags = 10) +
  ggtitle("Autocorrelation plots for model m2 - fixed effects") +
  theme(
    strip.text = element_text(size = 5),            
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 6),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  )
# ACF for random effects
m2_mcmc_acf_random <- mcmc_acf_bar(m2_array, pars = m2_random_pars, lags = 10) +
  ggtitle("Autocorrelation plots for model m2 - random effects") +
  theme(
    strip.text = element_text(size = 5),            
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 6),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  )

#Summary
m2_neff <- neff_ratio(m2)
m2_rhat <- rhat(m2)
summary(m2)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m2,draws=1000)
m2_ppc <- ppc_stat(Adoption, Adoption_rep, stat = "mean") +
  ggtitle("Posterior predictive check for model m2") + 
  labs(x = "Adoption rate") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)   
  )

#Bayesian p-value
m2_pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))

#------------------------------ Model 3 ----------------------------------------
# varying intercept (random effects)
names(m3)

#priors ------------------------------------------------------------------------
m3_prior_summary <- prior_summary(m3)
m3$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m3_array <- as.array(m3)
m3_all_pars <- dimnames(m3_array)$parameters

m3_fixed_pars <- m3_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m3_all_pars)]
m3_random_pars <- m3_all_pars[grepl("^(b\\[|Sigma|cor_)", m3_all_pars)]

#Diagnostic Plots 
m3_mcmc_trace <- mcmc_trace(m3)

m3_mcmc_dens_overlay <- mcmc_dens_overlay(m3)

# ACF for fixed effects
m3_mcmc_acf_fixed <- mcmc_acf_bar(m3_array, pars = m3_fixed_pars, lags = 10)
# ACF for random effects
m3_mcmc_acf_random <- mcmc_acf_bar(m3_array, pars = m3_random_pars, lags = 10)

#Summary
m3_neff <- neff_ratio(m3)
m3_rhat <- rhat(m3)
summary(m3)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m3,draws=1000)
m3_ppc <- ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
m3_pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))

#------------------------------ Model 3.1 --------------------------------------
# varying intercept (random effects)
names(m3.1)

#priors ------------------------------------------------------------------------
m3.1_prior_summary <- prior_summary(m3.1)
m3.1$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m3.1_array <- as.array(m3.1)
m3.1_all_pars <- dimnames(m3.1_array)$parameters

m3.1_fixed_pars <- m3.1_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m3.1_all_pars)]
m3.1_random_pars <- m3.1_all_pars[grepl("^(b\\[|Sigma|cor_)", m3.1_all_pars)]

#Diagnostic Plots 
m3.1_mcmc_trace <- mcmc_trace(m3.1) +
  ggtitle("MCMC trace plots for model m3.1") + 
  theme(
    strip.text = element_text(size = 6), # Adjust facet label text size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)         
  )

m3.1_mcmc_dens_overlay <- mcmc_dens_overlay(m3.1)+
  ggtitle("MCMC trace plots for model m3.1") + 
  theme(
    strip.text = element_text(size = 6), # Adjust facet label text size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)         
  )

# ACF for fixed effects
m3.1_mcmc_acf_fixed <- mcmc_acf_bar(m3.1_array, pars = m3.1_fixed_pars, lags = 10) +
  ggtitle("Autocorrelation plots for model m3.1 - fixed effects") +
  theme(
    strip.text = element_text(size = 5),            
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 6),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  )
# ACF for random effects
m3.1_mcmc_acf_random <- mcmc_acf_bar(m3.1_array, pars = m3.1_random_pars, lags = 10) +
  ggtitle("Autocorrelation plots for model m3.1 - random effects") +
  theme(
    strip.text = element_text(size = 5),            
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 6),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  )

#Summary
m3.1_neff <- neff_ratio(m3.1)
m3.1_rhat <- rhat(m3.1)
summary(m3.1)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m3.1,draws=1000)
m3.1_ppc <- ppc_stat(Adoption, Adoption_rep, stat = "mean") +
  ggtitle("Posterior predictive check for model m3.1") + 
  labs(x = "Adoption rate") +
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)   
  )

#Bayesian p-value
m3.1_pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))

#-------------------------------- Model 4 --------------------------------------
# varying intercept (random effects)
names(m4)

#priors ------------------------------------------------------------------------
m4_prior_summary <- prior_summary(m4)
m4$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m4_array <- as.array(m4)
m4_all_pars <- dimnames(m4_array)$parameters

m4_fixed_pars <- m4_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m4_all_pars)]
m4_random_pars <- m4_all_pars[grepl("^(b\\[|Sigma|cor_)", m4_all_pars)]

#Diagnostic Plots 
m4_mcmc_trace <- mcmc_trace(m4) +
  ggtitle("MCMC trace plots for model m4") + 
  theme(
    strip.text = element_text(size = 6), # Adjust facet label text size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)         
  )

m4_mcmc_dens_overlay <- mcmc_dens_overlay(m4) +
  ggtitle("MCMC density plots for model m4") + 
  theme(
    strip.text = element_text(size = 6), # Adjust facet label text size
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)         
  )

# ACF for fixed effects
m4_mcmc_acf_fixed <- mcmc_acf_bar(m4_array, pars = m4_fixed_pars, lags = 10) +
  ggtitle("Autocorrelation plots for model m4 - fixed effects") +
  theme(
    strip.text = element_text(size = 5),            
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 6),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  )

# ACF for random effects
m4_mcmc_acf_random <- mcmc_acf_bar(m4_array, pars = m4_random_pars, lags = 10) +
  ggtitle("Autocorrelation plots for model m4 - random effects") +
  theme(
    strip.text = element_text(size = 5),            
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 6),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.spacing = unit(0.2, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  )

#Summary
m4_neff <- neff_ratio(m4)
m4_rhat <- rhat(m4)
summary(m4)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m4,draws=1000)
m4_ppc <- ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
m4_pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))

#------------------------------ Model 4.1 --------------------------------------
# varying intercept (random effects)
names(m4.1)

#priors ------------------------------------------------------------------------
m4.1_prior_summary <- prior_summary(m4.1)
m4.1$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m4.1_array <- as.array(m4)
m4.1_all_pars <- dimnames(m4_array)$parameters

m4.1_fixed_pars <- m4.1_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m4.1_all_pars)]
m4.1_random_pars <- m4.1_all_pars[grepl("^(b\\[|Sigma|cor_)", m4.1_all_pars)]

#Diagnostic Plots 
m4.1_mcmc_trace <- mcmc_trace(m4.1)
m4.1_mcmc_dens_overlay <- mcmc_dens_overlay(m4.1)

# ACF for fixed effects
m4.1_mcmc_acf_fixed <- mcmc_acf_bar(m4.1_array, pars = m4.1_fixed_pars, lags = 10)
# ACF for random effects
m4.1_mcmc_acf_random <- mcmc_acf_bar(m4.1_array, pars = m4.1_random_pars, lags = 10)

#Summary
m4.1_neff <- neff_ratio(m4.1)
m4.1_rhat <- rhat(m4.1)
summary(m4.1)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m4.1,draws=1000)
m4.1_ppc <- ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
m4.1_pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))

#-------------------------------------------------------------------------------
#------------------------- 4. BHM comparison -----------------------------------
#-------------------------------------------------------------------------------

#----------------- Watanabe-Akaike Information Criterion (ELPD) ----------------
waic_m1 <- waic(m1)
waic_m2 <- waic(m2)
waic_m3 <- waic(m3)
waic_m3.1 <- waic(m3.1)
waic_m4 <- waic(m4)
waic_m4.1 <- waic(m4.1)

#All models
WAIC <- loo_compare(waic_m1, waic_m2, waic_m3, waic_m3.1, waic_m4, waic_m4.1)

WAIC_df <- as.data.frame(WAIC)
colnames(WAIC_df) <- c("elpd_diff", "se_diff", "elpd_waic", "se_elpd_waic", 
                       "p_waic", "se_p_waic", "waic", "se_waic")
WAIC_summary <- cbind(Model = rownames(WAIC_df), round(WAIC_df, 2))
rownames(WAIC_summary) <- NULL
print(WAIC_summary) #m3.1

#Varying intercept models
WAIC_vi <- loo_compare(waic_m1, waic_m2)

WAIC_vi_df <- as.data.frame(WAIC_vi)
colnames(WAIC_vi_df) <- c("elpd_diff", "se_diff", "elpd_waic", "se_elpd_waic", 
                       "p_waic", "se_p_waic", "waic", "se_waic")
WAIC_vi_summary <- cbind(Model = rownames(WAIC_vi_df), round(WAIC_vi_df, 2))
rownames(WAIC_vi_summary) <- NULL
print(WAIC_vi_summary) #m2

#Varying intercept & slopes models
WAIC_vis <- loo_compare(waic_m3, waic_m3.1)

WAIC_vis_df <- as.data.frame(WAIC_vis)
colnames(WAIC_vis_df) <- c("elpd_diff", "se_diff", "elpd_waic", "se_elpd_waic", 
                          "p_waic", "se_p_waic", "waic", "se_waic")
WAIC_vis_summary <- cbind(Model = rownames(WAIC_vis_df), round(WAIC_vis_df, 2))
rownames(WAIC_vis_summary) <- NULL
print(WAIC_vis_summary) #m3.1

#Varying intercept & slopes + interaction term models
WAIC_visi <- loo_compare(waic_m4, waic_m4.1)

WAIC_visi_df <- as.data.frame(WAIC_visi)
colnames(WAIC_visi_df) <- c("elpd_diff", "se_diff", "elpd_waic", "se_elpd_waic", 
                           "p_waic", "se_p_waic", "waic", "se_waic")
WAIC_visi_summary <- cbind(Model = rownames(WAIC_visi_df), round(WAIC_visi_df, 2))
rownames(WAIC_visi_summary) <- NULL
print(WAIC_visi_summary) #m4

#Cross model check
WAIC_vi_vis <- loo_compare(waic_m2, waic_m3.1)
WAIC_vis_visi <- loo_compare(waic_m3.1, waic_m4)
WAIC_visi_vi <- loo_compare(waic_m4, waic_m2)
WAIC_all <- loo_compare(waic_m2, waic_m3.1, waic_m4)

WAIC_all_df <- as.data.frame(WAIC_all)
colnames(WAIC_all_df) <- c("elpd_diff", "se_diff", "elpd_waic", "se_elpd_waic", 
                           "p_waic", "se_p_waic", "waic", "se_waic")
WAIC_all_summary <- cbind(Model = rownames(WAIC_all_df), round(WAIC_all_df, 2))
rownames(WAIC_all_summary) <- NULL
print(WAIC_all_summary)

#-------------------------------------------------------------------------------
#--------------------------- 5. BHM results ------------------------------------
#-------------------------------------------------------------------------------
##results for m2, m3.1, m4, m4.1

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

#model 2------------------------------------------------------------------------
##Output
m2_fixed <- tidy_rounded(m2, "fixed")
m2_ran_vals <- tidy_rounded(m2, "ran_vals")
m2_ran_pars <- tidy_rounded(m2, "ran_pars")
m2_ran_auxiliary <- tidy_rounded(m2, "auxiliary")
m2_CI <- as.data.frame(posterior_interval(m2, prob=0.95))
m2_CI <-  m2_CI %>%
  tibble::rownames_to_column(var = "Parameters") %>%
  select(Parameters, everything())

View(m2_ran_vals)

##Probability estimate is non-zero
variables_of_interest <- c("Age_cat25-34", "Age_cat55+", "Age_cat45-54", "Incomequintile 2",
                           "Incomequintile 4", "Incomequintile 5", "Higher_edu",
                           "Env_concern", "Gov_support", "EPS", "b[(Intercept) Country_name:US]",
                           "b[(Intercept) Country_name:IL]", "b[(Intercept) Country_name:BE]",
                           "b[(Intercept) Country_name:NL]", "b[(Intercept) Country_name:UK]",
                           "b[(Intercept) Country_name:CA]", "b[(Intercept) Country_name:CH]",
                           "b[(Intercept) Country_name:FR]", "b[(Intercept) Country_name:SE]")
fixed_random_df <- get_probabilities(m2$stan_summary, variables_of_interest)
# Round only numeric columns
fixed_random_df[, sapply(fixed_random_df, is.numeric)] <- round(fixed_random_df[, sapply(fixed_random_df, is.numeric)], 2)
m2_fixed_random <- fixed_random_df

ran_pars_df <- as.data.frame(m2_ran_pars)
colnames(ran_pars_df) <- c("Term", "Group", "Estimate")
ran_pars_df$Estimate <- round(ran_pars_df$Estimate, 2)
m2_ran_pars <- ran_pars_df

auxiliary_df <- as.data.frame(m2_ran_auxiliary)
colnames(auxiliary_df) <- c("Term", "Estimate", "Std_Error", "CI_Lower", "CI_Upper")
auxiliary_df[, sapply(auxiliary_df, is.numeric)] <- round(auxiliary_df[, sapply(auxiliary_df, is.numeric)], 2)
m2_auxiliary <- auxiliary_df

#model 3.1----------------------------------------------------------------------
##Output
m3.1_fixed <- tidy_rounded(m3.1, "fixed")
m3.1_ran_vals <- tidy_rounded(m3.1, "ran_vals")
m3.1_ran_pars <- tidy_rounded(m3.1, "ran_pars")
m3.1_ran_auxiliary <- tidy_rounded(m3.1, "auxiliary")
m3.1_CI <- as.data.frame(posterior_interval(m3.1, prob=0.95))
m3.1_CI <-  m3.1_CI %>%
  tibble::rownames_to_column(var = "Parameters") %>%
  select(Parameters, everything())

View(m3.1_ran_pars)

##Probability estimate is non-zero
variables_of_interest <- c("Age_cat25-34", "Age_cat55+", "Age_cat45-54", "Incomequintile 2",
                           "Incomequintile 4", "Incomequintile 5", "Higher_edu",
                           "Env_concern", "Gov_support", "EPS", "b[(Intercept) Country_name:US]",
                           "b[EPS Country_name:US]", "b[(Intercept) Country_name:IL]",
                           "b[EPS Country_name:IL]", "b[(Intercept) Country_name:BE]",
                           "b[EPS Country_name:BE]", "b[(Intercept) Country_name:NL]",
                           "b[EPS Country_name:NL]", "b[(Intercept) Country_name:UK]",
                           "b[EPS Country_name:UK]", "b[(Intercept) Country_name:CA]",
                           "b[EPS Country_name:CA]", "b[(Intercept) Country_name:SE]",
                           "b[EPS Country_name:SE]", "b[(Intercept) Country_name:CH]",
                           "b[EPS Country_name:CH]", "b[(Intercept) Country_name:FR]",
                           "b[EPS Country_name:FR]")
fixed_random_df <- get_probabilities(m3.1$stan_summary, variables_of_interest)
# Round only numeric columns
fixed_random_df[, sapply(fixed_random_df, is.numeric)] <- round(fixed_random_df[, sapply(fixed_random_df, is.numeric)], 2)
m3.1_fixed_random <- fixed_random_df

ran_pars_df <- as.data.frame(m3.1_ran_pars)
colnames(ran_pars_df) <- c("Term", "Group", "Estimate")
ran_pars_df$Estimate <- round(ran_pars_df$Estimate, 2)
m3.1_ran_pars <- ran_pars_df

auxiliary_df <- as.data.frame(m3.1_ran_auxiliary)
colnames(auxiliary_df) <- c("Term", "Estimate", "Std_Error", "CI_Lower", "CI_Upper")
auxiliary_df[, sapply(auxiliary_df, is.numeric)] <- round(auxiliary_df[, sapply(auxiliary_df, is.numeric)], 2)
m3.1_auxiliary <- auxiliary_df

#model 4------------------------------------------------------------------------
##Output
m4_fixed <- tidy_rounded(m4, "fixed")
m4_ran_vals <- tidy_rounded(m4, "ran_vals")
m4_ran_pars <- tidy_rounded(m4, "ran_pars")
m4_CI <- as.data.frame(posterior_interval(m4, prob=0.95))
m4_CI <- m4_CI %>%
  tibble::rownames_to_column(var = "Parameters") %>%
  select(Parameters, everything())

View(m4_ran_vals)

##Probability estimate is non-zero
variables_of_interest <- c("Age_cat25-34", "Age_cat55+", "Age_cat45-54", "Higher_edu",
                           "Env_concern", "Gov_support", "EPS", "Incomequintile 2",
                           "Incomequintile 4", "Incomequintile 5", "EPS:Incomequintile 2", 
                           "EPS:Incomequintile 4", "EPS:Incomequintile 5",
                           "b[(Intercept) Country_name:US]",
                           "b[EPS Country_name:US]", "b[(Intercept) Country_name:IL]",
                           "b[EPS Country_name:IL]", "b[(Intercept) Country_name:BE]",
                           "b[EPS Country_name:BE]", "b[(Intercept) Country_name:NL]",
                           "b[EPS Country_name:NL]", "b[(Intercept) Country_name:UK]",
                           "b[EPS Country_name:UK]", "b[(Intercept) Country_name:CA]",
                           "b[EPS Country_name:CA]", "b[(Intercept) Country_name:SE]",
                           "b[EPS Country_name:SE]", "b[(Intercept) Country_name:CH]",
                           "b[EPS Country_name:CH]", "b[(Intercept) Country_name:FR]",
                           "b[EPS Country_name:FR]")
fixed_random_df <- get_probabilities(m4$stan_summary, variables_of_interest)
# Round only numeric columns
fixed_random_df[, sapply(fixed_random_df, is.numeric)] <- round(fixed_random_df[, sapply(fixed_random_df, is.numeric)], 2)
m4_fixed_random <- fixed_random_df

ran_pars_df <- as.data.frame(m4_ran_pars)
colnames(ran_pars_df) <- c("Term", "Group", "Estimate")
ran_pars_df$Estimate <- round(ran_pars_df$Estimate, 2)
m4_ran_pars <- ran_pars_df

auxiliary_df <- as.data.frame(m4_ran_auxiliary)
colnames(auxiliary_df) <- c("Term", "Estimate", "Std_Error", "CI_Lower", "CI_Upper")
auxiliary_df[, sapply(auxiliary_df, is.numeric)] <- round(auxiliary_df[, sapply(auxiliary_df, is.numeric)], 2)
m4_auxiliary <- auxiliary_df

#model 4.1 ---------------------------------------------------------------------
##Output
m4.1_fixed <- tidy_rounded(m4.1, "fixed")
m4.1_ran_vals <- tidy_rounded(m4.1, "ran_vals")
m4.1_ran_pars <- tidy_rounded(m4.1, "ran_pars")
m4.1_ran_auxiliary <- tidy_rounded(m4.1, "auxiliary")
m4.1_CI <- as.data.frame(posterior_interval(m4.1, prob=0.95))
m4.1_CI <- m4.1_CI %>%
  tibble::rownames_to_column(var = "Parameters") %>%
  select(Parameters, everything())

View(m4.1_fixed)

##Probability estimate is non-zero
variables_of_interest <- c("Age_cat25-34", "Age_cat55+", "Age_cat45-54", "Higher_edu",
                           "Env_concern", "Gov_support", "EPS", "Incomequintile 2",
                           "Incomequintile 4", "Incomequintile 5", "EPS:Gov_support",
                           "b[(Intercept) Country_name:US]",
                           "b[EPS Country_name:US]", "b[(Intercept) Country_name:IL]",
                           "b[EPS Country_name:IL]", "b[(Intercept) Country_name:BE]",
                           "b[EPS Country_name:BE]", "b[(Intercept) Country_name:NL]",
                           "b[EPS Country_name:NL]", "b[(Intercept) Country_name:UK]",
                           "b[EPS Country_name:UK]", "b[(Intercept) Country_name:CA]",
                           "b[EPS Country_name:CA]", "b[(Intercept) Country_name:SE]",
                           "b[EPS Country_name:SE]", "b[(Intercept) Country_name:CH]",
                           "b[EPS Country_name:CH]", "b[(Intercept) Country_name:FR]",
                           "b[EPS Country_name:FR]")
fixed_random_df <- get_probabilities(m4.1$stan_summary, variables_of_interest)
# Round only numeric columns
fixed_random_df[, sapply(fixed_random_df, is.numeric)] <- round(fixed_random_df[, sapply(fixed_random_df, is.numeric)], 2)
m4.1_fixed_random <- fixed_random_df

ran_pars_df <- as.data.frame(m4.1_ran_pars)
colnames(ran_pars_df) <- c("Term", "Group", "Estimate")
ran_pars_df$Estimate <- round(ran_pars_df$Estimate, 2)
m4.1_ran_pars <- ran_pars_df

auxiliary_df <- as.data.frame(m4.1_ran_auxiliary)
colnames(auxiliary_df) <- c("Term", "Estimate", "Std_Error", "CI_Lower", "CI_Upper")
auxiliary_df[, sapply(auxiliary_df, is.numeric)] <- round(auxiliary_df[, sapply(auxiliary_df, is.numeric)], 2)
m4.1_auxiliary <- auxiliary_df

#--------------------------------- Summary -------------------------------------
#all following summaries and comparisons based on model m3.1, m4 and m2
#m3.1: varying intercepts and slopes with EPS as group-predictor
#m4: varying intercepts and slopes with EPS, and interaction term between group- & HH-level predictors EPS:Income
#m2: varying intercepts with EPS as group-predictor

#MCMC diagnostics --------------------------------------------------------------
##Plots
m3.1_mcmc_trace
m3.1_mcmc_acf_fixed
m3.1_mcmc_acf_random
m3.1_mcmc_dens_overlay

m4_mcmc_trace
m4_mcmc_acf_fixed
m4_mcmc_acf_random
m4_mcmc_dens_overlay

m2_mcmc_trace
m2_mcmc_acf_fixed
m2_mcmc_acf_random
m2_mcmc_dens_overlay

##neff-ratio
m2_neff_df <- data.frame(Parameter = names(m2_neff), Neff = m2_neff, Model = "m2")
m3.1_neff_df <- data.frame(Parameter = names(m3.1_neff), Neff = m3.1_neff, Model = "m3.1")
m4_neff_df <- data.frame(Parameter = names(m4_neff), Neff = m4_neff, Model = "m4")
neff_summary <- rbind(m3.1_neff_df, m4_neff_df, m2_neff_df)

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

##Rhat
m2_rhat_df <- data.frame(Parameter = names(m2_rhat), Rhat = m2_rhat, Model = "m2")
m3.1_rhat_df <- data.frame(Parameter = names(m3.1_rhat), Rhat = m3.1_rhat, Model = "m3.1")
m4_rhat_df <- data.frame(Parameter = names(m4_rhat), Rhat = m4_rhat, Model = "m4")
rhat_summary <- rbind(m3.1_rhat_df, m4_rhat_df, m2_rhat_df)

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

#Posterior predictive check ----------------------------------------------------
##plots
m3.1_ppc
m2_ppc
m4_ppc

##posterior predictive p-value
ppc_pval_df <- data.frame(
  Model = c("m2", "m3.1", "m4"),
  pppval = c(m2_pval, m3.1_pval, m4_pval)
)

# View the table
ppc_pval_df

#Posterior classification ------------------------------------------------------
print(WAIC_all_summary)

set.seed(84735)
m2_postclass <- classification_summary(model = m2, data = windows, cutoff = 0.7)
m3.1_postclass <- classification_summary(model = m3.1, data = windows, cutoff = 0.7)
m4_postclass <- classification_summary(model = m4, data = windows, cutoff = 0.7)

accuracy_list <- list(
  "Model m2" = m2_postclass$accuracy_rates,
  "Model m3.1" = m3.1_postclass$accuracy_rates,
  "Model m4" = m4_postclass$accuracy_rates
)

models_postclass_accuracy <- bind_rows(lapply(names(accuracy_list), function(model) {
  data.frame(
    Metric = rownames(accuracy_list[[model]]),
    Rate = round(unlist(accuracy_list[[model]]), 3),
    Model = model,
    row.names = NULL
  )
}))

# Pivot to wide format
models_postclass_accuracy <- models_postclass_accuracy %>%
  pivot_wider(names_from = Model, values_from = Rate) %>%
  select(Metric, `Model m2`, `Model m3.1`, `Model m4`)

#Posterior analysis ------------------------------------------------------------
##fixed effects
m2_fixed$Model <- "m2"
m3.1_fixed$Model <- "m3.1"
m4_fixed$Model <- "m4"

models_fixed <- bind_rows(m2_fixed, m3.1_fixed, m4_fixed)

model_order <- c("m2", "m3.1", "m4")
col_types <- c("estimate", "std.error", "conf.low", "conf.high")

models_fixed <- models_fixed %>%
  select(Model, term, all_of(col_types)) %>%
  pivot_wider(
    names_from = Model,
    values_from = all_of(col_types),
    names_glue = "{Model}_{.value}"
  ) %>%
  select(term, all_of(as.vector(t(outer(model_order, col_types, paste, sep = "_")))))

##random parameter
m2_ran_pars$Model <- "m2"
m3.1_ran_pars$Model <- "m3.1"
m4_ran_pars$Model <- "m4"

models_ran_pars <- bind_rows(m2_ran_pars, m3.1_ran_pars, m4_ran_pars) %>%
  mutate(Model = factor(Model, levels = c("m2", "m3.1", "m4"))) %>%
  arrange(Model) %>%
  select(Model, everything())

##auxiliary parameter
m2_auxiliary$Model <- "m2"
m3.1_auxiliary$Model <- "m3.1"
m4_auxiliary$Model <- "m4"

models_auxiliary <- bind_rows(m2_auxiliary, m3.1_auxiliary, m4_auxiliary) %>%
  mutate(Model = factor(Model, levels = c("m2", "m3.1", "m4"))) %>%
  arrange(Model) %>%
  select(Model, everything())

##posterior CI
m2_CI$Model <- "m2"
m3.1_CI$Model <- "m3.1"
m4_CI$Model <- "m4"

models_CI <- bind_rows(m2_CI, m3.1_CI, m4_CI)

model_order <- c("m2", "m3.1", "m4")
col_types <- c("2.5%", "97.5%")

models_CI <- models_CI %>%
  select(Model, Parameters, all_of(col_types)) %>%
  pivot_wider(
    names_from = Model,
    values_from = all_of(col_types),
    names_glue = "{Model}_{.value}"
  ) %>%
  select(Parameters, all_of(as.vector(t(outer(model_order, col_types, paste, sep = "_")))))

#-------------------------------------------------------------------------------
#------------------------------- Output ----------------------------------------
#-------------------------------------------------------------------------------

#--------------------------- Output Tables --------------------------------------
# List of your data frames:
tables_list <- list(
  rhat_summary_df = rhat_summary_df,
  neff_summary_df = neff_summary_df,
  ppc_pval_df = ppc_pval_df,
  models_postclass_accuracy = models_postclass_accuracy,
  WAIC_all_summary = WAIC_all_summary,
  models_fixed = models_fixed,
  models_ran_pars = models_ran_pars,
  models_auxiliary = models_auxiliary,
  models_CI = models_CI
)

# Output directory:
output_dir <- "./output"

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
##m2
ggsave("./output/output_windows/m2_mcmc_trace.pdf", plot = m2_mcmc_trace, dpi = 300, scale = 1.2)
ggsave("./output/output_windows/m2_mcmc_acf_fixed.pdf", plot = m2_mcmc_acf_fixed, dpi = 300, scale = 1.2)
ggsave("./output/output_windows/m2_mcmc_acf_random.pdf", plot = m2_mcmc_acf_random, dpi = 300, scale = 1.2)
ggsave("./output/output_windows/m2_mcmc_dens_overlay.pdf", plot = m2_mcmc_dens_overlay, dpi = 300, scale = 1.2)
ggsave("./output/output_windows/m2_ppc.pdf", plot = m2_ppc, dpi = 300, scale = 1.2)

##m3.1
ggsave("./output/scatter_EPS_adoption_by_tech_sub.pdf", plot = scatter_EPS_adoption_by_tech_sub, dpi = 300, scale = 1.2)


##m4



m3.1_mcmc_trace
m3.1_mcmc_acf_fixed
m3.1_mcmc_acf_random
m3.1_mcmc_dens_overlay
m3.1_ppc

m4_mcmc_trace
m4_mcmc_acf_fixed
m4_mcmc_acf_random
m4_mcmc_dens_overlay
m4_ppc


























