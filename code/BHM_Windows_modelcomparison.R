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
pacman::p_load("dplyr","haven", "readr", "bayesrules", "tidyverse", "broom.mixed",
               "mice", "rstanarm", "bayesplot", "ggplot2", "loo")

#-------------------------------------------------------------------------------
#-------------------------- 2. Loading Data ------------------------------------
#-------------------------------------------------------------------------------
#data windows
windows <- read.csv("./processed_data/windows.csv")

#BHM windows models
m1 <- read_rds("./output/fitWindows_m1.rds") # varying intercept
m2 <- read_rds("./output/fitWindows_m2.rds") # varying intercept + group level predictor
m3 <- read_rds("./output/fitWindows_m3.rds") # varying intercept & slope (1+EPS|country)
m3.1 <- read_rds("./output/fitWindows_m3.1.rds") # varying intercept & slope (EPS|country)
m4 <- read_rds("./output/fitWindows_m4.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Income
m4.1 <- read_rds("./output/fitWindows_m4.1.rds") # varying intercept & slope (1+EPS|country) + interaction EPS*Gov_support

#-------------------------------------------------------------------------------
#------------------------- 3. BHM evaluation -----------------------------------
#-------------------------------------------------------------------------------

#------------------------------ Model 1 ----------------------------------------
# varying intercept (random effects)
names(m1)

#priors ------------------------------------------------------------------------
prior_summary(m1)
m1$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m1_array <- as.array(m1)
m1_all_pars <- dimnames(m1_array)$parameters

m1_fixed_pars <- m1_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m1_all_pars)]
m1_random_pars <- m1_all_pars[grepl("^(b\\[|Sigma|cor_)", m1_all_pars)]

#Diagnostic Plots 
mcmc_trace(m1)

mcmc_dens_overlay(m1)

# ACF for fixed effects
mcmc_acf_bar(m1_array, pars = m1_fixed_pars, lags = 10)
# ACF for random effects
mcmc_acf_bar(m1_array, pars = m1_random_pars, lags = 10)

#Summary
neff_ratio(m1)
rhat(m1)
summary(m1)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m1,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

#------------------------------ Model 2 ----------------------------------------
# varying intercept (random effects) + group-level predictor
names(m2)

#priors ------------------------------------------------------------------------
prior_summary(m2)
m2$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m2_array <- as.array(m2)
m2_all_pars <- dimnames(m2_array)$parameters

m2_fixed_pars <- m2_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m2_all_pars)]
m2_random_pars <- m2_all_pars[grepl("^(b\\[|Sigma|cor_)", m2_all_pars)]

#Diagnostic Plots 
mcmc_trace(m2)

mcmc_dens_overlay(m2)

# ACF for fixed effects
mcmc_acf_bar(m2_array, pars = m2_fixed_pars, lags = 10)
# ACF for random effects
mcmc_acf_bar(m2_array, pars = m2_random_pars, lags = 10)

#Summary
neff_ratio(m2)
rhat(m2)
summary(m2)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m2,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

#------------------------------ Model 3 ----------------------------------------
# varying intercept & slope (1+EPS|country)
names(m3)

#priors ------------------------------------------------------------------------
prior_summary(m3)
m3$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m3_array <- as.array(m3)
m3_all_pars <- dimnames(m3_array)$parameters

m3_fixed_pars <- m3_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m3_all_pars)]
m3_random_pars <- m3_all_pars[grepl("^(b\\[|Sigma|cor_)", m3_all_pars)]

#Diagnostic Plots 
mcmc_trace(m3)

mcmc_dens_overlay(m3)

# ACF for fixed effects
mcmc_acf_bar(m3_array, pars = m3_fixed_pars, lags = 10)
# ACF for random effects
mcmc_acf_bar(m3_array, pars = m3_random_pars, lags = 10)

#Summary
neff_ratio(m3)
rhat(m3)
summary(m3)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m3)
ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

#------------------------------ Model 3.1 --------------------------------------
# varying intercept & slope (EPS|country)
names(m3.1)

#priors ------------------------------------------------------------------------
prior_summary(m3.1)
m3.1$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m3.1_array <- as.array(m3.1)
m3.1_all_pars <- dimnames(m3.1_array)$parameters

m3.1_fixed_pars <- m3.1_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m3.1_all_pars)]
m3.1_random_pars <- m3.1_all_pars[grepl("^(b\\[|Sigma|cor_)", m3.1_all_pars)]

#Diagnostic Plots 
mcmc_trace(m3.1)

mcmc_dens_overlay(m3.1)

# ACF for fixed effects
mcmc_acf_bar(m3.1_array, pars = m3.1_fixed_pars, lags = 10)
# ACF for random effects
mcmc_acf_bar(m3.1_array, pars = m3.1_random_pars, lags = 10)

#Summary
neff_ratio(m3.1)
rhat(m3.1)
summary(m3.1)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m3.1)
ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

#------------------------------ Model 4 ----------------------------------------
# varying intercept & slope (1+EPS|country) + interaction EPS*Income
names(m4)

#priors ------------------------------------------------------------------------
prior_summary(m4)
m4$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m4_array <- as.array(m4)
m4_all_pars <- dimnames(m4_array)$parameters

m4_fixed_pars <- m4_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m4_all_pars)]
m4_random_pars <- m4_all_pars[grepl("^(b\\[|Sigma|cor_)", m4_all_pars)]

#Diagnostic Plots 
mcmc_trace(m4)

mcmc_dens_overlay(m4)

# ACF for fixed effects
mcmc_acf_bar(m4_array, pars = m4_fixed_pars, lags = 10)
# ACF for random effects
mcmc_acf_bar(m4_array, pars = m4_random_pars, lags = 10)

#Summary
neff_ratio(m4)
rhat(m4)
summary(m4)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m4)
ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

#------------------------------ Model 4.1 --------------------------------------
# varying intercept & slope (1+EPS|country) + interaction EPS*Income
names(m4.1)

#priors ------------------------------------------------------------------------
prior_summary(m4.1)
m4.1$prior.info

#Diagnostics -------------------------------------------------------------------
##separate fixed and random effects
m4.1_array <- as.array(m4.1)
m4.1_all_pars <- dimnames(m4.1_array)$parameters

m4.1_fixed_pars <- m4.1_all_pars[!grepl("^(b\\[|Sigma|cor_|lp__)", m4.1_all_pars)]
m4.1_random_pars <- m4.1_all_pars[grepl("^(b\\[|Sigma|cor_)", m4.1_all_pars)]

#Diagnostic Plots 
mcmc_trace(m4.1)

mcmc_dens_overlay(m4.1)

# ACF for fixed effects
mcmc_acf_bar(m4.1_array, pars = m4.1_fixed_pars, lags = 10)
# ACF for random effects
mcmc_acf_bar(m4.1_array, pars = m4.1_random_pars, lags = 10)

#Summary
neff_ratio(m4.1)
rhat(m4.1)
summary(m4.1)

#Posterior predictive checks
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(m4.1)
ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

#-------------------------------------------------------------------------------
#------------------------- 4. BHM comparison -----------------------------------
#-------------------------------------------------------------------------------

#----------------- Watanabe-Akaike Information Criterion -----------------------
waic_m1 <- waic(m1)
waic_m2 <- waic(m2)
waic_m3 <- waic(m3)
waic_m3.1 <- waic(m3.1)
waic_m4 <- waic(m4)
waic_m4.1 <- waic(m4.1)

loo_compare(waic_m1, waic_m2, waic_m3, waic_m3.1, waic_m4, waic_m4.1)

#-------------------------------------------------------------------------------
#--------------------------- 5. BHM results ------------------------------------
#-------------------------------------------------------------------------------
##results for m3, m3.1, m4, m4.1

#-------------------------- Posterior analysis ---------------------------------
tidy_rounded <- function(model, effect) {
  tidy(model, effects = effect, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

#model 3------------------------------------------------------------------------
m3_fixed <- tidy_rounded(m3, "fixed")
m3_ran_vals <- tidy_rounded(m3, "ran_vals")
m3_ran_pars <- tidy_rounded(m3, "ran_pars")
m3_auxiliary <- tidy_rounded(m3, "auxiliary")
posterior_interval(m3, prob=0.95)


## Probability estimate is non-zero

#income
mat <- as.matrix(fitWindows_m3.1$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#government support
mat <- as.matrix(fitWindows_m3.1$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#EPS
mat <- as.matrix(fitWindows_m3.1$stan_summary)
m <- mat["EPS","mean"]
s <- mat["EPS", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#interpreting results
library(ggeffects)
plot(ggpredict(fitAppliances_m5, terms = c("EPS", "Income")))







