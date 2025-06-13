#-------------------------------------------------------------------------------
#--------------- Bayesian hierarchical model - Appliances ----------------------
#-------------------------------------------------------------------------------
# 1. Loading Packages
# 2. Loading Data
# 3. Bayesian Linear Regressions

#-------------------------------------------------------------------------------
#------------------------ 1. Loading Packages ----------------------------------
#-------------------------------------------------------------------------------
pacman::p_load("dplyr","haven", "readr", "bayesrules", "tidyverse", "broom.mixed",
               "mice", "rstanarm", "bayesplot", "ggplot2")

#-------------------------------------------------------------------------------
#-------------------------- 2. Loading Data ------------------------------------
#-------------------------------------------------------------------------------
#appliances
appliances <- read.csv("./processed_data/appliances.csv")
glimpse(appliances)
View(appliances)

#-------------------------------------------------------------------------------
#------------------- 3. Bayesian Linear Regressions ----------------------------
#-------------------------------------------------------------------------------

# 3.1 Weights and Normalized Weights -------------------------------------------
#weights are already normalized
sum(appliances$weight_2)
nrow(appliances)
summary(appliances$weight_2)

## 3.2 model2 weighted, level-2 predictor: varying intercept -------------------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitAppliances_m2 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS + (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = appliances
)

## Diagnostic Plots 
mcmc_trace(fitAppliances_m2)
mcmc_acf_bar(
  as.array(fitAppliances_m2),
  lags = 10
)
mcmc_dens_overlay(fitAppliances_m2)

## 3.3 model3.1 weighted, level-2 predictor: varying intercept and slopes --------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitAppliances_m3.1 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS + (EPS | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = appliances
)

## Diagnostic Plots 
mcmc_trace(fitAppliances_m3.1)
mcmc_acf_bar(
  as.array(fitAppliances_m3.1),
  lags = 10
)
mcmc_dens_overlay(fitAppliances_m3.1)

## 3.4 model4 weighted, level-2 predictor: varying intercept and slopes + interaction term --------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitAppliances_m4 <- stan_glmer(
  Adoption ~ Age_cat + Female + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS*Income + (EPS | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = appliances
)

## Diagnostic Plots 
mcmc_trace(fitAppliances_m4)
mcmc_acf_bar(
  as.array(fitAppliances_m4),
  lags = 10
)
mcmc_dens_overlay(fitAppliances_m4)

############################ SAVING RESULTS ####################################
#m2 - weighted, random and fixed effects
saveRDS(fitAppliances_m2, "./output/fitAppliances_m2.rds")
#m3.1 - weighted, randowm and fixed effects, varying slope and intercept (EPS|Country)
saveRDS(fitAppliances_m3.1, "./output/fitAppliances_m3.1.rds")
#m4 - weighted, randowm and fixed effects, varying slope and intercept + EPS*Income
saveRDS(fitAppliances_m4, "./output/fitAppliances_m4.rds")








