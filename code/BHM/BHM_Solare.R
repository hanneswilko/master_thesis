#-------------------------------------------------------------------------------
#--------------- Bayesian hierarchical model - Solare ----------------------
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
#Solare
Solare <- read.csv("./processed_data/solare.csv")
glimpse(Solare)
View(Solare)

#-------------------------------------------------------------------------------
#------------------- 3. Bayesian Linear Regressions ----------------------------
#-------------------------------------------------------------------------------

# 3.1 Weights and Normalized Weights -------------------------------------------
#weights are already normalized
sum(Solare$weight_2)
nrow(Solare)
summary(Solare$weight_2)

## 3.2 model2 weighted, level-2 predictor: varying intercept -------------------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitSolare_m2 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS + (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = Solare
)

## Diagnostic Plots 
mcmc_trace(fitSolare_m2)
mcmc_acf_bar(
  as.array(fitSolare_m2),
  lags = 10
)
mcmc_dens_overlay(fitSolare_m2)

summary(fitSolare_m2)

## 3.3 model3.1 weighted, level-2 predictor: varying intercept and slopes --------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitSolare_m3.1 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS + (EPS | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  control = list(adapt_delta = 0.999),
  weights = weight_2,
  data = Solare
)

## Diagnostic Plots 
mcmc_trace(fitSolare_m3.1)
mcmc_acf_bar(
  as.array(fitSolare_m3.1),
  lags = 10
)
mcmc_dens_overlay(fitSolare_m3.1)

mcmc_pairs(as.array(fitSolare_m3.1), pars = c("EPS", "Sigma[Country_name:(Intercept),(Intercept)]",
                                              "Sigma[Country_name:EPS,(Intercept)]"))

summary(fitSolare_m3.1)

## 3.4 model4 weighted, level-2 predictor: varying intercept and slopes + interaction term --------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitSolare_m4 <- stan_glmer(
  Adoption ~ Age_cat + Female + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS*Income + (EPS | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  control = list(adapt_delta = 0.999),
  weights = weight_2,
  data = Solare
)

## Diagnostic Plots 
mcmc_trace(fitSolare_m4)
mcmc_acf_bar(
  as.array(fitSolare_m4),
  lags = 10
)
mcmc_dens_overlay(fitSolare_m4)

mcmc_pairs(as.array(fitSolare_m4), pars = c("EPS", "Sigma[Country_name:(Intercept),(Intercept)]",
                                              "Sigma[Country_name:EPS,(Intercept)]"))

summary(fitSolare_m4)

############################ SAVING RESULTS ####################################
#m2 - weighted, random and fixed effects
saveRDS(fitSolare_m2, "./output/models_rds/fitSolare_m2.rds")
#m3.1 - weighted, random and fixed effects, varying slope and intercept (EPS|Country)
saveRDS(fitSolare_m3.1, "./output/models_rds/fitSolare_m3.1.rds")
#m4 - weighted, random and fixed effects, varying slope and intercept + EPS*Income
saveRDS(fitSolare_m4, "./output/models_rds/fitSolare_m4.rds")





