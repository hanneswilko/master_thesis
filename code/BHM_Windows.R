#-------------------------------------------------------------------------------
#--------------- Bayesian hierarchical model - Windows -------------------------
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
#windows
windows <- read.csv("./processed_data/windows.csv")
View(windows)

#-------------------------------------------------------------------------------
#------------------- 3. Bayesian Linear Regressions ----------------------------
#-------------------------------------------------------------------------------

# 3.1 Weights and Normalized Weights -------------------------------------------
#weights are already normalized
sum(windows$weight_2)
nrow(windows)
summary(windows$weight_2)

## 3.2 model1 weighted, random effects -----------------------------------------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitWindows_m1 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support + 
    (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = windows
)

## Diagnostic Plots 
mcmc_trace(fitWindows_m1)
mcmc_acf_bar(
  as.array(fitWindows_m1),
  lags = 10
)
mcmc_dens_overlay(fitWindows_m1)

## 3.3 model2 weighted, level-2 predictor: varying intercept -------------------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitWindows_m2 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS + (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = windows
)

## Diagnostic Plots 
mcmc_trace(fitWindows_m2)
mcmc_acf_bar(
  as.array(fitWindows_m2),
  lags = 10
)
mcmc_dens_overlay(fitWindows_m2)

## 3.4 model3 weighted, level-2 predictor: varying intercept and slopes --------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitWindows_m3 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS + (1 + EPS | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = windows
)

## Diagnostic Plots 
mcmc_trace(fitWindows_m3)
mcmc_acf_bar(
  as.array(fitWindows_m3),
  lags = 10
)
mcmc_dens_overlay(fitWindows_m3)

## 3.5 model3.1 weighted, level-2 predictor: varying intercept and slopes --------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitWindows_m3.1 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS + (EPS | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = windows
)

## Diagnostic Plots 
mcmc_trace(fitWindows_m3.1)
mcmc_acf_bar(
  as.array(fitWindows_m3.1),
  lags = 10
)
mcmc_dens_overlay(fitWindows_m3.1)

## 3.6 model4 weighted, level-2 predictor: varying intercept and slopes + interaction term --------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitWindows_m4 <- stan_glmer(
  Adoption ~ Age_cat + Female + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS*Income + (1 + EPS | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = windows
)

## Diagnostic Plots 
mcmc_trace(fitWindows_m4)
mcmc_acf_bar(
  as.array(fitWindows_m4),
  lags = 10
)
mcmc_dens_overlay(fitWindows_m4)

## 3.7 model4.1 weighted, level-2 predictor: varying intercept and slopes + interaction term --------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitWindows_m4.1 <- stan_glmer(
  Adoption ~ Age_cat + Female + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Income +
    EPS*Gov_support + (1 + EPS | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = windows
)

## Diagnostic Plots 
mcmc_trace(fitWindows_m4.1)
mcmc_acf_bar(
  as.array(fitWindows_m4.1),
  lags = 10
)
mcmc_dens_overlay(fitWindows_m4.1)

############################ SAVING RESULTS ####################################
#m1 - weighted, random effects
saveRDS(fitWindows_m1, "./output/fitWindows_m1.rds")
#m2 - weighted, random and fixed effects
saveRDS(fitWindows_m2, "./output/fitWindows_m2.rds")
#m3 - weighted, randowm and fixed effects, varying slope and intercept (1 + EPS|Country)
saveRDS(fitWindows_m3, "./output/fitWindows_m3.rds")
#m3.1 - weighted, randowm and fixed effects, varying slope and intercept (EPS|Country)
saveRDS(fitWindows_m3.1, "./output/fitWindows_m3.1.rds")
#m4 - weighted, randowm and fixed effects, varying slope and intercept + EPS*Income
saveRDS(fitWindows_m4, "./output/fitWindows_m4.rds")
#m4.1 - weighted, randowm and fixed effects, varying slope and intercept + EPS*Gov_support
saveRDS(fitWindows_m4.1, "./output/fitWindows_m4.1.rds")








