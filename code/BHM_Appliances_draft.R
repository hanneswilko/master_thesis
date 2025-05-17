#-------------------------------------------------------------------------------
#------------ Bayesian hierarchical model - Appliances -------------------------
#-------------------------------------------------------------------------------
# 1. Loading Packages
# 2. Loading Data
# 3. Bayesian Linear Regressions

#-------------------------------------------------------------------------------
#------------------------ 1. Loading Packages ----------------------------------
#-------------------------------------------------------------------------------
pacman::p_load("dplyr","tidyr","haven", "readr",
               "mice", "rstanarm", "bayesplot", "ggplot2")

#-------------------------------------------------------------------------------
#-------------------------- 2. Loading Data ------------------------------------
#-------------------------------------------------------------------------------
#Appliances
appliances <- read.csv("./processed_data/appliances.csv")
View(appliances)

#-------------------------------------------------------------------------------
#------------------- 3. Bayesian Linear Regressions ----------------------------
#-------------------------------------------------------------------------------

# 3.1 Weights and Normalized Weights -------------------------------------------
#weights are already normalized
sum(appliances$weight_2)
nrow(appliances)
summary(appliances$weight_2)

# 3.2 model1 unweighted, random effects ----------------------------------------
fitAppliances_m1 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support + 
    (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 5000, thin = 10,
  data = appliances
)

## Diagnostic plots 
bayesplot::mcmc_trace(fitAppliances_m1)
bayesplot::mcmc_acf_bar(
  as.array(fitAppliances_m1), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitAppliances_m1)

## Summary of results with 95% posterior intervals
summary(fitAppliances_m1)
posterior_interval(fitAppliances_m1,prob=0.95)

## Posterior predictive plot and Bayesian p-value
Adoption <- appliances$Adoption
Adoption_rep <- posterior_predict(fitAppliances_m1,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## Probability estimate is non-zero
mat <- as.matrix(fitAppliances_m1$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
pnorm(0, mean = m, sd = s)

## 3.3 model2 weighted, random effects -----------------------------------------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitAppliances_m2 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support + 
    (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = appliances
)

## Diagnostic Plots 
bayesplot::mcmc_trace(fitAppliances_m2)
bayesplot::mcmc_acf_bar(
  as.array(fitAppliances_m2), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitAppliances_m2)

## Summary Results
summary(fitAppliances_m2)
posterior_interval(fitAppliances_m2,prob=0.95)

## Posterior predictive plot and Bayesian p-value 
Adoption <- appliances$Adoption
Adoption_rep <- posterior_predict(fitAppliances_m2,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## Probability estimate is non-zero

#income
mat <- as.matrix(fitAppliances_m2$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
pnorm(0, mean = m, sd = s)
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#government support
mat <- as.matrix(fitAppliances_m2$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)


## 3.4 model3 weighted, level-2 predictor -----------------------------------------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitAppliances_m3 <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support +
    EPS + (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = appliances
)

prior_summary(fitAppliances_m3)

## Diagnostic Plots 
bayesplot::mcmc_trace(fitAppliances_m2)
bayesplot::mcmc_acf_bar(
  as.array(fitAppliances_m2), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitAppliances_m2)

## Summary Results
summary(fitAppliances_m2)
posterior_interval(fitAppliances_m2,prob=0.95)

## Posterior predictive plot and Bayesian p-value 
Adoption <- appliances$Adoption
Adoption_rep <- posterior_predict(fitAppliances_m2,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## Probability estimate is non-zero

#income
mat <- as.matrix(fitAppliances_m2$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
pnorm(0, mean = m, sd = s)
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#government support
mat <- as.matrix(fitAppliances_m2$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)










