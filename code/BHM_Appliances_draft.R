#-------------------------------------------------------------------------------
#------------ Bayesian hierarchical model - Appliances -------------------------
#-------------------------------------------------------------------------------
# 1. Loading Packages
# 2. Loading Data
# 3. Bayesian Linear Regressions
## 3.1 Weights and Normalized Weights
## 3.2 Model Setup
## 3.3 Diagnostic Plots
## 3.4 Summary Results
## 3.5 Posterior predictive plot and Bayesian p-value
## 3.6 Probability estimate is non-zero

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
#### Unweighted
fitAppliances <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support + 
    (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 5000, thin = 10,
  data = appliances
)

##### Diagnostic plots 
bayesplot::mcmc_trace(fitAppliances)
bayesplot::mcmc_acf_bar(
  as.array(fitAppliances), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitAppliances)

##### Summary of results with 95% posterior intervals
summary(fitAppliances)
posterior_interval(fitAppliances,prob=0.95)

##### Posterior predictive plot and Bayesian p-value
Adoption <- appliances$Adoption
Adoption_rep <- posterior_predict(fitAppliances,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

##### Probability estimate is non-zero
mat <- as.matrix(fitAppliances$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
pnorm(0, mean = m, sd = s)

############################## Next steps ######################################

## 3.1 Weights and Normalized Weights ------------------------------------------
#weights are already normalized
sum(appliances$weight_2)
nrow(appliances)
summary(appliances$weight_2)

## 3.2 Model Setup -------------------------------------------------------------
options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

fitAppliancesw <- stan_glmer(
  Adoption ~ Age_cat + Female + Income + Higher_edu + Home_ownership + 
    Dwelling_house + Dwelling_size + Rural + Env_concern + Gov_support + 
    (1 | Country_name),
  family = binomial(link = "logit"),
  prior_covariance = decov(regularization = 3),
  iter = 2000, warm = 1000, thin = 1,
  weights = weight_2,
  data = appliances
)

## 3.3 Diagnostic Plots --------------------------------------------------------
bayesplot::mcmc_trace(fitAppliancesw)
bayesplot::mcmc_acf_bar(
  as.array(fitAppliancesw), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitAppliancesw)

## 3.4 Summary Results ---------------------------------------------------------
summary(fitAppliancesw)
posterior_interval(fitAppliancesw,prob=0.95)

## 3.5 Posterior predictive plot and Bayesian p-value --------------------------
Adoption <- appliances$Adoption
Adoption_rep <- posterior_predict(fitAppliancesw,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## 3.6 Probability estimate is non-zero ----------------------------------------
#income
mat <- as.matrix(fitAppliancesw$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
pnorm(0, mean = m, sd = s)

#government support
mat <- as.matrix(fitAppliancesw$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]

#prob <0
pnorm(0, mean = m, sd = s)

#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)











