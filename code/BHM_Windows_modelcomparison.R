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
               "mice", "rstanarm", "bayesplot", "ggplot2")

#-------------------------------------------------------------------------------
#-------------------------- 2. Loading Data ------------------------------------
#-------------------------------------------------------------------------------
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

#priors ------------------------------------------------------------------------
prior_summary(fitWindows_m3.1)

#Diagnostics -------------------------------------------------------------------
#Diagnostic Plots 
bayesplot::mcmc_trace(fitWindows_m3.1)

bayesplot::mcmc_acf_bar(
  as.array(fitWindows_m3.1), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility

bayesplot::mcmc_hist(fitWindows_m3.1)

#Summary
summary(fitWindows_m3.1)

#Posterior predictive plot
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(fitWindows_m3.1,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")

#Bayesian p-value
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

#-------------------------------------------------------------------------------
#------------------------- 4. BHM comparison -----------------------------------
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
#--------------------------- 5. BHM results ------------------------------------
#-------------------------------------------------------------------------------

posterior_interval(fitWindows_m3.1,prob=0.95)
tidy(fitWindows_m3.1, effects = "ran_pars") #standard deviations random effects
tidy(fitWindows_m3.1, effects = "fixed", conf.int = T, conf.level = 0.95)

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







