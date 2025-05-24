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
#Appliances
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
bayesplot::mcmc_trace(fitWindows_m1)
bayesplot::mcmc_acf_bar(
  as.array(fitWindows_m1), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitWindows_m1)

## Summary Results
summary(fitWindows_m1)
posterior_interval(fitWindows_m1,prob=0.95)

## Posterior predictive plot and Bayesian p-value 
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(fitWindows_m1,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## Probability estimate is non-zero

#income
mat <- as.matrix(fitWindows_m1$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#government support
mat <- as.matrix(fitWindows_m1$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

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

prior_summary(fitWindows_m2)

## Diagnostic Plots 
bayesplot::mcmc_trace(fitWindows_m2)
bayesplot::mcmc_acf_bar(
  as.array(fitWindows_m2), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitWindows_m2)

## Summary Results
summary(fitWindows_m2)
posterior_interval(fitWindows_m2,prob=0.95)

## Posterior predictive plot and Bayesian p-value 
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(fitWindows_m2,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## Probability estimate is non-zero

#income
mat <- as.matrix(fitWindows_m2$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#government support
mat <- as.matrix(fitWindows_m2$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#EPS
mat <- as.matrix(fitWindows_m2$stan_summary)
m <- mat["EPS","mean"]
s <- mat["EPS", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

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

prior_summary(fitWindows_m3)

## Diagnostic Plots 
bayesplot::mcmc_trace(fitWindows_m3)
bayesplot::mcmc_acf_bar(
  as.array(fitWindows_m3), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitWindows_m3)

## Summary Results
summary(fitWindows_m3)
posterior_interval(fitWindows_m3,prob=0.95)
tidy(fitWindows_m3, effects = "ran_pars") #standard deviations random effects
tidy(fitWindows_m3, effects = "fixed", conf.int = T, conf.level = 0.95)

## Posterior predictive plot and Bayesian p-value 
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(fitWindows_m3,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## Probability estimate is non-zero

#income
mat <- as.matrix(fitWindows_m3$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#government support
mat <- as.matrix(fitWindows_m3$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#EPS
mat <- as.matrix(fitWindows_m3$stan_summary)
m <- mat["EPS","mean"]
s <- mat["EPS", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

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

prior_summary(fitWindows_m3)

## Diagnostic Plots 
bayesplot::mcmc_trace(fitWindows_m3)
bayesplot::mcmc_acf_bar(
  as.array(fitWindows_m3), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitWindows_m3)

## Summary Results
summary(fitWindows_m3)
posterior_interval(fitWindows_m3,prob=0.95)
tidy(fitWindows_m3, effects = "ran_pars") #standard deviations random effects
tidy(fitWindows_m3, effects = "fixed", conf.int = T, conf.level = 0.95)

## Posterior predictive plot and Bayesian p-value 
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(fitWindows_m3,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## Probability estimate is non-zero

#income
mat <- as.matrix(fitWindows_m3$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#government support
mat <- as.matrix(fitWindows_m3$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#EPS
mat <- as.matrix(fitWindows_m3$stan_summary)
m <- mat["EPS","mean"]
s <- mat["EPS", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

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

prior_summary(fitWindows_m4)

## Diagnostic Plots 
bayesplot::mcmc_trace(fitWindows_m4)
bayesplot::mcmc_acf_bar(
  as.array(fitWindows_m4), 
  pars = c("Incomequintile 2", "Incomequintile 3", "Incomequintile 4", "Incomequintile 5","Rural"),
  lags = 10
) #check per variable or group of variables to increase visibility
bayesplot::mcmc_hist(fitWindows_m4)

## Summary Results
summary(fitWindows_m4)
posterior_interval(fitWindows_m4,prob=0.95)

## Posterior predictive plot and Bayesian p-value 
Adoption <- windows$Adoption
Adoption_rep <- posterior_predict(fitWindows_m4,draws=1000)
ppc_stat(Adoption, Adoption_rep, stat = "mean")
pval <- mean(apply(Adoption_rep, 1, mean) > mean(Adoption))
pval

## Probability estimate is non-zero
#income
mat <- as.matrix(fitWindows_m4$stan_summary)
m <- mat["Incomequintile 2","mean"]
s <- mat["Incomequintile 2", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#government support
mat <- as.matrix(fitWindows_m4$stan_summary)
m <- mat["Gov_support","mean"]
s <- mat["Gov_support", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#EPS
mat <- as.matrix(fitWindows_m4$stan_summary)
m <- mat["EPS","mean"]
s <- mat["EPS", "sd"]
#prob <0
pnorm(0, mean = m, sd = s)
#prob >0
pnorm(0, mean = m, sd = s, lower.tail = FALSE)

#SAVING RESULTS
#m1 - weighted, random effects
saveRDS(fitWindows_m1, "./output/fitWindows_m1.rds")
#m2 - weighted, random and fixed effects
saveRDS(fitWindows_m2, "./output/fitWindows_m2.rds")
#m3 - weighted, randowm and fixed effects, varying slope and intercept
saveRDS(fitWindows_m3, "./output/fitWindows_m3.rds")
#m4 - weighted, randowm and fixed effects, varying slope and intercept + interactionterm
saveRDS(fitWindows_m4, "./output/fitWindows_m4.rds")

############################### Next Steps #####################################
#interaction government support:EPS model 5

#interpreting results
library(ggeffects)
plot(ggpredict(fitAppliances_m5, terms = c("EPS", "Income")))







