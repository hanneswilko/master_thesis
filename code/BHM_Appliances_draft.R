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
bayesplot::mcmc_trace(fitJOBSAw, 
                      pars=c("(Intercept)","tt3g08","tt3g01",
                             "tt3g11b","t3self","sigma",
                             "Sigma[idschool:(Intercept),(Intercept)]",
                             "Sigma[idschool:tt3g08,(Intercept)]"))

bayesplot::mcmc_acf_bar(fitJOBSAw, 
                        pars=c("(Intercept)","tt3g08","tt3g01",
                               "tt3g11b","t3self","sigma",
                               "Sigma[idschool:(Intercept),(Intercept)]",
                               "Sigma[idschool:tt3g08,(Intercept)]"))

bayesplot::mcmc_dens(fitJOBSAw, 
                     pars=c("(Intercept)","tt3g08","tt3g01",
                            "tt3g11b","t3self","sigma",
                            "Sigma[idschool:(Intercept),(Intercept)]",
                            "Sigma[idschool:tt3g08,(Intercept)]"))

## 3.4 Summary Results ---------------------------------------------------------
summary(fitJOBSAw, pars=c("(Intercept)","tt3g08","tt3g01",
                          "tt3g11b","t3self","sigma",
                          "Sigma[idschool:(Intercept),(Intercept)]",
                          "Sigma[idschool:tt3g08,(Intercept)]"))

posterior_interval(fitJOBSAw,prob=0.95,
                   pars=c("(Intercept)","tt3g08","tt3g01",
                          "tt3g11b","t3self","sigma",
                          "Sigma[idschool:(Intercept),(Intercept)]",
                          "Sigma[idschool:tt3g08,(Intercept)]"))

## 3.5 Posterior predictive plot and Bayesian p-value --------------------------
t3jobsa <- TALIS7w$t3jobsa
t3jobsa_rep <- posterior_predict(fitJOBSAw,draws=1000)
ppc_stat(t3jobsa, t3jobsa_rep, stat = "mean")
pvalw <- mean(apply(t3jobsa_rep, 1, mean) > mean(t3jobsa))
pvalw

## 3.6 Probability estimate is non-zero ----------------------------------------
mat <- as.matrix(fitJOBSAw$stan_summary)
m <- mat["tt3g08","mean"]
s <- mat["tt3g08", "sd"]

pnorm(m, mean = 0 , sd = s , lower.tail = T) - pnorm(0, mean = 0, sd = s, lower.tail = T)











