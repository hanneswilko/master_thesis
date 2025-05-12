#-------------------------------------------------------------------------------
#------------ Bayesian hierarchical model - Appliances -------------------------
#-------------------------------------------------------------------------------
# 1. Loading Packages
# 2. Loading Data
# 3. Data wrangling
# 4. Bayesian Linear Regressions
## 4.1 Weights and Normalized Weights
## 4.2 Model Setup
## 4.3 Diagnostic Plots
## 4.4 Summary Results
## 4.5 Posterior predictive plot and Bayesian p-value
## 4.6 Probability estimate is non-zero

#-------------------------------------------------------------------------------
#------------------------ 1. Loading Packages ----------------------------------
#-------------------------------------------------------------------------------
pacman::p_load("dplyr","tidyr","haven", "readr",
               "mice", "rstanarm", "bayesplot", "ggplot2")

options(mc.cores = 4) #for speeding up computation when working with models or imputation tasks that support parallelization

#-------------------------------------------------------------------------------
#-------------------------- 2. Loading Data ------------------------------------
#-------------------------------------------------------------------------------
#Appliances
appliances <- read.csv("./processed_data/appliances.csv")
View(appliances)

#-------------------------------------------------------------------------------
#-------------------------- 3. Data Wrangling ----------------------------------
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
#------------------- 4. Bayesian Linear Regressions ----------------------------
#-------------------------------------------------------------------------------











