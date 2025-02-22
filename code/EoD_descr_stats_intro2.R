
setwd("/Users/franziskadisslbacher/Documents/lehre/r_p_econdist/LWS_DATA")

####
library(tidyverse)
library(Hmisc) #weighted stats
library(sos) #find function
library(spatstat) #weighted stats
library(haven) #load st<ata
library(acid) #weighted ineq measures
library(ineq)
library(survey)
library(mitools)

###
us16wh <- read_dta("./LWS_Data/us16wh.dta")
us16wr <- read_dta("./LWS_Data/us16wr.dta")
us16wp <- read_data("./LWS_Data/us16wp.dta")
##NET WEALTH: 
#hid
#inum
#hpopwgt
a <- us16wh %>% group_by(inum) %>%
  summarise(mean_dnw = mean(dnw, na.rm = TRUE), 
            median_dnw = quantile(dnw, 0.5), 
            p90 = quantile(dnw, 0.9),
            pop = sum(hpopwgt), 
            sample = n()) %>%
  summarise_all(mean) %>%
  mutate(type = "unweighted")

#Explain Weights

#Weighted Stats
weighted.mean.ud <- function(x,w){
  wm <- sum(x*w, na.rm=T)/sum(w, na.rm = T)
  return(wm)
}

#better:
weighted.mean.ud <- function(x, w) {
  y <- x[which(!is.na(x))]
  wgt <- w[which(!is.na(x))]
  wmean <- sum(y*wgt/sum(wgt))
  return(wmean)
}

weighted.median.ud <- function(x, w) {
  w <- w[order(x)]
  x <- x[order(x)]
  prob <- cumsum(w)/sum(w)
  ps <- which(abs(prob - .5) == min(abs(prob - .5)))
  return(x[ps])
}


weighted.quantile.ud <- function(x, w, quant) {
  w <- w[order(x)]
  x <- x[order(x)]
  prob <- cumsum(w)/sum(w)  
  ps <- which(abs(prob - quant) == min(abs(prob - quant)))
  return(x[ps])
}

b <- us16wh %>% group_by(inum) %>%
  summarise(mean_dnw = weighted.mean.ud(x =dnw, w=hpopwgt),
            median_dnw = weighted.median.ud(x=dnw, w=hpopwgt),
            p90 = weighted.quantile.ud(x = dnw, w = hpopwgt, quant = 0.9),
            pop = sum(hpopwgt), 
            sample = n()) %>%
  summarise_all(mean) %>%
  mutate(type = "weighted.ud")

#Hmisc
c <- us16wh %>% group_by(inum) %>%
  summarise(mean_dnw = weighted.mean(x = dnw, w = hpopwgt),
            median_dnw = weighted.median(dnw, hpopwgt, type = 4), 
            p90 = spatstat.geom::weighted.quantile(dnw, hpopwgt, probs = 0.9, type = 4),
            pop = sum(hpopwgt), 
            sample = n()) %>%
  summarise_all(mean) %>%
  mutate(type = "weighted_hmisc")

View(bind_rows(a,b,c))

###A function for the Gini index
## Value and Population Shares
us16wh<-us16wh[order(us16wh$dnw),]

e1<-cumsum(us16wh$dnw*us16wh$hpopwgt/sum(us16wh$dnw*us16wh$hpopwgt))
p1<-cumsum(us16wh$hpopwgt/sum(us16wh$hpopwgt))

#A Lorenz Curve
plot(p1, e1) 
abline(a = 0, b = 1)

lorenz.data <- data.frame(cum.pop.share = p1, 
                          cum.wealth.share = e1)

ggplot(data = lorenz.data) +
  aes(x = cum.pop.share, y = cum.wealth.share) +
  geom_point(colour = "red") +
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  theme_classic() +
  labs(title = "A Lorenz Curve") + 
  xlab("Cumulative Population Share") +
  ylab("Cumulative Wealth Share")

#
x <- us16wh$dnw
w <- us16wh$hpopwgt

gini.ud <- function(x,w) {
  ox <- order(x)
  x <- x[ox]
  weight <- w[ox]/sum(w)
  p <- cumsum(weight)
  nu <- cumsum(weight * x) 
  n <-  length(nu)
  nu <- nu/nu[n]
  gini <- sum(nu[-1]*p[-n]) - sum(nu[-n]*p[-1])
  return(gini) 
  }


# Gini
gini.ud_b <- function(x, w){
  ox <- order(x)
  x <- x[ox]
  w <- w[ox]
  e2<-cumsum(x*w/sum(x*w))
  p2<-cumsum(w/sum(w))
  
  gini <- (0.5-sum((p1-c(0,p1[1:(length(p1)-1)]))*(e1+c(0,e1[1:(length(e1)-1)]))/2))/0.5
  return(gini)
}

gini.ud_b(x = us16wh$dnw, 
          w = us16wh$hpopwgt)

gini.ud(x = us16wh$dnw, 
        w = us16wh$hpopwgt)


d <- us16wh %>% group_by(inum) %>%
  summarise(mean_dnw = weighted.mean.ud(x =dnw, w=hpopwgt),
            median_dnw = weighted.median.ud(x=dnw, w=hpopwgt),
            p90 = weighted.quantile.ud(x = dnw, w = hpopwgt, quant = 0.9),
            gini_idx = gini.ud_b(x = dnw, w = hpopwgt),
            pop = sum(hpopwgt), 
            sample = n()) %>%
  summarise_all(mean) %>%
  mutate(type = "weighted.ud") 


#gini from package
e <- us16wh %>% group_by(inum) %>%
  summarise(mean_dnw = weighted.mean(x = dnw, w = hpopwgt),
            median_dnw = weighted.median(dnw, hpopwgt, type = 4), 
            p90 = spatstat.geom::weighted.quantile(dnw, hpopwgt, probs = 0.9, type = 4),
            gini_idx = as.numeric(acid::weighted.gini(dnw, hpopwgt)$Gini[1,1]),
            pop = sum(hpopwgt), 
            sample = n()) %>%
  summarise_all(mean) %>%
  mutate(type = "weighted_hmisc") %>% select(-inum)

View(bind_rows(d,e))





#Combine Files

us16wph <- left_join(us16wh, 
                    us16wp %>% dplyr::select(hid, inum, ppopwgt, pid, sex, age, immigr,
                                       educ, wexptl, edmom_c, eddad_c, emp, fyft)
                    )

##Survey Package

#Survey Design 
us.svymi<-svydesign(ids=us16wh$hid, 
                    weights=~hpopwgt, data=us16wh)
us.svymip <- svydesign(ids = us16wph$pid, 
                       weights = ~ppopwgt, 
                       data = us16wph)

svymean(~dnw, us.svymi)
svymean(~dnw, us.svymip)
#Where does the difference come from?

svyquantile(~dnw, 
            design = us.svymi, 
            quantiles = c(0.5, 0.9),
            ci = TRUE)

ineq.sample(x="dnw", 
            design = us.svymi, 
            measure = "gini")

ineq.sample(x="dnw", 
            design = us.svymi, 
            measure = c("mean"))


#by receipt of an inheritance
#check codebook: 0 -> not received; 1 -> received
noI <- subset(us.svymi, pir==0)
yesI <- subset(us.svymi, pir==1)

#han: Non-financial
#haf: Financial Excluding Pensions, 
#hass: Social Security Pension Entitlements
#hlr: Real estate liabilities
#hln: non-housing liabilities

no_res <- svymean(~dnw + han + haf + hlr + hln + pia1, noI)
yes_res <- svymean(~dnw + han + haf + hlr + hln + pia1, yesI)
no_res$inherited <- "No"
yes_res$inherited <- "Yes"
res <- bind_rows(no_res, yes_res)

svyquantile(~dnw + han + haf + hlr + hln + pia1, quantiles = 0.5, noI)

#Svyby
svyby(~dnw,
      ~sex+age,
      svymean, design = us.svymip)

##MItools and survey packages

# multiple imputations index
mi.idx<-us16wh$inum

#Split the File by index
for(i in 1:5) {
  #assign(filename, what)
  assign(paste("us16wh_",i,sep=""), us16wh[mi.idx==i,])
}

# combine Files to multiple imputations list
us.mi<-imputationList(list(us16wh_1,us16wh_2,us16wh_3,us16wh_4,us16wh_5))

# mi survey design object
us.svymi<-svydesign(id=~hid, weights=~hpopwgt, data=us.mi)

svymean(~dnw, design = us.svymi) #nope

#but:
with(us.svymi,svymean(~dnw))
mean.dnw.mi.c <- summary(MIcombine(mean.dnw.mi))

median.dnw.mi <- with(us.svymi, svyquantile(~dnw, quantiles = 0.5))
summary(MIcombine(median.dnw.mi))

mean(unlist(with(us.svymi, ineq.sample(x="dnw",  measure = "gini"))))
mean(unlist(with(us.svymi, ineq.sample(x="dnw", measure = "mean"))))

#with(design, fun = function)


##Using Replicate Weights
#without replicate weights
mean.dnw.mi

us16wr <- us16wr %>% replace(is.na(.), 0) %>%
  arrange(hid) %>% dplyr::select(-hid)
us16wh <- us16wh %>% 
  arrange(hid)


us.svyrw <- svrepdesign(data=us.mi,
                         weights=~hpopwgt, #sampling weights
                         repweights=us16wr, 
                         combined.weights=TRUE)


# Calculate mean of wage income
mean.dnw.rw <- withReplicates(us.svyrw,
               quote(sum(.weights*dnw)/sum(.weights)))
summary(MIcombine(mean.dnw.rw))

####By Hand
#Mean of Estimate using the rewp.weight
apply(do.call(rbind, mean.dnw.mi), 2, mean)

estimates <- data.frame()

for(i in 1:5){
  estimates[i,"mean"] <- as.data.frame(mean.dnw.rw[[i]])[,1]
}

mean(estimates$mean)

var <- data.frame()

for(i in 1:5){
 var[i,"SE"] <- as.data.frame(mean.dnw.rw[[i]])[,2]
}


#Variance
#between.var<-1/(M-1)*sum((m-mean(m))^2)
#within.var <- 1/M*sum(SE^2)

between_var <- function(m, mean, values){
  a <- 1/(m-1)
  b <- sum((values - mean(values))^2)
  return(a*b)
}

within_var <- function(m, se_vector){
  a <- 1/m*sum(se_vector^2)
  return(a)
}

var <- as.numeric(var$SE)
between.var <- 1/(5-1)*(sum((estimates$mean - mean(estimates$mean))^2))
within.var <- 1/5*(sum(var^2))
total.var<-within.var + (1+1/5)*between.var
sqrt(total.var)



####FIGURES
#Principal difficulty: Representing Sampling Weights
#Debt conditional on immigration and sex
tab.dnw <- svyby(~dnw, 
                  ~sex + educ, 
                  svymean, 
                  design = us.svymip)
barplot(tab.dnw, 
        names.arg = c("Low", "Medium", "High"))
legend("top", c("Male", "Female"), 
                fill = c("black", "gray"), ncol = 2,bty = "n")

ggplot(tab.dnw) +
  geom_bar(aes(x = educ, y = dnw), stat = "identity") + 
  facet_wrap(~sex)

tab.dnw<- melt(tab.dnw, 
     id.vars = c("sex"), 
      measure.var = c("dnw"), 
     variable.name = "group", 
     value.name = "estimate")

tab.dnw$educ <- c(rep("Low",2),rep("Medium", 2), rep("High", 2))
tab.dnw$group <- paste0(tab.dnw$sex, tab.dnw$educ)

ggplot(tab.dnw, 
       aes(x = group, 
           y = estimate, 
           fill = factor(sex), 
           color = factor(sex)))   +
  geom_bar(stat = "identity")
  


##Recode to Factors

us.svymip <- update(us.svymip, 
                    sex = factor(sex, 
                                 labels = c("Male", "Female")))

us.svymip <- update(us.svymip, 
                    educ = factor(educ, 
                                  labels = c("Low", "Medium", "High"))
                  )

#again
tab.dnw <- svyby(~dnw, 
                 ~sex + educ, 
                 svymean, 
                 design = us.svymip)


dotchart(tab.dnw, 
         main = c("Disposable Net Worth by Education and Sex"))


#Forst Plot
install.packages("rmeta")
library(meta)


#One continous variable
cdf.dnw <- svycdf(~dnw, us.svymip)
cdf.no.weight <- ecdf(us16wph$dnw)

plot(cdf.dnw, 
     do.points = FALSE, 
     xlab = "Disposable Net Worth", 
     ylab = "Cumulative Probabilty", 
     main = "CDF", xlim = c(0, 4*10^5))

lines(cdf.no.weight, do.points = FALSE, 
      lwd = 2, col = "red")
legend("bottomright", 
       bty = "n", 
       fill = c("black", "red"), 
       legend = c("Weighted", "Unweighted"))

#Boxplot- The problem of wealth variabes
svyboxplot(log(hanncv)~factor(pir), design = us.svymip)

svyboxplot(dnw~factor(pir), design = us.svymip, ylim = c(0,2*10^6))

svyhist(~dnw, design = subset(us.svymip, 
                              dnw <= 3*10^5), 
        main = "Histogram Wealth", 
        col = "gray", xlab = "Disposable Net Wealth", breaks = 100)

lines(svysmooth(~dnw, design = subset(us.svymip, 
                                       dnw <= 3*10^5), lwd = 2))

#two continous variables
svyplot(dnw ~han, 
        design = us.svymip, style = "bubble", 
        main = "Disposable Wealth and Real Asets", 
        ylab = "Real Assets", xlab = "Disposable Wealth")


