##########INTRODUCING THE SURVEY PACKAGE LOCALLY
library(tidyverse)
library(Hmisc)
library(tidyr)
library(haven)
library(acid)
library(survey)
install.packages("readstata13")
library(readstata13)
library(mitools)
library(convey)
### Select your working directory
setwd("/Users/hannes/Documents/Studium/Master/Third_Semester/Economics_of_distribution/seminar/coding/data")

### Load data
#list all files in current working directory
exfiles <- list.files()
exfiles <- exfiles[exfiles!="plots"]
exfiles <- exfiles[exfiles!="unneeded_file.R"]

#load files
for(f in 1:length(exfiles)){
  tmp <- paste0(getwd(), "/", exfiles[f])
  tmp <- haven::read_dta(tmp, 
                         encoding = NULL,
                         col_select = NULL,
                         skip = 0,
                         n_max = Inf,
                         .name_repair = "unique")
  #assign filename
  name_out <- substr(exfiles[f], 1, nchar(exfiles[f])-4)
  #assign filename to data frame
  assign(name_out, tmp)
}
rm(tmp)


#bind all files by type (h, p)
hh_dat <- bind_rows(it14wh, us16wh)
indiv_dat <- bind_rows(it14wp, us16wp)

#merge individual and hh data
lws <- merge(hh_dat, indiv_dat, 
             by = c("did", "hid", "inum"), 
             suffixes = c("_h", "_p"))


################THE SURVEY PACKAGE: Treats population weights, implicates, and rep weights

###Start with US Data only

#Survey Design 
us.svymi<-svydesign(ids=us16wh$hid, 
                    weights=~hpopwgt, 
                    data=us16wh)
us.svymip <- svydesign(ids = us16wp$pid, 
                       weights = ~ppopwgt, 
                       data = us16wp)

svymean(~dnw + anw, us.svymi)

svyquantile(~dnw, 
            design = us.svymi, 
            quantiles = seq(0,1,0.1),
            ci = TRUE)
setwd("../")
#Read code for inequality functions
source("./codes/locally/ineq_measures_svy.R") #very important for 
#Survey Packages has many built-in functions, above code adds some inequality measuers
ineq.sample(x="dnw", 
            design = us.svymi, 
            measure = "gini")


#By Factor Level / Dummies
noI <- subset(us.svymi, pir==0)
yesI <- subset(us.svymi, pir==1)

no_res <- svymean(~dnw + han + haf + hlr + hln + pia1, noI)
no_res$pir_c <- "NOT Received"
do.call(rbind, no_res)
yes_res <- svymean(~dnw + han + haf + hlr + hln + pia1, yesI)
yes_res$pir_c <- "Received"
do.call(rbind, yes_res)

out <- bind_rows(yes_res, no_res)

#Svyby quite useful (by could by country!) --> Like Group By in Dplyr
svyby(~dnw,
      ~hhtype+pir,
      svymean, design = us.svymi)


####Now, let's add the multiple imputations again

###Start with the Raw Data
mi.idx<-us16wh$inum
#Just an index for each row that goes from 1:5

#Split the File by index
for(i in 1:5) {
  #assign(filename, what)
  assign(paste("us16wh_",i,sep=""), 
         us16wh[mi.idx==i,])
}

# combine Files to multiple imputations list
us.mi<-mitools::imputationList(list(us16wh_1,
                                    us16wh_2,
                                    us16wh_3,
                                    us16wh_4,
                                    us16wh_5))
class(us.mi)

# mi survey design object: data now the list
us.svymi<-svydesign(id=~hid, 
                    weights=~hpopwgt, 
                    data=us.mi)

svymean(~dnw, design = us.svymi) #nope

#but:
with(us.svymi,svymean(~dnw))

#improve format
mean.dnw.mi <- with(us.svymi,svymean(~dnw))
mean.dnw.mi.c <- summary(MIcombine(mean.dnw.mi))
#what does missInfo mean???

median.dnw.mi <- with(us.svymi, 
                      svyquantile(~dnw, 
                                  quantiles = 0.5))
summary(MIcombine(median.dnw.mi))
mean.dnw.mi <- with(us.svymi, svymean(~dnw))
save_for_comp_mean <- summary(MIcombine(mean.dnw.mi))

#be careful with functions from function
mean(unlist(with(us.svymi, 
                 ineq.sample(x="dnw",  measure = "gini"))))
mean(unlist(with(us.svymi, 
                 ineq.sample(x="dnw", measure = "mean"))))
#Regression:
with(us.svymi,svyglm(pir~dnw+hhtype, family = quasibinomial))

#Finally - Adding Replication Weights

##Using Replicate Weights
mean.dnw.mi

us16wr <- us16wr %>% replace(is.na(.), 0) %>%
  arrange(hid) %>% dplyr::select(-hid)

us16wh <- us16wh %>% 
  arrange(hid)

#recall: id specified in survey design
us.svyrw <- svrepdesign(data=us.mi, 
                        id = ~hid,
                        weights=~hpopwgt, 
                        repweights=us16wr[, -1], #exclude first col with id
                        scale = 1 ,
                        rscales = rep( 1 / 998 , 999 ) , #if using full set
                        type = "other",
                        combined.weights=TRUE)


#you can also specify a subset of the replication weights using
#a regular expression and just using a subset of them. 
#Example: repweights="hrgwgt[1-499]+", then repweghts whould be part
#of the dataset (in us.mi) rather than in a single file!!!!


# Calculate mean of dnw - note that the weights are
mean.dnw.rw <- withReplicates(us.svyrw,
                              quote(sum(.weights*dnw)/sum(.weights)))



#same point estimate as before!
mean(unlist(with(us.svymi, ineq.sample(x="dnw", 
                                       measure = "mean"))))
#what about SE?
save_for_comp_mean 

####Regression SEs: MI + Replicates
us.svy<-convey_prep(us.svyrw)
MIcombine(with(us.svy, svyglm(pir~dnw+hhtype)))
example_reg<-MIcombine(with(scf.svy, 
                      svyglm(pir~dnw, 
                             family=quasibinomial(link='probit'))))  
summary(first)
fist$coefficients


MIcombine(with(scf.svy, svygini(~dnw)))


#We can subset etc. 
#subset
MIcombine(with(subset(us.svy, pir==0), svygini(~dnw)))
#Update designs
us.svy<-update(us.svy, mio=1*(dnw>1000000))
sum<-MIcombine(with(us.svy, sum(mio)))
us.svy<-update(us.svy, count=1)
coef(MIcombine(with(subset(us.svy, mio==1), 
                    svyby(~count, by=~pir, svytotal))))
coef(MIcombine(with(subset(us.svy, mio==1), 
                    svyby(~dnw, by=~pir, svymean))))

###Group Assignments 
(with(us.svy, svyquantile(~dnw, seq(0,1,0.01)))) 
#Calculate Full CDF
cdfs<-with(us.svy, svycdf(~dnw)) 
#list of 5
cdfs[[1]] #1st Implicate
cdfs[[1]][[1]](1000) #at position
cdfs[[1]][[1]](c(100,10000)) #at positions
#now generalize: 
us.svy<-update(us.svy, 
               cdf_wealth=cdfs[[5]][[1]](dnw))
#hidden, but there: 
us.svy[["designs"]][[1]][["variables"]]$cdf_wealth
#aggregate to percentiles
us.svy<-update(us.svy, cdf_wealth100=ceiling(cdf_wealth*100))
us.svy[["designs"]][[1]][["variables"]]$cdf_wealth100
#and cutt
us.svy<-update(us.svy, cdf_wealth.class=cut(cdf_wealth,c(0,0.5,0.9,0.99,1)))
us.svy<-update(us.svy, cdf_wealth.class2=cut(cdf_wealth, seq(0,1,0.1)))
us.svy[["designs"]][[1]][["variables"]]$cdf_wealth.class2

##svy very cool since possible to introduce easily survey data and adopt it for probit model
###svyglm()
