## Session B: Sample and Population Statistics
#############################
#How to transform data frames 
#############################
# while in general you can do most operations with the usual function setup
# provided by R - functionname(argument1, argument2, ...) - we will use the 
# dplyr package throughout most parts of this session. 
library(dplyr)
library(doBy)
library(tidyverse)
library(Hmisc)
library(tidyr)
library(haven)
library(acid)

# Select your working directory
setwd("/Users/hannes/Documents/Studium/Master/Third_Semester/Economics_of_distribution/seminar/coding/intro")

### Load data
#list all files in current working directory
#the idea is that if you have a lot of files to not call them each the following function helps to do it for you
exfiles <- list.files()
exfiles <- exfiles[exfiles!="intro_descr_stats.R"]
#load files
for(f in 1:length(exfiles)){
  tmp <- paste0(getwd(), "/", exfiles[f]) #creates path to the respective file
  tmp <- read_dta(tmp) #now we read the path created
  #assign filename
  name_out <- substr(exfiles[f], 1, nchar(exfiles[f])-4) #cuts of the ".dta" of the filename
  #assign filename to data frame
  assign(name_out, tmp) # assigns the new file name to the associated file
}
rm(tmp)

#bind all files by type (h, p)
hh_dat <- bind_rows(it14wh, us16wh)
indiv_dat <- bind_rows(it14wp, us16wp) #warning comes from diff of STATA application and R usage

#merge individual and hh data
lws <- merge(hh_dat, indiv_dat, 
             by = c("did", "hid", "inum"), suffixes = c("_h", "_p")) 
#for the merge always needed to merge by these 3 variabels
#e.g. country id did + hid to account for same ID of HH in different countries
#inum accounts for each imputation by each HH (multiply imputations thus 5 ids per HH)
#--> using all 3 var. to account for exact identification to merge

#Filter Data - only US data
hh_dat_us <- hh_dat %>% 
  filter(cname == "United States")


# subset the data frame such that you only have your variables of interest
hh_dat <- hh_dat %>% dplyr::select(hid, inum, cname, hpopwgt, 
                                           hhtype, 
                                           pir, cir, cia, cig, 
                                           ha, hl, dnw, anw, inw, tnw, 
                                           hanr, hanrp, hann)


#Inspecting the data set 
str(hh_dat_us) #same information as you get from the environment
# first few entries of your data
glimpse(hh_dat_us)
head(hh_dat_us) 
# number of observations and variables, also same as environment
dim(hh_dat_us)
# opens your data as tab 
#View(data.subset) 
# Rearranges your data
hh_dat_us  %>% group_by(inum) %>%
  arrange(tnw, desc(hanrp)) %>% 
  View() 

##############################################
#How to calculate sample/population statistics
#Basic Descriptive Statistics
##############################################
# Note that these population sums do not necessarily represent real world sums.
# As we only work with a shortened training data set, these errors might occur,
# due to randomness of households chosen and their respective weights.

# How many individuals are represented by the sample?
# Sum up household weights
# (Note that you get the same result by summing up individual weights. We only
# use the household weights here, because in this training data there are no
# individual level variables contained)
n.pop <- sum(hh_dat_us$hpopwgt) #number of households
n.pop #wrong --> implicates ignored!

n.indiv <- lws %>% 
  group_by(cname_p, inum) %>% 
  summarise(n = sum(ppopwgt), #ppopwgt: individual weight #n=population size
            n_na_dnw = sum(is.na(dnw))) %>%
  group_by(cname_p) %>%
  summarise(n_final = mean(n),
            n_na_dnw_final = mean(n_na_dnw))

#Note: sum of households weights across individuals --> sum of individuals
n.hh <- lws %>% 
  group_by(cname_p, inum) %>% 
  summarise(n = sum(hpopwgt)) %>%
  group_by(cname_p) %>%
  summarise(n = mean(n))

#sum of households weights across households --> sum of households
hh_dat %>% 
  group_by(cname, inum) %>% 
  summarise(n = sum(hpopwgt)) #hpopwgt: HH weights

####Rubin's Rule
#by country: Rubin's Rule
hh_dat %>% group_by(cname, inum) %>% 
    summarise(n = sum(hpopwgt)) %>%
    group_by(cname) %>%
    summarise(n = sum(n)/max(inum)) #same thing as average (sum/by max imputation)

#Household Structure
table(hh_dat$hhtype)
#household types: recoding HH types s.t. it is a lower number of types (if then logic - '|' means or)
hh_dat <- hh_dat%>% mutate(hhtype = as.character(hhtype), 
                                      hhtype = case_when(is.na(hhtype) ~ NA, 
                                      hhtype == "100" ~ "Single Person", 
                                      hhtype == "210" ~ "Couple Without Children", 
                                      hhtype == "220" ~ "Couple With Children", 
                                      hhtype == "230" ~ "Single Parent", 
                                      hhtype == "310" | hhtype == "510" | hhtype == "610" | hhtype == "910" ~ "Couple Without Children Other", 
                                      hhtype == "320" | hhtype == "520" | hhtype == "620" | hhtype == "920" ~ "Couple With Children Other",
                                      hhtype == "330" | hhtype == "530" | hhtype == "630" | hhtype == "930" ~ "Single Parent Other",
                                     #all Others
                                      TRUE ~ "Other")) 
table(hh_dat$hhtype)


#Population Size by Group Again
#by country: Rubin's Rule
#Still Correct?
hh_dat %>% group_by(cname, inum, hhtype) %>% 
  summarise(n = sum(hpopwgt)) %>%
  group_by(cname, inum, hhtype) %>% #adding HH type to get weighted ouput by HH type
  summarise(n = sum(n)/max(inum)) 

#save constant for rubin's rule (which will vary across countries in LWS)
inums <- hh_dat %>% group_by(cname, inum) %>%
  summarise(inum_max = max(inum)) %>% filter(inum_max == max(inum_max)) %>% 
  dplyr::select(inum_max)

# Combining population size and size by group calculations
inums <- hh_dat %>% 
  group_by(cname) %>% 
  summarise(inum_max = max(inum))

# Combining population size and size by group calculations -> 'complicated' version of code above
size_by_group <- hh_dat %>% 
  # Calculate Population Size by Group
  group_by(cname, inum, hhtype) %>% 
  summarise(n = sum(hpopwgt)) %>%
  left_join(inums, by = "cname") %>%
  group_by(cname, hhtype) %>%
  summarise(n_group = sum(n)/inum_max, 
            #keep inum
            inum_max = inum_max, 
            inum = inum) %>%
  # Calculate Total Population Size
  left_join(pop_size <- hh_dat %>% group_by(cname, inum) %>% 
              summarise(n = sum(hpopwgt)) %>% 
              summarise(n = sum(n)/max(inum)), 
            by = "cname") %>%
  # Final Calculations
  mutate(n_share = n_group/n) %>% 
  filter(inum == 1) %>% 
  mutate(cname, hhtype, n = round(n, 0), n_group = round(n_group, 0), n_share = round(n_share * 100, 2)) %>% 
  dplyr::select(-inum)


# Now plot the shares of each group
?ggplot2 #accessing documentation for ggplot
# or go to: https://r-graphics.org/
# the RColorBrewer package is used to generate a colour scheme
coul <- RColorBrewer::brewer.pal(8, "Set2") 
#or
devtools::install_github("karthik/wesanderson")
#better: 
#https://github.com/EmilHvitfeldt/r-color-palettes
#devtools::install_github("jrnold/ggthemes")
#install.packages("ggthemes")

# Create a barplot using ggplot2
size_by_group %>% #refering to data set
  ggplot(aes(x = hhtype, y = n_share, fill = cname)) +
  geom_bar(stat = "identity", position = "dodge") + #stat=identity --> height of bar identical to values in data
  scale_fill_brewer(palette = "Paired", name = "Country") +
  labs(y = "% of Population", x = "Household Type", title = "Shares of Household Type in Population") +
  scale_y_continuous(limits = c(0, max(size_by_group$n_share + 5))) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) # v/hjust = vertical & horizontal distance to axis

####Mean and Median Wealth by Receipt of Inheritance Across Distribution###
#Mean and Median Wealth by Household Size by Inheritance in Past Year
sum(is.na(hh_dat$pir))
#Create Factor for receiving Inheritance
hh_dat <- hh_dat %>% 
  mutate(pir_c = ifelse(pir == 0, 
                        "No Inheritance/Gift", 
                        "Inheritance Gift"),
         pir_c = factor(pir_c, levels = c("Inheritance Gift", "No Inheritance/Gift")))


#Assign Households to Groups Defined by DNW
hh_dat <- hh_dat %>% group_by(cname, inum) %>%
  mutate(
  dnw_top1_cutoff = wtd.quantile(x=dnw, probs = 0.99, weights = hpopwgt, na.rm = TRUE),
  dnw_t10_cutoff  = wtd.quantile(x=dnw, probs = 0.90, weights = hpopwgt, na.rm = TRUE), 
  dnw_min = min(dnw), 
  dnw_groups = case_when(dnw >=dnw_min & dnw < dnw_t10_cutoff ~ "Bottom 90 %", 
                         dnw >=dnw_t10_cutoff ~ "Top 10%", 
                         dnw >=dnw_top1_cutoff ~ "Top 1%")
  )
  

table(hh_dat$dnw_groups) #Problem! case_when evaluates line by line, and assigns 
#first correct case

#Thus: 
hh_dat <- hh_dat %>%
  group_by(cname, inum) %>%
  mutate(
    dnw_top1_cutoff = wtd.quantile(x=dnw, probs = 0.99, weights = hpopwgt, na.rm = TRUE),
    dnw_t10_cutoff  = wtd.quantile(x=dnw, probs = 0.90, weights = hpopwgt, na.rm = TRUE), 
    dnw_min = min(dnw), 
    dnw_groups = case_when(dnw >=dnw_top1_cutoff ~ "Top 1%", 
                           dnw >=dnw_t10_cutoff ~ "Top 10%", 
                           dnw >=dnw_min & dnw < dnw_t10_cutoff ~ "Bottom 90 %")
  ) #with case when function the order of the conditions matters (most restrictive one first)

table(hh_dat$dnw_groups) 

#Calculate some summary statistics
stats_out <- hh_dat %>% group_by(cname, inum, pir_c, dnw_groups) %>%
  summarise(Mean = wtd.mean(x = dnw, weights = hpopwgt, na.rm = TRUE),
            Median = wtd.quantile(x = dnw, weights = hpopwgt, probs = 0.5, na.rm = TRUE),
            Mean_Median_Ratio = Mean/Median, 
            Gini = round(as.numeric(weighted.gini(x = dnw, w=hpopwgt)[1]),2)) #weighted.gini: ouput is a list hence as.numeric and the brackets to get the 1 value


#Apply Rubin's Rule and aggregate
stats_out <- stats_out %>% left_join(inums) %>%
  mutate(inum = as.factor(inum)) %>%
  group_by(cname, pir_c, dnw_groups) %>%
  mutate(across(where(is.numeric), ~ sum(.)/inum_max, .names = "{col}_rubin")) %>%
  filter(inum == "1") %>% dplyr::select(-inum_max_rubin, -inum, -inum_max)

#Better Round afterwards!
stats_out %>%
  mutate(across(c(Mean_Median_Ratio, Gini), ~ round(., digits = 2)))

#For Plotting: Use Wide Format (easier to handle in ggplot2) --
stats_out <- stats_out %>%
  pivot_longer(
    cols = -c(cname, pir_c, dnw_groups),  # Exclude key variables from pivoting
    names_to = "statistic",  # Name of the new column for variable names
    values_to = "value"  # Name of the new column for values
  )


#Wealth by Wealth Groups
#Either Arrange within plot, or first create factor with correct order
stats_out$group <- paste0(stats_out$pir_c, 
                          " - ", 
                          stats_out$dnw_groups)

stats_out$group = factor(stats_out$group,
                         levels = c("Inheritance Gift - Bottom 90 %", 
                                    "No Inheritance/Gift - Bottom 90 %", 
                                    "Inheritance Gift - Top 10%", 
                                    "No Inheritance/Gift - Top 10%", 
                                    "Inheritance Gift - Top 1%", 
                                    "No Inheritance/Gift - Top 1%"))

plot <- stats_out %>%
  filter(statistic %in% c("Mean_rubin", "Median_rubin")) %>%
  ggplot(aes(x = statistic, y = value, 
             fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  guides(color = guide_legend(title = "Mean vs. Median")) +
  labs(y = "Value", x = "Group", 
       title = "Mean and Median Wealth by Group", fill = "Group") +
  scale_fill_manual(values = c("#BC5561", "#DEA3AA", 
                    "#456B9E", "#859CBB", "#3A6641", "#85BB8D")) +
  scale_x_discrete(labels = c("Mean", "Median")) +
  theme_minimal()
plot

#Or two panels next to each other
require(gridExtra)
plot1 <- stats_out %>%
  filter(statistic %in% c("Mean_rubin")) %>%
  ggplot(aes(x = statistic, y = value, 
             fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  guides(color = guide_legend(title = "Mean ")) +
  labs(y = "Value", x = "Group", 
       title = "Mean Wealth by Group", fill = "Group") +
  scale_fill_manual(values = c("#BC5561", "#DEA3AA", 
                               "#456B9E", "#859CBB", "#3A6641", "#85BB8D")) +
  scale_x_discrete(labels = c("Mean")) +
  theme_minimal() +
 
  theme ( legend.position="none")
plot2 <- stats_out %>%
  filter(statistic %in% c("Median_rubin")) %>%
  ggplot(aes(x = statistic, y = value, 
             fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  guides(color = guide_legend(title = "Median ")) +
  labs(y = "Value", x = "Group", 
       title = "Median Wealth by Group", fill = "Group") +
  scale_fill_manual(values = c("#BC5561", "#DEA3AA", 
                               "#456B9E", "#859CBB", "#3A6641", "#85BB8D")) +
  scale_x_discrete(labels = c("Mean")) +
  theme_minimal() 

grid.arrange(plot1, plot2, ncol=2)

setwd("..")

ggsave("./plots/by_group.png") 

###Assign Households to Deciles / Quantiles
#Group Households Into Deciles: How can we do that given the implicates?
#Create a fixed quantile for an observation!
#### * quantiles and cdf ####
genCDF<-function(x,w) spatstat.geom::ewcdf(x=x,weights = w)(x) #calculates cdf for HH
cutCDF<-function(x,w,q) cut(genCDF(x,w),breaks=seq(0,1,1/q),labels = as.character(1:q)) #to introduce quantiles (q)

#assign by cname!
hh_dat <- hh_dat %>% group_by(cname) %>% 
  mutate(deciles = cutCDF(x=dnw, w = hpopwgt, q=10), #q=10 for deciles
         percentiles = cutCDF(x = dnw, w = hpopwgt, q = 100)) 

#calculate statistic by decile
perc_means <- hh_dat %>% group_by(cname, inum, percentiles) %>% 
  summarise(mean_decile = wtd.mean(x = dnw, weights = hpopwgt,na.rm = TRUE))

#plot
perc_means %>%
  filter(cname =="Italy") %>%
  ggplot(aes(x = percentiles, y = mean_decile, 
             fill = "#BC5561")) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  guides(fill = guide_legend(title = "")) +
  scale_y_continuous(breaks = c(500000, 1000000, 2000000, 3000000), 
                     labels = c("500k", "1Mio", "2Mio", "3Mio")) +
  labs(y = "", x = "Percentiles", 
       title = "Mean by Percentile - Italy") +
  theme(axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(), 
  legend.position = "none") 


#####Weighted Density Plot
#weighted density plot showing the distribution 
ihs<-function(x) log(x + (x^2+1)^0.5)
lws %>% filter(cname_p == "United States") %>%
  filter(!is.na(sex)) %>%
ggplot(aes(ihs(dnw)*hpopwgt, fill = factor(sex))) +
  geom_density(aes(weight = ppopwgt/5), alpha = 0.2) +
  theme_minimal() +
  scale_fill_discrete(name = "Gender", 
                      labels = c("Men", "Women")) +
  labs(x = "DNW", 
       y = "Density") + 
  theme_minimal()

##IHS vs Log
myvec <- seq(-100,1000, 10)
plot(myvec)
plot(ihs(myvec), col = "red")
lines(log(myvec), col = "green", lwd = 2)

##Write your own functions!
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


a <- us16wh %>% group_by(cname, inum) %>%
  summarise(mean_dnw = weighted.mean.ud(x =dnw, w=hpopwgt),
            median_dnw = weighted.median.ud(x=dnw, w=hpopwgt),
            p90 = weighted.quantile.ud(x = dnw, w = hpopwgt, quant = 0.9),
            pop = sum(hpopwgt), 
            sample = n()) %>%
  summarise_all(mean) %>%
  mutate(type = "weighted.ud")

#compare to package
b <- us16wh %>% group_by(cname, inum) %>%
  summarise(mean_dnw = weighted.mean(x = dnw, w = hpopwgt),
            median_dnw = spatstat.geom::weighted.quantile(dnw, hpopwgt,probs = 0.5), 
            p90 = spatstat.geom::weighted.quantile(dnw, hpopwgt, probs = 0.9, type = 4),
            pop = sum(hpopwgt), 
            sample = n()) %>%
  summarise_all(mean) %>%
  mutate(type = "weighted_hmisc")
bind_rows(a,b)

###Shares and Share Ratios

###Asset Composition Along the Wealth Distribution





