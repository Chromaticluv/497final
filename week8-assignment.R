
rm(list = ls(all.names = TRUE)) 

# Install packages and call libraries
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("ggeasy")
#install.packages("margins")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggeasy)

# Set seed to reproduce results that rely on random number generators
set.seed(42)  # You can replace 42 with any integer value

# Set directory to a folder on your machine
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 8")

# Read data into R as a new data frame
nvc <- read.table("nvc.transformed.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
 
########################################################
##  NOTE!!! outcome variable is "nvc.start.1"         ##
##      This binary variable marks the start          ##
##      of a nonviolent campaign in a country-year    ##
########################################################

# Summary number of rows
n_obs <- nvc %>% # over 8k rows in the data
  summarise(n_obs = sum(!is.na(nvc.start.1)))
n_obs 

# Omit rows with NA for baseline predictor variable
# nvc <- nvc[complete.cases(nvc$nvc.start.1), ] # no effect
# nvc <- nvc[complete.cases(nvc$wdi.pop), ] # changes the sample obs used to calculate outcome mean
 
 # Summary number of rows
 n_obs <- nvc %>% # over 8k rows in the data
   summarise(n_obs = sum(!is.na(nvc.start.1)))
 n_obs 
 
# Add columns with country age and prediction year (observation year + 1)
nvc$age <- nvc$year - nvc$yrborn
nvc$predyr <- nvc$year + 1

# Outcome mean
cutp<- mean(nvc$nvc.start.1, na.rm = TRUE)

# Cross-tabs
xtabs(~nvc.start.1 + postcoldwar, data = nvc) # by postcoldwar period
xtabs(~nvc.start.1 + nld.any.1, data = nvc) # by election year

# Some t-test: more likely during election year
t.test(nvc.start.1 ~ postcoldwar, data = nvc, var.equal = FALSE)
t.test(nvc.start.1 ~ nld.any.1, data = nvc, var.equal = FALSE)

# plot outcome by predictor
ggplot(nvc, aes(x=log(wdi.pop), y=nvc.start.1)) + 
  geom_point() +  
  theme_light() + 
  geom_smooth(method=loess) + 
  geom_hline(yintercept=0, linetype="dashed", color = "red") 

### Declare three formula for three models: outcome ~ predictor variables ###
    # Base model #
base.f <- formula(nvc.start.1 ~ log(wdi.pop))  #Population size (log)

    # Grievance model #
gripes.f <- formula(nvc.start.1 ~ log(wdi.pop) +                        # Population size (log)
                      log(xxxcimr) +                                      # Infant mortality rate rel to global median (log)
                      wdi.gdpchg.s +                                      # GDP growth rate (sq rt)
                      sqrt(wdi.cpi) +                                     # Consumer price index (sq rt)
                      log1p(bnn.yroff) +                                  # Leader's years in office (log)
                      elceleth.c +                                        # Ruling elites' ethnicity is politically salient
                      dispota4.c +                                        # Any state-led discrimination
                      cir.physint +                                       # CIRI physical integrity index
                      I(cir.physint^2) )

  # Political Opportunity model #
polopp.f <- formula(nvc.start.1 ~ log(wdi.pop) +                        # Population size (logged)
                      log1p(age) +                                        # Country age (logged)
                      postcoldwar +                                       # Post-Cold War period (1991+)
                      ios.iccpr1 +                                        # Signatory to ICCPR 1st Optional Protocol
                      nld.any.1 +                                         # Election year
                      pitfdem +                                           # PITF democracy indicator
                      I(pitfdem * nld.any.1) +                            # Election year/democracy interaction
                      I(postcoldwar * nld.any.1) +                        # Election year/post-cold war interaction
                      as.factor(fiw.cl) +                                 # FH civil liberties index (flipped & centered)
                      log1p(pol.durable) +                                # Regime durability
                      log1p(cou.tries5) )

#############################################################################

# Test models: logit/binomial estimator
m1 <- glm(base.f, data=nvc,family=binomial)
summary(m1)
m2 <- glm(gripes.f, data=nvc,family=binomial)
summary(m2)
m3 <- glm(polopp.f, data=nvc,family=binomial)
summary(m3)

# New column equal to outcome but as factor
nvc$ponset<-as.factor(nvc$nvc.start.1)

# Base model predictions 
nvc$pr1 <- predict(m1, newdata = nvc, type = "response")
p1 <- ggplot(nvc, aes(pr1,fill=ponset, na.rm = TRUE)) + 
  geom_density(alpha = 0.25, na.rm = TRUE) +
  ggtitle("Pr(Collapse|X) by Regime collapse")  + 
  xlab("Predicted probabilities of regime collapse") + 
  ylab("Density") +
  geom_vline(xintercept = mean(nvc$nvc.start.1,na.rm = TRUE), color = "red",linetype = 1)
p1
nvc$correct1<-(nvc$pr1>=cutp & nvc$nvc.start.1==1) | 
  (nvc$pr1<cutp & nvc$nvc.start.1==0)
summarize(nvc, mean(correct1,na.rm=TRUE))

 
# Grievance model predictions 
nvc$pr2 <- predict(m2, newdata = nvc, type = "response")
p2 <- ggplot(nvc, aes(pr2,fill=ponset,na.rm = TRUE)) + geom_density(alpha = 0.25) +
  ggtitle("Pr(Collapse|X) by Regime collapse")  + 
  xlab("Predicted probabilities of regime collapse") + 
  ylab("Density") +
  geom_vline(xintercept = cutp, color = "red",linetype = 1)
p2
nvc$correct2<-(nvc$pr2>=cutp & nvc$nvc.start.1==1) | 
  (nvc$pr2<cutp & nvc$nvc.start.1==0)
summarize(nvc, mean(correct2,na.rm=TRUE))

# Political opportunity model predictions 
nvc$pr3 <- predict(m3, newdata = nvc, type = "response")
p3 <- ggplot(nvc, aes(pr3,fill=ponset,na.rm = TRUE)) + geom_density(alpha = 0.25) +
  ggtitle("Pr(NVC) by observed values of campaign")  + 
  xlab("Predicted probabilities of non-violent campaign") + 
  ylab("Density") +
  geom_vline(xintercept = mean(nvc$nvc.start.1,na.rm = TRUE), color = "red",linetype = 1)
p3
nvc$correct3<-(nvc$pr3>=cutp & nvc$nvc.start.1==1) | 
  (nvc$pr3<cutp & nvc$nvc.start.1==0)
summarize(nvc, mean(correct3,na.rm=TRUE))

##########################################################
##########################################################
##########################################################
##########################################################

 