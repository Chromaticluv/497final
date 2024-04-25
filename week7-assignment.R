rm(list = ls(all.names = TRUE)) 

# Install packages and call libraries
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("plm")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("fixest")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeasy)
library(plm)
library(lmtest)
library(sandwich)
library(fixest)

# Set directory to a folder on your machine
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 7")

# Read data into R as a new data frame
gwf <-read.csv("gwf-wdi.csv")

# omit rows with missing data on a covariate, "wdi_oilrents"
gwf<-gwf[!(is.na(gwf$wdi_oilrents)),]

# omit rows with missing data on a covariate, "wdi_gdp_pc"
gwf<-gwf[!(is.na(gwf$wdi_gdp_pc)),]

# Temporal domain of the data
summarise(gwf, min(gwf$year,na.rm=TRUE))
summarise(gwf, max(gwf$year,na.rm=TRUE))
 
# Generate log transformations of skewed variables
gwf <- gwf %>%
  mutate(
    lpop =log(wdi_population),
    loil =(log(1+wdi_oilrents)),
    lgdppc = log(wdi_gdp_pc),
    ld = log(gwf_duration),
  )

 

#######################  Homework #########################

# Transition to Democracy
# OLS and clustered errors
m1<-lm(gwf_dem~ loil, data=gwf,)
coeftest(m1,vcov = vcovCL, type = "HC1",cluster = ~cowcode) # set the model to "m1" & set the cluster variable to "cowcode"
# OLS with covariates and clustered errors
m2<-lm(gwf_dem ~  ld + lpop + lgdppc + gwf_jrofficer + loil, data=gwf,)
coeftest(m2,vcov = vcovCL, type = "HC1",cluster = ~cowcode) # set the model to "m1" & set the cluster variable to "cowcode"
# two-way FE and clustered errors
m3<-plm(gwf_dem~ld + lgdppc + lpop + loil  + gwf_jrofficer, 
         data = gwf,
         index = c("cowcode", "year"), 
         model =  "within", effect = "twoways")
coeftest(m3, vcovHC(m3, type = 'HC1', cluster = 'group'))

# Transition to Dictatorship
# OLS and clustered errors
m4<-lm(gwf_dict~ loil, data=gwf,)
coeftest(m4,vcov = vcovCL, type = "HC1",cluster = ~cowcode) # set the model to "m1" & set the cluster variable to "cowcode"
# OLS with covariates and clustered errors
m5<-lm(gwf_dict~ld + lpop + lgdppc + gwf_jrofficer + loil, data=gwf,)
coeftest(m5,vcov = vcovCL, type = "HC1",cluster = ~cowcode) # set the model to "m1" & set the cluster variable to "cowcode"
# two-way FE with covariates and clustered errors
m6<-plm(gwf_dict~ld + lgdppc + lpop + loil  + gwf_jrofficer, 
        data = gwf,
        index = c("cowcode", "year"), 
        model =  "within", effect = "twoways")
coeftest(m6, vcovHC(m6, type = 'HC0', cluster = 'group'))

# Capture model summary frames
c1<-summary(m1)
c2<-summary(m2)
c3<-summary(m3)
c4<-summary(m4)
c5<-summary(m5)
c6<-summary(m6)
 
# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(c1$coefficients),
                          Coefficient = c1$coefficients[ , 1],
                          SE = c1$coefficients[, 2],
                          Model = "Bivariate OLS")
model2Frame <- data.frame(Variable = rownames(c2$coefficients),
                          Coefficient = c2$coefficients[ , 1],
                          SE = c2$coefficients[, 2],
                          Model = "Covariates, OLS")
model3Frame <- data.frame(Variable = rownames(c3$coefficients),
                          Coefficient = c3$coefficients[ , 1],
                          SE = c3$coefficients[, 2],
                          Model = "Covariates, FE")
model4Frame <- data.frame(Variable = rownames(c4$coefficients),
                          Coefficient = c4$coefficients[ , 1],
                          SE = c4$coefficients[, 2],
                          Model = "Bivariate OLS")
model5Frame <- data.frame(Variable = rownames(c5$coefficients),
                          Coefficient = c5$coefficients[ , 1],
                          SE = c5$coefficients[, 2],
                          Model = "Covariates, OLS")
model6Frame <- data.frame(Variable = rownames(c6$coefficients),
                          Coefficient = c6$coefficients[ , 1],
                          SE = c6$coefficients[, 2],
                          Model = "Covariates, FE")
# Combine these data.frames
DemModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame))  # etc.
DictModelFrame <- data.frame(rbind(model4Frame, model5Frame, model6Frame))  # etc.

# Keep only the oil predictor variable 
DemModelFrame<-filter(DemModelFrame, Variable == "loil")
DictModelFrame<-filter(DictModelFrame, Variable == "loil")

# Plot: position=position_dodge(width = 1/2) staggers the estimates around the whole number on the axis
DemModelFrame$Variable <- factor(DemModelFrame$Model, levels = c("Bivariate OLS", "Covariates, OLS", "Covariates, FE"))
demplot <- ggplot(DemModelFrame, aes(colour = Model))
demplot <- demplot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
demplot <- demplot + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*1.65,
                                ymax = Coefficient + SE*1.65),
                            lwd = 1, position = position_dodge(width = 1/2))
demplot <- demplot + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*1.96,
                                 ymax = Coefficient + SE*1.96),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE",  )
demplot <- demplot + theme_bw() + theme(legend.position = "right")
demplot <- demplot + ggtitle("Transition to democracy") + ggeasy::easy_center_title()
demplot <- demplot + xlab("Model") + ylab("Oil rents \n coefficient estimate")
print(demplot)

# Plot: position=position_dodge(width = 1/2) staggers the estimates around the whole number on the axis
DictModelFrame$Variable <- factor(DictModelFrame$Model, levels = c("Bivariate OLS", "Covariates, OLS", "Covariates, FE"))
dictplot <- ggplot(DictModelFrame, aes(colour = Model))
dictplot <- dictplot + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
dictplot <- dictplot + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*1.65,
                                ymax = Coefficient + SE*1.65),
                            lwd = 1, position = position_dodge(width = 1/2))
dictplot <- dictplot + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*1.96,
                                 ymax = Coefficient + SE*1.96),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE",  )
dictplot <- dictplot + theme_bw() + theme(legend.position = "right")
dictplot <- dictplot + ggtitle("Transition to new dictatorship") + ggeasy::easy_center_title()
dictplot <- dictplot + xlab("Model") + ylab("Oil rents \n coefficient estimate")
print(dictplot)