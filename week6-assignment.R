rm(list = ls(all.names = TRUE)) 

# Install packages and call libraries
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages(c("survival", "survminer"))
#install.packages("miceadds")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeasy)
library(miceadds)

# Set directory to a folder on your machine
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 6")

# Read data into R as a new data frame
gwf <-read.csv("gwf2.csv")

# Generate variables
gwf <- gwf %>%
  mutate(
    jrofficer = ifelse(militrank >= 1 & militrank <= 2, 1, 0),
    srofficer = ifelse(militrank >= 3 & militrank <= 4, 1, 0),
    ld = log(gwf_duration),
  )


### RMSEs for transition to democracy 
m1<-lm(gwf_dem~poly(gwf_duration, 1, raw = TRUE) + srofficer + jrofficer + supportparty, data=gwf,)
sqrt(mean(m1$residuals^2))
m2<-lm(gwf_dem~poly(ld, 1, raw = TRUE) + srofficer + jrofficer + supportparty, data=gwf,)
sqrt(mean(m2$residuals^2))
m3<-lm(gwf_dem~poly(gwf_duration, 3, raw = TRUE) + srofficer + jrofficer + supportparty, data=gwf,)
sqrt(mean(m3$residuals^2))
m4<-lm(gwf_dem~poly(ld, 3, raw = TRUE) + srofficer + jrofficer + supportparty, data=gwf,)
sqrt(mean(m4$residuals^2))


### RMSEs for transition to new dictatorship 
m1<-lm(gwf_dict~poly(gwf_duration, 1, raw = TRUE) + srofficer + jrofficer + supportparty, data=gwf,)
sqrt(mean(m1$residuals^2))
m2<-lm(gwf_dict~poly(ld, 1, raw = TRUE) + srofficer + jrofficer + supportparty, data=gwf,)
sqrt(mean(m2$residuals^2))
m3<-lm(gwf_dict~poly(gwf_duration, 3, raw = TRUE) + srofficer + jrofficer + supportparty, data=gwf,)
sqrt(mean(m3$residuals^2))
m4<-lm(gwf_dict~poly(ld, 3, raw = TRUE) + srofficer + jrofficer + supportparty, data=gwf,)
sqrt(mean(m4$residuals^2))

 
#####  two models, one for each outcome ####
model1<-lm.cluster(gwf_dem ~ jrofficer + srofficer + supportparty + 
                  poly(ld, 3, raw = TRUE),  
                  cluster = 'gwf_casename', 
                  data=gwf,)
model2<-lm.cluster(gwf_dict ~ jrofficer + srofficer + supportparty + 
                     poly(ld, 3, raw = TRUE),  
                   cluster = 'gwf_casename', 
                   data=gwf,)
# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)),
                          Coefficient = summary(model1)[, 1],
                          SE = summary(model1)[, 2],
                          Transition = "to democracy")

model1Frame <- model1Frame %>%  filter(!row_number() %in% c(1))
model2Frame <- data.frame(Variable = rownames(summary(model2)),
                          Coefficient = summary(model2)[, 1],
                          SE = summary(model2)[, 2],
                          Transition = "to new dictatorship")
model2Frame <- model2Frame %>%  filter(!row_number() %in% c(1))

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame))  # etc.
  
# Plot: position=position_dodge(width = 1/2) staggers the estimates around the whole number on the axis
zp1 <- ggplot(allModelFrame, aes(colour = Transition))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*1.65,  # multiply SE by 1.65 for 90% confidence interval
                                ymax = Coefficient + SE*1.65),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*1.96,
                                 ymax = Coefficient + SE*1.96),  # multiply SE by 1.96 for 95% confidence interval
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE",  )
zp1 <- zp1 + theme_bw() +  theme(legend.position = "bottom")
zp1 <- zp1 + ggtitle("Time polynomials and \n types of autocratic regime collapse") + ggeasy::easy_center_title()
zp1 <- zp1 + scale_x_discrete(labels = c('Jr officer', 'Time', 'Time^(2)', 'Time^(3)',
                                         'Sr officer', 'Support party' ))
print(zp1) 
 


 



