rm(list = ls(all.names = TRUE)) 

# Install packages and call libraries
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("simstudy")
# install.packages("modnets")
# install.packages("ggtrendline")
# install.packages("miceadds")
# install.packages("ggeasy")

library(simstudy)
library(modnets)
library(dplyr)
library(ggplot2)
library(ggtrendline)
library(miceadds)
library(ggeasy)

# Set directory to a folder on your machine
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 4")

# Read data into R as a new data frame
gwf <-read.csv("gwf2.csv")

# Tabulate values of milit (military rank variable)
tabulate(gwf$militrank+1)  # note  that 0 is the lowest value so +1 to tabulate 0 1 2 3 4

# Generate variables
gwf <- gwf %>%
  mutate(
    jrofficer = ifelse(militrank >= 1 & militrank <= 2, 1, 0),
    srofficer = ifelse(militrank >= 3 & militrank <= 4, 1, 0),
    ld = log(gwf_duration),
  )

##### Democratic transition t-test and difference-of-means plots #######
# T-test and confidence intervals
t_test_results <- t.test(gwf$gwf_dem ~ gwf$jrofficer)
m1 <- t_test_results$estimate[1]
m2 <- t_test_results$estimate[2]
t_test_results <- t.test(gwf$gwf_dem ~ gwf$srofficer)
m1 <- t_test_results$estimate[1]
m2 <- t_test_results$estimate[2]

# Plots 
par(mfrow=c(1,2))  # next 2 plots combined into 1 row in a single plot
# jr officer
# m, sd, and n are data objects with means, standard deviations and number of observations for gwf_failure in each group (coldwar and notcoldwar)
m <- aggregate(gwf_dem ~ jrofficer,data=gwf, function(x) c(mean = mean(x)))
sd <- aggregate(gwf_dem ~ jrofficer,data=gwf, function(x) c(sd = sd(x)))
n <- aggregate(gwf_dem ~ jrofficer,data=gwf, function(x) c(n = length(x)))
mx<-c(m$gwf[1],m$gwf[2])  # new data object that just contains the mean values
names(m) = c("Other \n leaders", "Junior \n officer")  # names for the bars in m
# Next command calculates the standard errors from the standard deviations
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mx, ylim=c(0, .06), xpd=FALSE,
              ylab = "Probability of democratic transition", 
              xlab = " ",
              cex.axis=.75,
              cex.lab=.75,
              cex.names=.75,
              main="Junior officers",
              names.arg=names(m))
arrows(x0=bp, y0=mx-se, y1=mx+se,code=0, angle=90 )  # arrows provides the CI on top of the bar
# sr officer plot
m <- aggregate(gwf_dem ~ srofficer,data=gwf, function(x) c(mean = mean(x)))
sd <- aggregate(gwf_dem ~ srofficer,data=gwf, function(x) c(sd = sd(x)))
n <- aggregate(gwf_dem ~ srofficer,data=gwf, function(x) c(n = length(x)))
mx<-c(m$gwf[1],m$gwf[2])  # new data object that just contains the mean values
names(m) = c("Other \n leaders", "Senior \n officer")  # names for the bars in m
# Next command calculates the standard errors from the standard deviations
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mx, ylim=c(0, .06), xpd=FALSE,
              ylab = "Probability of regime collapse", 
              xlab = " ",
              cex.axis=.75,
              cex.lab=.75,
              cex.names=.75,
              main="Senior officers",
              names.arg=names(m))
arrows(x0=bp, y0=mx-se, y1=mx+se,code=0, angle=90 )  # arrows provides the CI on top of the bar


##### Transition to new dictatorship t-test and difference-of-means plots #######
# T-test and confidence intervals
t_test_results <- t.test(gwf$gwf_dict ~ gwf$jrofficer)
m1 <- t_test_results$estimate[1]
m2 <- t_test_results$estimate[2]
t_test_results <- t.test(gwf$gwf_dict ~ gwf$srofficer)
m1 <- t_test_results$estimate[1]
m2 <- t_test_results$estimate[2]

# Plots 
par(mfrow=c(1,2))  # next 2 plots combined into 1 row in a single plot
# jr officer 
# m, sd, and n are data objects with means, standard deviations and number of observations for gwf_failure in each group (coldwar and notcoldwar)
m <- aggregate(gwf_dict ~ jrofficer,data=gwf, function(x) c(mean = mean(x)))
sd <- aggregate(gwf_dict ~ jrofficer,data=gwf, function(x) c(sd = sd(x)))
n <- aggregate(gwf_dict ~ jrofficer,data=gwf, function(x) c(n = length(x)))
mx<-c(m$gwf[1],m$gwf[2])  # new data object that just contains the mean values
names(m) = c("Other \n leaders", "Junior \n officer")  # names for the bars in m
# Next command calculates the standard errors from the standard deviations
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mx, ylim=c(0, .06), xpd=FALSE,
              ylab = "Probability of transition to new dictatorship", 
              xlab = " ",
              cex.axis=.75,
              cex.lab=.75,
              cex.names=.75,
              main="Junior officers",
              names.arg=names(m))
arrows(x0=bp, y0=mx-se, y1=mx+se,code=0, angle=90 )  # arrows provides the CI on top of the bar
# sr officer
m <- aggregate(gwf_dem ~ srofficer,data=gwf, function(x) c(mean = mean(x)))
sd <- aggregate(gwf_dem ~ srofficer,data=gwf, function(x) c(sd = sd(x)))
n <- aggregate(gwf_dem ~ srofficer,data=gwf, function(x) c(n = length(x)))
mx<-c(m$gwf[1],m$gwf[2])  # new data object that just contains the mean values
names(m) = c("Other \n leaders", "Senior \n officer")  # names for the bars in m
# Next command calculates the standard errors from the standard deviations
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mx, ylim=c(0, .06), xpd=FALSE,
              ylab = "Probability of transition to new dictatorship", 
              xlab = " ",
              cex.axis=.75,
              cex.lab=.75,
              cex.names=.75,
              main="Senior officers",
              names.arg=names(m))
arrows(x0=bp, y0=mx-se, y1=mx+se,code=0, angle=90 )  # arrows provides the CI on top of the bar



#####  two models, one for each outcome ####
model1<-lm.cluster(gwf_dem~jrofficer + srofficer + ld,  cluster = 'gwf_casename', data=gwf,)
model2<-lm.cluster(gwf_dict~jrofficer + srofficer + ld,  cluster = 'gwf_casename', data=gwf,)

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)),
                          Coefficient = summary(model1)[, 1],
                          SE = summary(model1)[, 2],
                          Transition = "to democracy")
model2Frame <- data.frame(Variable = rownames(summary(model2)),
                          Coefficient = summary(model2)[, 1],
                          SE = summary(model2)[, 2],
                          Transition = "to new dictatorship")
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
zp1 <- zp1 + ggtitle("Military leaders and \n types of autocratic regime collapse") + ggeasy::easy_center_title()
zp1 <- zp1 + scale_x_discrete(labels = c('(Intercept)','Jr officer','Duration (log)', 'Sr officer'))
print(zp1) 


