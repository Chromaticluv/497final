rm(list = ls(all.names = TRUE)) 

# Install packages and call libraries
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("simstudy")
# install.packages("modnets")
# install.packages("ggtrendline")
library(simstudy)
library(modnets)
library(dplyr)
library(ggplot2)
library(ggtrendline)

# Set directory to a folder on your machine
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 2")

# Read data into R as a new data frame
gwf <-read.csv("gwf1.csv")

# Look at data set
# View(gwf)

# Summarize supportparty and then plot supportparty by year
summarize(gwf, mean(supportparty, na.rm=TRUE))
p<-ggplot(gwf, aes(x=year, y=supportparty)) + 
  theme_light() + geom_smooth(method=loess)   
p + ggtitle("Share of dictatorships with supporting political parties") +
  xlab("Year") + ylab("Share with support parties")

# Table by supportparty with mean values for gwf_failure
table_resultA <- with(gwf, tapply(gwf_failure, supportparty, mean))
table_resultA

# Table by supportparty with mean values for gwf_dem and gwf_dict
table_resultDEM <- with(gwf, tapply(gwf_dem, supportparty, mean))
table_resultDEM

# Table by supportparty with mean values for gwf_dem and gwf_dict
table_resultDICT <- with(gwf, tapply(gwf_dict, supportparty, mean))
table_resultDICT

# T-tests for three outcomes
test1<-t.test(gwf_failure ~ supportparty, data = gwf, var.equal = FALSE)
test1
test2<-t.test(gwf_dem ~ supportparty, data = gwf, var.equal = FALSE)
test2
test3<-t.test(gwf_dict ~ supportparty, data = gwf, var.equal = FALSE)
test3


######## Plot mean probability of failure type with CIs #######
par(mfrow=c(1,3))  # next 3 plots combined into 1 row in a single figure

# All types of collapse
m <- aggregate(gwf_failure ~ supportparty,data=gwf, function(x) c(mean = mean(x)))
sd <- aggregate(gwf_failure ~ supportparty,data=gwf, function(x) c(sd = sd(x)))
n <- aggregate(gwf_failure ~ supportparty,data=gwf, function(x) c(n = length(x)))
mean<-c(m$gwf[1],m$gwf[2])
names(m) = c("No support \n party", "Support \n party")
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mean, ylim=c(0, .085), xpd=FALSE,
              ylab = "Collapse probability", 
              xlab = " ",
              main="Autocratic regime \n collapse",
              names.arg=names(m))
arrows(x0=bp, y0=mean-se, y1=mean+se,code=0, angle=90 )


# Transition to democracy
m <- aggregate(gwf_dem ~ supportparty,data=gwf, function(x) c(mean = mean(x))) 
sd <- aggregate(gwf_dem ~ supportparty,data=gwf, function(x) c(sd = sd(x)))
n <- aggregate(gwf_dem ~ supportparty,data=gwf, function(x) c(n = length(x)))
mean<-c(m$gwf[1],m$gwf[2])
names(m) = c("No support \n party", "Support \n party")
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mean, ylim=c(0, .085), xpd=FALSE,
              ylab = "Democratic transition probability", 
              xlab = " ",
              main="Transition to \n Democracy",
              names.arg=names(m))
arrows(x0=bp, y0=mean-se, y1=mean+se,code=0, angle=90 )


# Transition to new dictatorship
m <- aggregate(gwf_dict ~ supportparty,data=gwf, function(x) c(mean = mean(x)))
sd <- aggregate(gwf_dict ~ supportparty,data=gwf, function(x) c(sd = sd(x)))
n <- aggregate(gwf_dict ~ supportparty,data=gwf, function(x) c(n = length(x)))
mean<-c(m$gwf[1],m$gwf[2])
names(m) = c("No support \n party", "Support \n party")
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mean, ylim=c(0, .085), xpd=FALSE,
              ylab = "New dictatorship transition probability", 
              xlab = " ",
              main="Transition to \n new dictatorship",
              names.arg=names(m))
arrows(x0=bp, y0=mean-se, y1=mean+se,code=0, angle=90)