rm(list = ls(all.names = TRUE)) 

# Install packages and call libraries
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("geosphere")
#install.packages("maps")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeasy)
library(ggmap)
library(geosphere)
library(maps)

# Set directory to a folder on your machine
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 5")

# Read data into R as a new data frame
coups <-read.csv("coups1.csv")

# Subset data for non-democracies only
coups<-subset(coups, gwf_democracy==0 & gwf_nondem==1)
summarize(coups, mean(gwf_democracy, na.rm=TRUE))
summarize(coups, mean(gwf_nondem, na.rm=TRUE))

# Generate variables
coups <- coups %>%
  mutate(
    failed.coup = ifelse(cwc_anyacoup == 1 & cwc_anyscoup==0, 1, 0),
    success.coup = ifelse(cwc_anyacoup == 1 & cwc_anyscoup==1, 1, 0),
  )
coup.table<-table(coups$failed.coup, coups$success.coup)
coup.table

# Generate country-counts
p1<-aggregate(coups$failed.coup, list(coups$gwf_country), FUN=sum) 
colnames(p1) <- c('gwf_country', 'total.fail.coups')
p2<-aggregate(coups$success.coup, list(coups$gwf_country), FUN=sum) 
colnames(p2) <- c('gwf_country', 'total.success.coups')
p<-cbind(p1,p2$total.success.coups)
colnames(p) <- c('gwf_country', 'total.fail.coups' , 'total.success.coups')

# Look at p
View(p)

# Fix country names for merging with map data
p$gwf_country[p$gwf_country == "United States"] <- "USA"
p$gwf_country[p$gwf_country == "Congo-Brazzaville"] <- "Republic of Congo"
p$gwf_country[p$gwf_country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
p$gwf_country[p$gwf_country == "Cote d'Ivoire"] <- "Ivory Coast"

# Put map data into a data object named "world_map"
world_map <- map_data("world")
# omit Greenland and Antarctica to make map look better
world_map <- subset(world_map, region != "Antarctica" & region!="Greenland")

# Failed coup map
ggplot(p) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f", linewidth = 0.25
  ) +
  geom_map(map = world_map, aes(map_id = gwf_country, fill = total.fail.coups), linewidth = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "# of failed \n coup attempts") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  ylab(" ") + xlab(" ") +   # remove axis titles 
  ggtitle("Failed coup attempts, 1946-2022") + ggeasy::easy_center_title() + 
  theme(
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.text.y=element_blank(),  #remove y axis labels
    axis.ticks.y=element_blank()  #remove y axis ticks
  )

# Successful coup map
ggplot(p) +
  geom_map(
    dat = world_map, map = world_map, aes(map_id = region),
    fill = "white", color = "#7f7f7f", linewidth = 0.25
  ) +
  geom_map(map = world_map, aes(map_id = gwf_country, fill = total.success.coups), linewidth = 0.25) +
  scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "# of successful \n coup attempts") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  ylab(" ") + xlab(" ") +   # remove axis titles 
  ggtitle("Successful coup attempts, 1946-2022") + ggeasy::easy_center_title() + 
  theme(
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.text.y=element_blank(),  #remove y axis labels
    axis.ticks.y=element_blank()  #remove y axis ticks
  )

# T-tests 
t_test_success <- t.test(gwf_fail ~ success.coup,data=subset(coups,failed.coup==0))
t_test_fail    <- t.test(gwf_fail ~ failed.coup,data=subset(coups,success.coup==0))
t_test_success
t_test_fail

# T-test Plots 
par(mfrow=c(1,2))  # next 2 plots combined into 1 row in a single plot
# successful coups
# m, sd, and n are data objects with means, standard deviations and number of observations for gwf_failure in each group (coldwar and notcoldwar)
m <- aggregate(gwf_fail ~ success.coup,data=subset(coups,failed.coup==0), function(x) c(mean = mean(x)))
sd <- aggregate(gwf_fail ~ success.coup,data=subset(coups,failed.coup==0), function(x) c(sd = sd(x)))
n <- aggregate(gwf_fail ~ success.coup,data=subset(coups,failed.coup==0), function(x) c(n = length(x)))
mx<-c(m$gwf[1],m$gwf[2])  # new data object that just contains the mean values
names(m) = c("No coup attempt", "Successful coup")  # names for the bars in m
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mx, ylim=c(0, .65), xpd=FALSE,
              ylab = "Probability of regime collapse", 
              xlab = " ",
              cex.axis=.75,
              cex.lab=.75,
              cex.names=.75,
              main="Successful coups",
              names.arg=names(m))
arrows(x0=bp, y0=mx-se, y1=mx+se,code=0, angle=90 )  # arrows provides the CI on top of the bar
 
# successful coups
m <- aggregate(gwf_fail ~ failed.coup,data=subset(coups,success.coup==0), function(x) c(mean = mean(x)))
sd <- aggregate(gwf_fail ~ failed.coup,data=subset(coups,success.coup==0), function(x) c(sd = sd(x)))
n <- aggregate(gwf_fail ~ failed.coup,data=subset(coups,success.coup==0), function(x) c(n = length(x)))
mx<-c(m$gwf[1],m$gwf[2])  # new data object that just contains the mean values
names(m) = c("No coup attempt", "Failed coup")  # names for the bars in m
# Next command calculates the standard errors from the standard deviations
se       = c(sd$gwf[1]/sqrt(n$gwf[1]), 
             sd$gwf[2]/sqrt(n$gwf[2]))
bp <- barplot(mx, ylim=c(0, .65), xpd=FALSE,
              ylab = "Probability of regime collapse", 
              xlab = " ",
              cex.axis=.75,
              cex.lab=.75,
              cex.names=.75,
              main="Failed coups",
              names.arg=names(m))
arrows(x0=bp, y0=mx-se, y1=mx+se,code=0, angle=90 )  # arrows provides the CI on top of the bar


 