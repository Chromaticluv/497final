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
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 1")

 

# Get data sets we will use today
vdem13 <- read.csv("vdem13.csv")
gwf <-read.csv("gwf.csv")
 
# Keep only some columns of the data
vdem13<- subset(vdem13, select = c("cowcode", "country_name", "year", 
    "v2x_polyarchy", "v2x_regime", "v2xel_elecpres", "v2xel_elecparl"))

# Look at each data set
View(vdem13)
View(gwf)

# Merging data by cowcode and year 
mash <- merge(vdem13, gwf, by=c("cowcode","year"), all=TRUE)
View(mash)

# Sorting data by year/country
mash <- mash[order(mash$cowcode,mash$year),] 
View(mash)

# Drop observations with missing values on any variable
mash <- na.omit(mash)
View(mash)

 
 
######################  Assignment ####################

election <- mash[which(mash$gwf_dictatorship==1 & (mash$v2xel_elecpres==1 | mash$v2xel_elecparl==1) ), ]
nonelection <- mash[which(mash$gwf_dictatorship==1 & (mash$v2xel_elecpres==0 & mash$v2xel_elecparl==0)), ]
election$election <- 'election year'
nonelection$election  <- 'non-election year'
histd <- rbind(election, nonelection)
View(histd)
# Two density plots together, same variable by a binary category 
p <- ggplot(histd , aes(v2x_polyarchy,fill=election )) + geom_density(alpha = 0.5) +
  ggtitle("Level of democracy by election year")  + xlab("Democracy score") + ylab("Density")
p

# Fix up the plot a bit
p + theme(
plot.title = element_text(color="black", size=14, face="bold",hjust = 0.5),
axis.title.x = element_text(color="black", size=10, face="bold"),
axis.title.y = element_text(color="black", size=10, face="bold"),
legend.position = c(0.8, 0.85),
)

# create a .pdf and save that .pdf to the working directory
 pdf("g.pdf",         	   # File name
    width = 12, height = 10.5, # Width and height in inches
    bg = "white",          # Background color
    paper = "A4")          # Paper size


 

summarize(election, mean(v2x_polyarchy,na.rm=TRUE))
summarize(nonelection, mean(v2x_polyarchy,na.rm=TRUE))




