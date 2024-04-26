rm(list = ls(all.names = TRUE)) 

library(ggplot2)
library(tidyverse)
library(ggeasy)
#read in data
data <- read.csv("demcollapsedata.csv")
testdata<-read.csv("coupdata.csv")

min(data$year)
max(data$year) #data is between year 1991 and 2020

#test GLM
model.test<- formula(testdata, democracy ~ wdi_popurb)
unique(testdata$democracy)
mtest <- glm(model.test, data=testdata, family = binomial)

#Task: Use iterated k-fold cross-validataion with a logistic classifier and the AUC-PR performance metric
# to asses a "Best" model for predictive performance. With this "Best" model produce forecasts for the final
# year of the data set

#outcome variable: demcollapse (1 if the democracy collapsed in that year, 0 if not.)
#logistic function (probability)

###model 1: ruler legitimation ----


model.legit <- glm(demcollapse ~ v2exl_legitideol + #Ruler Ideological legitimation
                     v2exl_legitlead + #Ruler Personalist Legitimation
                     v2exl_legitperf, #Ruler Performance Legitimation 
                   data = data, family = binomial)
summary(model.legit)

#test distribution
hist(data$v2exl_legitideol) #normal, fine.
hist(data$v2exl_legitlead) #a bit of concern but mostly normal
hist(data$v2exl_legitperf) #a bit wonky

###model 2: political/ruling party ----

model.pol <- glm(demcollapse ~ iv2paseat + #Ruling Party Seat Share
                       iv2xpa_illiberal + #Ruling Party Illiberalism
                       iv2pariglef + #Ruling party left-right ideology
                       iv2xpa_popul,
                 data = data, family = binomial) #Ruling party populism
summary(model.pol)

###model 3: economic ----

model.econ <- glm(demcollapse ~ lpop + #Population logged
                        oilgasrentsgdp + #Oil and gas rents as % of GDP
                        gdppc + #GDP per cap
                        l12gr,
                  data = data, family = binomial) #growth prior two years
summary(model.econ)

#test skew
hist(data$lpop) #fine
hist(data$oilgasrentsgdp) #Very skewed. try log?
hist(log(data$oilgasrentsgdp)) #now skewed the opposite way. not sure if better.
hist(data$gdppc) #distribution is almost bi-modal, not normal at all.
hist(data$l12gr) #kind of skewed but mostly normal, ok.


#model 4: political/social polarization ----

model.soc <- glm(demcollapse ~ ipolarization + #Social Polarization when leader took power
                       iv2x_libdem + #Liberal democracy when leader came to power
                       iv2x_partipdem,
                 data=data, family=binomial) #Participatory democracy when leader came to power
summary(model.soc)

### Model 5: time ----

# demcollapse ~ gwf_duration + leadertimeinp~r + partyage
model.time <- glm(demcollapse ~ gwf_duration + #Duration of democracy
                        leadertimeinpower + #Leaders time in power
                        partyage,
                  data=data, family=binomial) #party age
summary(model.time)


### Distribution of Outcome ----

ggplot(data, aes(x=demcollapse)) +
  geom_bar(width = 0.3)

outcomes<- as.data.frame(table(data$demcollapse))

p <- outcomes$Freq[2] / (outcomes$Freq[1]+outcomes$Freq[2])
q <- 1-p



### Predicted probabilities of democracy collapse ----
#I am not really sure if this is what I'm looking for.
data$pr_legit <- predict(model.legit, newdata = data, type = "response")
p1 <- ggplot(data, aes(pr_legit, na.rm = TRUE)) + 
  geom_density(alpha = 0.25, na.rm = TRUE) +
  ggtitle("Pr(Collapse|X) by Regime collapse")  + 
  xlab("Predicted probabilities of regime collapse") + 
  ylab("Density") +
  geom_vline(xintercept = mean(nvc$nvc.start.1,na.rm = TRUE), color = "red",linetype = 1)
p1





