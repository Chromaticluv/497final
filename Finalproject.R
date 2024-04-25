

#read in data
data <- read.csv("demcollapsedata.csv")

#Task: Use iterated k-fold cross-validataion with a logistic classifer and the AUC-PR performance metric
# to asses a "Best" model for predictive performance. With this "Best" model produce forecases for the final
# year of the data set

#outcome variable: demcollapse (1 if the democracy collapsed in that year, 0 if not.)
#logistic function (probability)

###model 1: ruler legitimation ---``

data<-data[complete.cases(data$v2exl_legitideol),]
data<-data[complete.cases(data$v2exl_legitlead),]
data<-data[complete.cases(data$v2exl_legitperf),]
data<-data[complete.cases(data$demcollapse),]

model.legit <- formula(data, demcollapse ~ v2exl_legitideol + #Ruler Ideological Legitimation
                       v2exl_legitlead + # Ruler Personalist Legitimation
                       v2exl_legitperf) # Ruler Performance Legitimation
m1 <- glm(model.legit, data = data, family = binomial)


###model 2: political/ruling party ----

model.pol <- formula(data, demcollapse ~ iv2paseat + #Ruling Party Seat Share
                       iv2xpa_illiberal + #Ruling Party Illiberalism
                       iv2pareglef + #Ruling party left-right ideology
                       iv2xpa_popul) #Ruling party populism
m2 <- glm(model.pol, data=data, family=binomial)

###model 3: economic ----

model.econ <- formula(data, demcollapse ~ lpop + #Population logged
                    oilgasrentsgdp + #Oil and gas rents as % of GDP
                    gdppc + #GDP per cap
                    l12gr) #growth prior two years
m3 <- glm(model.econ, data=data, family = binomial)

#model 4: political/social polarization ----

model.soc <- formula(data, demcollapse ~ ipolarization + #Social Polarization when leader took power
                       iv2x_libdem + #Liberal democracy when leader came to power
                       iv2x_partipdem) #Participatory democracy when leader came to power
m4 <- glm(model.soc, data=data, family=binomial)

### Model 5: time ----

# demcollapse ~ gwf_duration + leadertimeinp~r + partyage
model.time <- formula(data, demcollapse ~ gwf_duration + #Duration of democracy
                        leadertimeinpower + #Leaders time in power
                        partyage) #party age
m5 <- glm(model.time, data=data, family = binomial)

