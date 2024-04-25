# Clear environment
rm(list = ls())

# Install and load required packages
# install.packages(c("tidyverse", "ggplot2", caret", "foreach"))
# install.packages(c("verification",  "pROC", "PRROC"))
library(tidyverse)
library(ggplot2)
library(caret)
library(foreach)
library(verification)
library(PRROC)
library(pROC)

# Set directory to a folder on your machine
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 10")

# Read data into R as a new data frame
nvc <- read.csv("nvc.transformed.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
rows.2012 <- which(nvc$year==2012)  # identify rows for year 2012
nvc.out.of.sample<- nvc[rows.2012, ]  # keep only rows with year==2012

# Make nvc all years prior to 2012
nvc <- nvc[-rows.2012,]  # keep only rows with year==2012

# Add columns with country age and prediction year (observation year + 1)
nvc$age <- nvc$year - nvc$yrborn
nvc$predyr <- nvc$year + 1

# Remove rows with missing data on the outcome
nvc <- nvc[!is.na(nvc$nvc.start.1), ]

# Model specification to test the function
mymodel.f <-  formula(nvc.start.1 ~ log(wdi.pop) +                        # Population size (log)
                        nld.any.1                                        # Election year
)  

# Initialize empty lists to store models and predictions
models <- list()
predictions <- list()

# Set seed to reproduce results that rely on random number generators
set.seed(42)
# obtain 10 random numbers between 2 and 10000 to use as seed values for each loop/repeat
iseed <- sample(2:10000,10, replace=FALSE) 


################## kayfoldf function ###########################
# Now we write our own `function' that does the repeated (10x) #
# 5-fold cross-validation of the model                         #
# This function takes the model/specification as the input     #
################################################################

kayfold<-function(mymodel.f) {
  print(mymodel.f)

  # Get predictor names for this model
  predictor.names <- all.vars(formula(mymodel.f))  # get predictor names
  predictor.names<-c("country", "year", predictor.names) # add country & year to predictor names list
  nvc.mymodel <- nvc[, c(predictor.names)]  # keep only model predictor variables + outcome variable

  # Keep only rows with no missing data
  rows.with.missing.data <- which(!complete.cases(nvc.mymodel))  # identify rows with missing data
  nvc.mymodel<- nvc.mymodel[-rows.with.missing.data, ]  # keep only rows with no missing data
  num.rows <- nrow(nvc.mymodel)

  # Data frame to store iterations of predicted probabilities and predicted class
  predictor.names<-c("country", "year", "nvc.start.1") # add country & year to predictor names list
  nvc.results<-nvc.mymodel[, c(predictor.names)]  # keep only outcome variable
  
  # set counter j to index iterations/repeats of the k-fold cross-validation
  j<-1    
  foreach(s = iseed) %do% {
    # For each iteration, manipulate a "temp" dataframe 
    nvc.temp<-nvc.mymodel
  
    # Randomly splits data into 5 folds
    set.seed(s)
    nvc.temp$index <- runif(num.rows) # add column for random number between 0,1
    folds <- cut(nvc.temp$index, breaks = 5, labels = FALSE)  # split data into five folds

    # Iterate over each value of 1:5 (for 5-fold cross-validation)
    foreach(k = 1:5) %do% {
      # Subset nvc.temp based on the kth fold of the data
      test.data <- nvc.temp[folds == k, ]
      # Subset nvc.mymodel based on the k-1 folds of the data
      train.data <- nvc.temp[folds != k, ]
      # Fit a logistic regression model to the train subset
      models[[k]] <- glm(mymodel.f, data = train.data, family = binomial)
      # Predicted values: betas from train.data model using X values from test.data 
      predictions[[k]] <- predict(models[[k]], newdata = test.data, type = "response")
      # Add predictions back into test.data
      test.data$pr <- predictions[[k]]
      cnames<-c("country", "year", "pr") # column names list
      test.data <- test.data[, c(cnames)]  # keep only four columns of data to merge back into nvc.mymodel
      cnames <- c("country", "year", paste0("pr", k))
      colnames(test.data) <- cnames
      nvc.temp<- merge(nvc.temp, test.data, by=c("country","year"), all=TRUE)
    }
  
    # Put each of the 5-fold predicted probabilities into a single column
    coln <- c("pr1", "pr2", "pr3", "pr4" , "pr5")
    nvc.pr <- nvc.temp[, coln]
    nvc.results[[paste0("pr", j)]] <- rowSums(nvc.pr, na.rm = TRUE)
  
    j<-j+1   # increase j value by 1 for the next repeat of the loop
  }
  
  # Average predicted probabilities from iterations
  coln <- c("pr1", "pr2", "pr3", "pr4", "pr5", "pr6", "pr7" , "pr8", "pr9", "pr10")
  nvc.results$pr<-rowMeans(nvc.results[,coln], na.rm = TRUE)  # mean predicted pr from each of iterations

  # Plot precision-recall curve
  pr <- pr.curve(nvc.results$nvc.start.1, nvc.results$pr, curve = TRUE )
  #plot(pr)
  print(pr$auc.integral)  # print the PR-AUC in the console output
  coln <- c("country", "year", "pr")
  out.nvc<-nvc.results[,coln]
  return(out.nvc)  # executing the function yields column of predicted probabilities

}


####################################
### Specify 4 theoretical models ###
####################################

# Model specifications to test - operationalized as formulas
gripes.f <- formula(nvc.start.1 ~ log(wdi.pop) +                        # Population size (log)
                      log(xxxcimr) +                                      # Infant mortality rate rel to global median (log)
                      wdi.gdpchg.s +                                      # GDP growth rate (sq rt)
                      sqrt(wdi.cpi) +                                     # Consumer price index (sq rt)
                      log1p(bnn.yroff) +                                  # Leader's years in office (log)
                      elceleth.c +                                        # Ruling elites' ethnicity is politically salient
                      dispota4.c +                                        # Any state-led discrimination
                      cir.physint +                                       # CIRI physical integrity index
                      I(cir.physint^2) )

modnzn.f <- formula(nvc.start.1 ~ log(wdi.pop) +                        # Population size (log)
                      wdi.popurb.mi +                                     # Urban population (0-100%), imputed
                      I(wdi.manuf.mi + wdi.servs.mi )+                    # Manufacturing & services (% of GDP), imputed
                      wdi.sch2.mi +                                       # 2ry school enrollment rate, imputed
                      log1p(wdi.mobp100) +                                # Mobile phone subs per 100 ppl (logged)
                      ios.gattwto )                                       # GATT/WTO member

rscmob.f <- formula(nvc.start.1 ~ log(wdi.pop) +                        # Population size (log)
                      wdi.popurb.mi +                                     # Urban population (0-100%), imputed
                      ythbul4 +                                           # Youth bulge (15-24 yos as % of tot pop)
                      log1p(bnk.unrest) +                                 # Sum of Banks riots & dems (log)
                      log1p(bnk.strikes) +                                # Sum of Banks strikes (log)
                      log1p(nvc.dosregt) +                                # Onsets of nonviolent campaigns in same region (log)
                      nvc.ongoing +                                       # Any ongoing nonviolent campaign
                      civilwar )                                          # Any ongoing PITF civil war

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
                      log1p(cou.tries5) )                                 # Coup activity in past 5 years 



#######################################################
### test the 4 models using the function "kayfoldf" ###
#######################################################
# Create data frame to store predicted probabilities from each model
coln <- c("country", "year", "nvc.start.1")
nvc.final<-nvc[,coln]
 

# Iterate over models and populate nvc.final
mnames <- c("gripes", "modnzn", "rscmob", "polopp")
for (m in mnames) {
  pr0 <- kayfold(get(paste0(m, ".f")))  # Assuming kayfold is a function returning vector of predicted probabilities
  nvc.final<-merge(nvc.final, pr0, by = c("country", "year"), all = TRUE)
  colnames(nvc.final)[which(colnames(nvc.final) == "pr")] <- paste0(m, "pr")
}
 

############  Now for an out-of sample forecast ###########
# use all data up to and including 2012 to run the model and get the beta coefficients
best.model <- glm(gripes.f, data = nvc, family = binomial)

# then use data observed data from next year to calculate predicted probabilities 
predictions <- predict(best.model, newdata = nvc.out.of.sample, type = "response")

# plot the predicted probabilities, which are now forecasts

 #############  THE END #############
