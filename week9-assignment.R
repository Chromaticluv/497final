# Clear environment
rm(list = ls())

# Install and load required packages
#install.packages(c("tidyverse", "caret", "foreach","psych"))
library(tidyverse)
library(caret)
library(foreach)
library(psych)  # for the "describe" command

# Set directory to a folder on your machine
setwd("C:/Users/jgw12/Dropbox/Courses/Modeling political change/Labs/Week 8")

# Read data into R as a new data frame
nvc <- read.csv("nvc.transformed.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Add columns with country age and prediction year (observation year + 1)
nvc$age <- nvc$year - nvc$yrborn

# Remove rows with missing data on the outcome
nvc <- nvc[!is.na(nvc$nvc.start.1), ]
nvc <- nvc[!is.na(nvc$wdi.pop), ]

# Initialize empty lists to store models and predictions
models <- list()
predictions <- list()

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




#### There are Four Formulas -- or sets of predictors #### 
#### Determine which one produces the highest Kappa   ####
#### When we set the cut point so that                ####
#### Sensitivity roughly equals Specificity           ####


########## START GRIPES  ##########
# Get predictor names for this model
      predictor.names <- all.vars(formula(gripes.f))  # get predictor names
      predictor.names<-c("country", "year", predictor.names) # add country & year to predictor names list
      nvc.gripes <- nvc[, c(predictor.names)]  # keep only model predictor variables + outcome variable

      # Keep only rows with no missing data
      rows.with.missing.data <- which(!complete.cases(nvc.gripes))  # identify rows with missing data
      nvc.gripes<- nvc.gripes[-rows.with.missing.data, ]  # keep only rows with no missing data
      num.rows <- nrow(nvc.gripes)  # a scalar value that is the number of rows in the data frame

      # Save the mean value of the observed outcome in the data frame with no missing data
      cutp<-mean(nvc.gripes$nvc.start.1)

      # Put each observation in a fold (5 folds for k=5 in k-fold cross-validation)
      set.seed(84)   # keep this integer to reproduce the exact same results
      nvc.gripes[[paste0("index")]] <- runif(num.rows) # add column for random number
      # 5 columns, for 5-fold cross-validation, for each iteration
      # In each of these columns, a random 20% of rows are "1" and 80% are "0"
      nvc.gripes[[paste0("k1")]] <- ifelse(nvc.gripes$index <= 0.2, 1, 0)
      nvc.gripes[[paste0("k2")]] <- ifelse(nvc.gripes$index > 0.2 & nvc.gripes$index <= 0.4, 1, 0)
      nvc.gripes[[paste0("k3")]] <- ifelse(nvc.gripes$index > 0.4 & nvc.gripes$index <= 0.6, 1, 0)
      nvc.gripes[[paste0("k4")]] <- ifelse(nvc.gripes$index > 0.6 & nvc.gripes$index <= 0.8, 1, 0)
      nvc.gripes[[paste0("k5")]] <- ifelse(nvc.gripes$index > 0.8, 1, 0)

      # Iterate over each value of 1:5 (for 5-fold cross-validation)
      foreach(i = 1:5) %do% {
          # Subset data based on the k-1 partitions of the data
          train.data <- nvc.gripes[nvc.gripes[[paste0("k", i)]] == 0, ]
          # Subset data into kth (unseen) partition of the data 
          test.data <- nvc.gripes[nvc.gripes[[paste0("k",i)]] == 1, ]
          # Fit a logistic regression model to the train subset
          models[[i]] <- glm(gripes.f, data = train.data, family = binomial)
          # Predicted values: betas from train.data model using X values from test.data 
          predictions[[i]] <- predict(models[[i]], newdata = test.data, type = "response")
          # Add predictions back into test.data
          test.data$pr <- predictions[[i]]
          test.data$PredProtestFold <- ifelse(test.data$pr >= cutp*1, 1, 0)  # cut point is 1.1*cutp, true==1, false==0
          cnames<-c("country", "year", "pr", "PredProtestFold") # column names list
          test.data <- test.data[, c(cnames)]  # keep only four columns of data to merge back into nvc.mymodel
          cnames <- c("country", "year", paste0("pr", i), paste0("PredProtestFold", i))
          colnames(test.data) <- cnames
          nvc.gripes<- merge(nvc.gripes, test.data, by=c("country","year"), all=TRUE)
      }

      # Put each of the 5-fold predicted probabilities & classes into a single column
      coln<-c("pr1", "pr2", "pr3", "pr4" , "pr5")
      nvc.pr<-nvc.gripes[,coln]
      nvc.gripes$pr <- rowSums(nvc.pr, na.rm = TRUE)
      coln<-c("PredProtestFold1", "PredProtestFold2", "PredProtestFold3", "PredProtestFold4" , "PredProtestFold5")
      nvc.pr<-nvc.gripes[,coln]
      nvc.gripes$pred <- rowSums(nvc.pr, na.rm = TRUE)

      # ConfusionMatrix(data = predicted_class, reference = true_data)
      cm<-confusionMatrix(data=as.factor(nvc.gripes$pred),reference=as.factor(nvc.gripes$nvc.start.1))                    
      spec <- cm$byClass["Specificity"]
      sens <- cm$byClass["Sensitivity"]
      kappa <-cm$overall["Kappa"]
      print(c(kappa, spec , sens ))   #Kappa=.0308
########## END GRIPES  ##########

      
########## START MODNZN  ##########
      # Get predictor names for this model
      predictor.names <- all.vars(formula(modnzn.f))  # get predictor names
      predictor.names<-c("country", "year", predictor.names) # add country & year to predictor names list
      nvc.modnzn <- nvc[, c(predictor.names)]  # keep only model predictor variables + outcome variable
      
      # Keep only rows with no missing data
      rows.with.missing.data <- which(!complete.cases(nvc.modnzn))  # identify rows with missing data
      nvc.modnzn<- nvc.modnzn[-rows.with.missing.data, ]  # keep only rows with no missing data
      num.rows <- nrow(nvc.modnzn)  # a scalar value that is the number of rows in the data frame
      
      # Save the mean value of the observed outcome in the data frame with no missing data
      cutp<-mean(nvc.modnzn$nvc.start.1)
      
      # Put each observation in a fold (5 folds for k=5 in k-fold cross-validation)
      set.seed(84)   # keep this integer to reproduce the exact same results
      nvc.modnzn[[paste0("index")]] <- runif(num.rows) # add column for random number
      # 5 columns, for 5-fold cross-validation, for each iteration
      # In each of these columns, a random 20% of rows are "1" and 80% are "0"
      nvc.modnzn[[paste0("k1")]] <- ifelse(nvc.modnzn$index <= 0.2, 1, 0)
      nvc.modnzn[[paste0("k2")]] <- ifelse(nvc.modnzn$index > 0.2 & nvc.modnzn$index <= 0.4, 1, 0)
      nvc.modnzn[[paste0("k3")]] <- ifelse(nvc.modnzn$index > 0.4 & nvc.modnzn$index <= 0.6, 1, 0)
      nvc.modnzn[[paste0("k4")]] <- ifelse(nvc.modnzn$index > 0.6 & nvc.modnzn$index <= 0.8, 1, 0)
      nvc.modnzn[[paste0("k5")]] <- ifelse(nvc.modnzn$index > 0.8, 1, 0)
      
      # Iterate over each value of 1:5 (for 5-fold cross-validation)
      foreach(i = 1:5) %do% {
        # Subset data based on the k-1 partitions of the data
        train.data <- nvc.modnzn[nvc.modnzn[[paste0("k", i)]] == 0, ]
        # Subset data into kth (unseen) partition of the data 
        test.data <- nvc.modnzn[nvc.modnzn[[paste0("k",i)]] == 1, ]
        # Fit a logistic regression model to the train subset
        models[[i]] <- glm(modnzn.f, data = train.data, family = binomial)
        # Predicted values: betas from train.data model using X values from test.data 
        predictions[[i]] <- predict(models[[i]], newdata = test.data, type = "response")
        # Add predictions back into test.data
        test.data$pr <- predictions[[i]]
        test.data$PredProtestFold <- ifelse(test.data$pr >= cutp*.99, 1, 0)  # cut point is 1.1*cutp, true==1, false==0
        cnames<-c("country", "year", "pr", "PredProtestFold") # column names list
        test.data <- test.data[, c(cnames)]  # keep only four columns of data to merge back into nvc.mymodel
        cnames <- c("country", "year", paste0("pr", i), paste0("PredProtestFold", i))
        colnames(test.data) <- cnames
        nvc.modnzn<- merge(nvc.modnzn, test.data, by=c("country","year"), all=TRUE)
      }
      
      # Put each of the 5-fold predicted probabilities & classes into a single column
      coln<-c("pr1", "pr2", "pr3", "pr4" , "pr5")
      nvc.pr<-nvc.modnzn[,coln]
      nvc.modnzn$pr <- rowSums(nvc.pr, na.rm = TRUE)
      coln<-c("PredProtestFold1", "PredProtestFold2", "PredProtestFold3", "PredProtestFold4" , "PredProtestFold5")
      nvc.pr<-nvc.modnzn[,coln]
      nvc.modnzn$pred <- rowSums(nvc.pr, na.rm = TRUE)
      
      # ConfusionMatrix(data = predicted_class, reference = true_data)
      cm<-confusionMatrix(data=as.factor(nvc.modnzn$pred),reference=as.factor(nvc.modnzn$nvc.start.1))                    
      spec <- cm$byClass["Specificity"]
      sens <- cm$byClass["Sensitivity"]
      kappa <-cm$overall["Kappa"]
      print(c(kappa, spec , sens ))  #Kappa=.032
########## END MODNZN  ##########
      
########## START RSCMOB  ##########
      # Get predictor names for this model
      predictor.names <- all.vars(formula(rscmob.f))  # get predictor names
      predictor.names<-c("country", "year", predictor.names) # add country & year to predictor names list
      nvc.rscmob <- nvc[, c(predictor.names)]  # keep only model predictor variables + outcome variable
      
      # Keep only rows with no missing data
      rows.with.missing.data <- which(!complete.cases(nvc.rscmob))  # identify rows with missing data
      nvc.rscmob<- nvc.rscmob[-rows.with.missing.data, ]  # keep only rows with no missing data
      num.rows <- nrow(nvc.rscmob)  # a scalar value that is the number of rows in the data frame
      
      # Save the mean value of the observed outcome in the data frame with no missing data
      cutp<-mean(nvc.rscmob$nvc.start.1)
      
      # Put each observation in a fold (5 folds for k=5 in k-fold cross-validation)
      set.seed(84)   # keep this integer to reproduce the exact same results
      nvc.rscmob[[paste0("index")]] <- runif(num.rows) # add column for random number
      # 5 columns, for 5-fold cross-validation, for each iteration
      # In each of these columns, a random 20% of rows are "1" and 80% are "0"
      nvc.rscmob[[paste0("k1")]] <- ifelse(nvc.rscmob$index <= 0.2, 1, 0)
      nvc.rscmob[[paste0("k2")]] <- ifelse(nvc.rscmob$index > 0.2 & nvc.rscmob$index <= 0.4, 1, 0)
      nvc.rscmob[[paste0("k3")]] <- ifelse(nvc.rscmob$index > 0.4 & nvc.rscmob$index <= 0.6, 1, 0)
      nvc.rscmob[[paste0("k4")]] <- ifelse(nvc.rscmob$index > 0.6 & nvc.rscmob$index <= 0.8, 1, 0)
      nvc.rscmob[[paste0("k5")]] <- ifelse(nvc.rscmob$index > 0.8, 1, 0)
      
      # Iterate over each value of 1:5 (for 5-fold cross-validation)
      foreach(i = 1:5) %do% {
        # Subset data based on the k-1 partitions of the data
        train.data <- nvc.rscmob[nvc.rscmob[[paste0("k", i)]] == 0, ]
        # Subset data into kth (unseen) partition of the data 
        test.data <- nvc.rscmob[nvc.rscmob[[paste0("k",i)]] == 1, ]
        # Fit a logistic regression model to the train subset
        models[[i]] <- glm(rscmob.f, data = train.data, family = binomial)
        # Predicted values: betas from train.data model using X values from test.data 
        predictions[[i]] <- predict(models[[i]], newdata = test.data, type = "response")
        # Add predictions back into test.data
        test.data$pr <- predictions[[i]]
        test.data$PredProtestFold <- ifelse(test.data$pr >= cutp*.94, 1, 0)  # cut point is 1.1*cutp, true==1, false==0
        cnames<-c("country", "year", "pr", "PredProtestFold") # column names list
        test.data <- test.data[, c(cnames)]  # keep only four columns of data to merge back into nvc.mymodel
        cnames <- c("country", "year", paste0("pr", i), paste0("PredProtestFold", i))
        colnames(test.data) <- cnames
        nvc.rscmob<- merge(nvc.rscmob, test.data, by=c("country","year"), all=TRUE)
      }
      
      # Put each of the 5-fold predicted probabilities & classes into a single column
      coln<-c("pr1", "pr2", "pr3", "pr4" , "pr5")
      nvc.pr<-nvc.rscmob[,coln]
      nvc.rscmob$pr <- rowSums(nvc.pr, na.rm = TRUE)
      coln<-c("PredProtestFold1", "PredProtestFold2", "PredProtestFold3", "PredProtestFold4" , "PredProtestFold5")
      nvc.pr<-nvc.rscmob[,coln]
      nvc.rscmob$pred <- rowSums(nvc.pr, na.rm = TRUE)
      
      # ConfusionMatrix(data = predicted_class, reference = true_data)
      cm<-confusionMatrix(data=as.factor(nvc.rscmob$pred),reference=as.factor(nvc.rscmob$nvc.start.1))                    
      spec <- cm$byClass["Specificity"]
      sens <- cm$byClass["Sensitivity"]
      kappa <-cm$overall["Kappa"]
      print(c(kappa, spec , sens ))  #Kappa=.039
########## END RSCMOB  ##########
      
########## START POLOPP  ##########
      # Get predictor names for this model
      predictor.names <- all.vars(formula(polopp.f))  # get predictor names
      predictor.names<-c("country", "year", predictor.names) # add country & year to predictor names list
      nvc.polopp <- nvc[, c(predictor.names)]  # keep only model predictor variables + outcome variable
      
      # Keep only rows with no missing data
      rows.with.missing.data <- which(!complete.cases(nvc.polopp))  # identify rows with missing data
      nvc.polopp<- nvc.polopp[-rows.with.missing.data, ]  # keep only rows with no missing data
      num.rows <- nrow(nvc.polopp)  # a scalar value that is the number of rows in the data frame
      
      # Save the mean value of the observed outcome in the data frame with no missing data
      cutp<-mean(nvc.polopp$nvc.start.1)
      
      # Put each observation in a fold (5 folds for k=5 in k-fold cross-validation)
      set.seed(84)   # keep this integer to reproduce the exact same results
      nvc.polopp[[paste0("index")]] <- runif(num.rows) # add column for random number
      # 5 columns, for 5-fold cross-validation, for each iteration
      # In each of these columns, a random 20% of rows are "1" and 80% are "0"
      nvc.polopp[[paste0("k1")]] <- ifelse(nvc.polopp$index <= 0.2, 1, 0)
      nvc.polopp[[paste0("k2")]] <- ifelse(nvc.polopp$index > 0.2 & nvc.polopp$index <= 0.4, 1, 0)
      nvc.polopp[[paste0("k3")]] <- ifelse(nvc.polopp$index > 0.4 & nvc.polopp$index <= 0.6, 1, 0)
      nvc.polopp[[paste0("k4")]] <- ifelse(nvc.polopp$index > 0.6 & nvc.polopp$index <= 0.8, 1, 0)
      nvc.polopp[[paste0("k5")]] <- ifelse(nvc.polopp$index > 0.8, 1, 0)
      
      # Iterate over each value of 1:5 (for 5-fold cross-validation)
      foreach(i = 1:5) %do% {
        # Subset data based on the k-1 partitions of the data
        train.data <- nvc.polopp[nvc.polopp[[paste0("k", i)]] == 0, ]
        # Subset data into kth (unseen) partition of the data 
        test.data <- nvc.polopp[nvc.polopp[[paste0("k",i)]] == 1, ]
        # Fit a logistic regression model to the train subset
        models[[i]] <- glm(polopp.f, data = train.data, family = binomial)
        # Predicted values: betas from train.data model using X values from test.data 
        predictions[[i]] <- predict(models[[i]], newdata = test.data, type = "response")
        # Add predictions back into test.data
        test.data$pr <- predictions[[i]]
        test.data$PredProtestFold <- ifelse(test.data$pr >= cutp*1.01, 1, 0)  # cut point is 1.1*cutp, true==1, false==0
        cnames<-c("country", "year", "pr", "PredProtestFold") # column names list
        test.data <- test.data[, c(cnames)]  # keep only four columns of data to merge back into nvc.mymodel
        cnames <- c("country", "year", paste0("pr", i), paste0("PredProtestFold", i))
        colnames(test.data) <- cnames
        nvc.polopp<- merge(nvc.polopp, test.data, by=c("country","year"), all=TRUE)
      }
      
      # Put each of the 5-fold predicted probabilities & classes into a single column
      coln<-c("pr1", "pr2", "pr3", "pr4" , "pr5")
      nvc.pr<-nvc.polopp[,coln]
      nvc.polopp$pr <- rowSums(nvc.pr, na.rm = TRUE)
      coln<-c("PredProtestFold1", "PredProtestFold2", "PredProtestFold3", "PredProtestFold4" , "PredProtestFold5")
      nvc.pr<-nvc.polopp[,coln]
      nvc.polopp$pred <- rowSums(nvc.pr, na.rm = TRUE)
      
      # ConfusionMatrix(data = predicted_class, reference = true_data)
      cm<-confusionMatrix(data=as.factor(nvc.polopp$pred),reference=as.factor(nvc.polopp$nvc.start.1))                    
      spec <- cm$byClass["Specificity"]
      sens <- cm$byClass["Sensitivity"]
      kappa <-cm$overall["Kappa"]
      print(c(kappa, spec , sens ))  #Kappa=.049
########## END POLOPP  ##########
      