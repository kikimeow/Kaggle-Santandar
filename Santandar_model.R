# set your working directory
path <- "~/Kaggle/Kaggle- Santandar"
setwd(path)


# load packages
packages <-c("dplyr")  #  load plyr ALWAYS before dplyr, AND not load plyr again.

lapply(packages, FUN = function(X) {
  do.call("library", list(X)) 
})

# memory management
memory.limit(size = 100000)


###
# XGBoost Baseline
###

# load files
train <- readRDS(file = "Data/train.Rda")
test <- readRDS(file="Data/test.Rda")

ProdList <- c("prod_1_currentAcct", "prod_2_payrollAcct", "prod_3_juniorAcct", "prod_4_masIndividualAcct", "prod_5_individualAcct", "prod_6_individualPlusAcct", "prod_7_eAccount","prod_8_funds","prod_9_mortgage","prod_10_pensions1","prod_11_loans","prod_12_taxes","prod_13_creditCard","prod_14_securities","prod_15_payroll","prod_16_pension2","prod_17_directDebit")

# create dataframe to store all the predicted probability of Testing Set
prodProbabilityTest <- data.frame(matrix(vector(), nrow(test), 18))
colnames(prodProbabilityTest) <- c("customerCode", paste0(ProdList))
prodProbabilityTest$customerCode <- test$customerCode

# setup environment
library(h2o)  # Requires version >=0.0.4 of h2oEnsemble
localH2O <- h2o.init(nthreads = -1, max_mem_size = "14G")  # Start an H2O cluster with nthreads = num cores on your machine.  Use all cores available to you (-1)
h2o.removeAll() # Clean slate - just in case the cluster was already running

# create h2o frames
train_h2o <- as.h2o(train,  destination_frame = "train_h2o")
test_h2o <- as.h2o(test,  destination_frame = "test_h2o")

subsetTrain <- train %>%
  filter(dateM == "06")
train_h2o06 <- as.h2o(subsetTrain,  destination_frame = "train_h2o06")

subsetTrain <- train %>%
  filter(dateM %in% c("06", "12"))
train_h2ocurr <- as.h2o(subsetTrain,  destination_frame = "train_h2ocurr")

subsetTrain <- train %>%
  filter(dateM %in% c("03", "04", "05"))
train_h2o030405 <- as.h2o(subsetTrain,  destination_frame = "train_h2o030405")

# Fill in column to be ignored by model.  
colExclude <- c("date", "customerCode")

#####
# Model Run 
#####

# Select products to run model on
ProdListModel <- ProdList[c(1:17)]

# loop through products
for (prod in ProdListModel){
  print(prod)
  startT <- Sys.time()
  print(paste("start:", startT))
  
  # Select which product to run
  product <- prod
  prodNum <- as.character(unique(na.omit(as.numeric(unlist(strsplit(unlist(product), "[^0-9]+")))))[1])
  
  # specify data dates
  if(prodNum == 12) { # prod_12_taxes use month 06
    trainingFrame <- train_h2o06
  } else if(prodNum == 1) {  # prod_1_currentAcct
    trainingFrame <- train_h2ocurr
  } else if(prodNum == 4 | prodNum == 7 | prodNum == 17) { #prod_4_masIndividualAcct & prod_7_eAccountDate & prod_17_directDebit
    trainingFrame <- train_h2o030405
  } else {
    trainingFrame <- train_h2o
  }
  
  print(paste("size of training frame", dim(trainingFrame)[1], "x", dim(trainingFrame)[2]))
  
  # use less folds for more data
  fold <- ifelse(prodNum == 12, 10,  # prod_12_taxes
                 ifelse(prodNum == 1, 10, # prod_1_currentAcct
                        ifelse(prodNum == 4 | prodNum == 7 | prodNum == 17, 10, #prod_4_masIndividualAcct & prod_7_eAccountDate & prod_17_directDebit
                               5)))
  print(paste("number of folds", fold))
  
  # Setup X (predictors) & Y
  Namey <- paste0("FlagAddition.", product)
  Namesx <- setdiff(names(train[,!(colnames(train)) %in% colExclude]), Namey)  # need to exclude the rest of added.prod
  Namesx <- Namesx[!(Namesx %in% colnames(train_h2o[which(colnames(train_h2o) == "FlagAddition.prod_1_currentAcct"): which(colnames(train_h2o) == "FlagAddition.prod_17_directDebit")]))]
  
  trainingFrame[Namey] <- as.factor(trainingFrame[Namey])
  
  # simple model
  model <- h2o.gbm(
    y = Namey,
    x = Namesx,
    training_frame = trainingFrame,
    stopping_rounds = 5,
    stopping_tolerance = 1e-4,
    stopping_metric = "logloss",
    sample_rate = 0.8,
    col_sample_rate = 0.8,
    seed = 1234,
    score_tree_interval = 10,
    distribution = "AUTO",
    model_id = product,
    nfolds = fold)
  
  print(model)
  
  # save the model
  model_path <- h2o.saveModel(model,  paste0(getwd(), "/Model/"), force = TRUE)
  print(model_path)
  
  # make predictions using model
  predictH2O <- predict(model, test_h2o)
  predictions <- as.data.frame(predictH2O$p1) # probability of "1"

  # add predicted probability to prodProbabilityTest table
  prodProbabilityTest[which(colnames(prodProbabilityTest)== paste0(product))] <- predictions
  endT <- Sys.time()
  print(paste(prod, "Time used:", endT-startT))
}
saveRDS(prodProbabilityTest, file="Output/prediction.Rda")
  
###
# Model Output Analysis
###
ProdList <- c("prod_1_currentAcct", "prod_2_payrollAcct", "prod_3_juniorAcct", "prod_4_masIndividualAcct", "prod_5_individualAcct", "prod_6_individualPlusAcct", "prod_7_eAccount","prod_8_funds","prod_9_mortgage","prod_10_pensions1","prod_11_loans","prod_12_taxes","prod_13_creditCard","prod_14_securities","prod_15_payroll","prod_16_pension2","prod_17_directDebit")

model <- vector("list", 17) # vector to save model
auc <- vector("list", 17) # vector to save AUC
accuracy <- vector("list", 17)
precision <- vector("list", 17) 
recall <- vector("list", 17) 
specificity <- vector("list", 17) 

for (i in 1:17){
  print(ProdList[i])
  modelPath <- paste0(getwd(), "/Model/", ProdList[i])
  model[[i]] <- h2o.loadModel(modelPath)
  h2o.varimp_plot(model[[i]], num_of_features = 15)  # Variable Importance: plot top 15 contributors 
  auc[[i]] <- h2o.auc(h2o.performance(model[[i]], newdata = NULL))
  accuracy[[i]] <- h2o.auc(h2o.performance(model[[i]], newdata = NULL))
  precision[[i]] <- h2o.precision(h2o.performance(model[[i]], newdata = NULL))
  recall[[i]] <- h2o.recall(h2o.performance(model[[i]], newdata = NULL))
  specificity[[i]] <- h2o.specificity(h2o.performance(model[[i]], newdata = NULL))
}

