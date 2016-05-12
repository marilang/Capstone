# re-use code from intrusion detection homework

# libraries
library(randomForest)
library(caret)

# working dir and data dir, file names for the data to be loaded
setwd("~/0 Projects/0 Data Science/2016 EDSO Program/Capstone")
#dd <- "~/0 Projects/0 Data Science/2016 EDSO Program/Capstone/yelp_csv"
#dataFileName <- "yelp_academic_dataset_business.csv"

# Start with preprocessed file
dd <- "~/0 Projects/0 Data Science/2016 EDSO Program/Capstone"
dataFileName <- "business.preprocessed.csv"

# Include the performance metrics function I wrote in HW 2. -ML
evaluateClassificationModel <- function(predictedClass, actualClass, modelname='model') {
  CM <- table(actualClass, predictedClass)
  TP <- CM[1,1]
  TN <- CM[2,2]
  FP <- CM[2,1]
  FN <- CM[1,2]
  ACC <- (TP+TN)/(TP+FP+TN+FN)
  ErrorRate <- (FP+FN)/(TP+FP+TN+FN)
  Precision <- TP/(TP+FP)
  Recall <- TP/(TP+FN)
  F1measure <- 2*Precision*Recall/(Precision + Recall)  
  Sensitivity <- TP/(TP+FN)
  Specificity <- TN/(TN+FP)
  return(list(modelname=modelname, CM=CM, ACC=ACC, ErrorRate=ErrorRate, 
              Precision=Precision, Recall=Recall, F1measure=F1measure,
              Sensitivity=Sensitivity, Specificity=Specificity))
}


# load the data: make sure the data set is the data dir
dataSet <- paste(dd,"/",dataFileName,sep="")
print(paste("Loading data set: ", dataSet))
data <- read.csv(dataSet,stringsAsFactors=T)

#===================================================
# STEP 1: Get familiar with the data (cursory look): 
# - class, mode, dimensions
# - names of variables
# - response variable 
#   i.e., predictand, dependent, outcome, or target var, class label
# - predictor variables
#   i.e., features, dimensions, attributes, independent vars
# - exclude variables
#   vars that will not be used as predictors
# - distribution by class labels
#===================================================
names(data)  
dim(data)

# some additional investigation -ML
class(data)
mode(data)
#View(data) 
data[1,]
summary(data)
str(data)

# Change the format of some columns
#summary(is.na(data$stars))  
# We do not find any NA values in the star ratings -ML
#HighRating <- function(stars){
#  return(as.numeric(stars>3.5))
#}
#data$High.Rating <- sapply(data$stars, HighRating)

#FreeWifi <- function(wifi){
#  return(as.numeric(wifi=="free"))
#}
#data$attributes.Wi.Fi.Free <- sapply(data$attributes.Wi.Fi, FreeWifi)
#summary(data$attributes.Wi.Fi.Free)

#WaitSvc <- function(waiter){
#  return(as.numeric(waiter=="True"))
#}
#data$attributes.Waiter.True <- sapply(data$attributes.Waiter.Service, WaitSvc)
#summary(data$attributes.Waiter.True)

# response/predictand: name and distribution by class labels
responseVarName <- "high.rating"
summary(data[ , responseVarName])

# predictors: names
excludeVarNames <- list('X', 'high.rating')

predictorVarNames <- setdiff(colnames(data), excludeVarNames)
print("Predictor Features")
print(paste(predictorVarNames))
print(paste("The number of predictor vars:", length(predictorVarNames)))

# include only predictor features
new_data <- as.matrix(data[ ,predictorVarNames])
t(apply(new_data,2,range))


# First, let's reserve 15% validation and test data for later. -ML

partition_data <- function(data) {
  #set.seed(12)
  all_samples <- 1:nrow(data)
  train_samples <- sample(1:nrow(data), 0.7*nrow(data))
  train_data <- data[train_samples, ]
  
  samples_2 <- all_samples[-train_samples]
  validation_samples <- sample(samples_2, 0.5*length(samples_2))
  validation_data <- data[validation_samples, ]
  
  test_samples <- all_samples[-c(train_samples, validation_samples)]
  test_data <- data[test_samples, ]
  return(list(train=train_data, validation=validation_data, test=test_data))
}

partitioned_data <- partition_data(data)
trainingData <- partitioned_data$train
validationData <- partitioned_data$validation
futureData <- partitioned_data$test

summary(trainingData[,responseVarName])
summary(validationData[,responseVarName])
summary(futureData[,responseVarName])

dim(trainingData)
names(trainingData)

classSelection <- subset(x = trainingData,  
                         TRUE, 
                         select=c(predictorVarNames, responseVarName))
validationSelection <- subset(x = validationData,  
                              TRUE, 
                              select=c(predictorVarNames, responseVarName))

respCol <- ncol(classSelection)
classPredictors <- classSelection[, -respCol]
classResponse <- classSelection[, respCol]
validationPredictors <- validationSelection[, -respCol]
validationResponse <- validationSelection[, respCol]




# Random Forest
rf_model <- randomForest(x=classPredictors, y=factor(classResponse), ntree=10, 
                         nodesize=7, importance=T)
rf_model
rf_predictions <- predict(rf_model, validationPredictors)
rf_confusion_matrix <- table(factor(validationResponse), rf_predictions)
rf_confusion_matrix
varImportance <- importance(rf_model)
#varImportance[1:10, ]
varImportance
# Plot the variable importance as measure by accuracy change
# help(varImpPlot)
varImpPlot(rf_model, type=1)

