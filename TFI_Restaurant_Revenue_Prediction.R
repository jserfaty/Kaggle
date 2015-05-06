library(dplyr)
library(caret)
library(caretEnsemble)
library(zoo)

# Load Data
train_raw <- read.csv("~\\Kaggle\\Restaurant Revenue Prediction\\Data\\train.csv") 
test_raw <- read.csv("~\\Kaggle\\Restaurant Revenue Prediction\\Data\\test.csv")

# Calculate Restaurant Age
train_age <- as.yearmon(as.POSIXct("2/28/2015",format="%m/%d/%Y")) - as.yearmon(as.POSIXct(train_raw$Open.Date,format="%m/%d/%Y"))
test_age <- as.yearmon(as.POSIXct("2/28/2015",format="%m/%d/%Y")) - as.yearmon(as.POSIXct(test_raw$Open.Date,format="%m/%d/%Y"))

# Append Age to train/test sets
train <- cbind(train_raw[,-c(1:2)],age = train_age)
test <- cbind(test_raw[,-c(1:2)],age = test_age)

# Train Models
set.seed(1)
control <- trainControl(method="repeatedcv",number=10,repeats=5,savePredictions=TRUE)
model_list <- caretList(
  revenue ~ .
  ,data = train[,-c(1:3)]
  ,trControl = control
  ,metric = "RMSE"
  ,tuneList = list(
    rf = caretModelSpec(method="rf",type="regression")
    ,knn = caretModelSpec(method="knn",type="regression")
    ,svmRadial = caretModelSpec(method="svmRadial")
    ,gaussprRadial = caretModelSpec(method="gaussprRadial")
  )
)

# Summarise Models and Train Ensemble
xyplot(resamples(model_list))
greedy_ensemble <- caretEnsemble(model_list)
summary(greedy_ensemble)

# Make Predictions and Build Submission
submission <- as.data.frame(cbind(test_raw[,1],predict(greedy_ensemble,newdata=test[,-c(1:3)])))
colnames(submission) <- c("Id","Prediction")
write.csv(submission,"submission.csv",row.names=FALSE,quote=FALSE)

# model RMSE=2425622
# Public LB RMSE=1673502