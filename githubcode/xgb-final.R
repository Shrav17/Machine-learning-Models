library(readxl)
library(tidyverse)
library(xgboost)
library(caret)

data <- read.csv("dataset.csv")
data <- data[,-c(1,22)]
data$Result <- as.factor(data$Result)

set.seed(100)  # For reproducibility
# Create index for testing and training data
inTrain <- createDataPartition(y = data$Result, p = 0.8, list = FALSE)
# subset data to training
training <- data[inTrain,]
# subset the rest to test
testing <- data[-inTrain,]

train_matrix = xgb.DMatrix(as.matrix(training %>% select(-Result)))
train_label = training$Result

test_matrix = xgb.DMatrix(as.matrix(testing %>% select(-Result)))
test_label = testing$Result

xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)


xgbGrid <- expand.grid(nrounds = c(100,200),
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)


set.seed(0) 
xgb_model = train(
  train_matrix, train_label,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

predicted = predict(xgb_model, test_matrix)

confusionMatrix(test_label, predicted)

test_accuracy <- 100 * length(which(predicted== test_label))/length(test_label)
test_accuracy
#97.73
par(mfrow = c(2, 2))
plot(xgb_model)

xgb_model$bestTune

options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = y_test))
plot(my_data)

xgb.plot.deepness(xgb_model)
