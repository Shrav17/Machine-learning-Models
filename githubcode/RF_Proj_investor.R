#if Class Imbalance  is present then c50, decision tree use, Markov chain
getwd()
setwd("D:/National College of Ireland/Sem-2/CRm")

#reading csv file
data <- read.csv("investorbig.csv")

#Unique  column is removed
data <- data[,-c(1,15,16)]

#Summary of dataset
str(data)

#Converting target varibale to factor, and the independent factor values to 
data$investor_status <- as.factor(data$investor_status)

#Checking for the variance
nearZeroVar(data)

#Class imbalance 
barplot(table(data$investor_status))

View(data)
#Stratification
#stratified("dataset","Column number to be stratified","number of rows to be considered of each value in the target")
library(splitstackshape)
strat_data <- stratified(data, group = 1, 3200) 

write.csv(strat_data,"stratdata2.csv")

table(strat_data$investor_status)

barplot(table(strat_data$investor_status))

#Benchmark model
No_invest <- rep(0,length(strat_data$investor_status))
No_invest <- as.factor(No_invest)

#Checking Accuracy of bench mark model
library(caret)
caret::confusionMatrix(No_invest,strat_data$investor_status)

#Accuracy : 0.7019 , Kappa : 0

#Creation of training set and test set
library(caTools)
set.seed(42)
split = sample.split(strat_data$investor_status, SplitRatio = 0.70)
training_set = subset(strat_data, split == TRUE)
test_set = subset(strat_data, split == FALSE)

length(training_set)

class(training_set)

#Class value for the training set and test set was "data.table" and "data.frame"
training_set <- as.data.frame(training_set)
test_set <- as.data.frame(test_set)

#Checking for the instances , how they are distributed
table(training_set$investor_status)
table(test_set$investor_status)

#Building Random Forest Model as there is huge imbalance data
library(randomForest)
classifier = randomForest(x = training_set[,-1],
                          y = training_set$investor_status,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[,-1]) 


#Making the Confusion Matrix
confusionMatrix(test_set[,1],y_pred)

#Accuracy : 1
