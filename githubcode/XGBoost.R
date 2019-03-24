#-------------------------------- Extreme Gradient Boosting (XGBoost) ---------------------------------------------
#XGBoost used on Classification

data <- read.csv("pulsar.csv")

str(data)

#Converting the Categorical Variable into Factor

data$target_class <- factor(data$target_class)

#Splitting the dataset into training data and testing data

library(caTools)
set.seed(1337)

#spliting the data with 70 for training and 30 for testing

split <- sample.split(data,SplitRatio = .70)

training <- subset(data,split == TRUE)
test <- subset(data,split==FALSE)

#xgboost function takes the parameter with matrix
#removing the target varibable and converting the dataset into matrix
training_matrix <- as.matrix(training[-9])
test_matrix <- as.matrix(test[-9])

#Taking the target class variable in the matirx form so that we can have the comparison from the output for training matrix
# and compare with training_labels

training_labels <- as.matrix(training$target_class)
test_labels <- as.matrix(test$target_class)

#applying xgboost mentioning the number of rounds it has to consider
install.packages("xgboost")
library(xgboost)
xgbmodel <- xgboost(data=training_matrix,label = training_labels,nrounds =100)

#Calculating the prediction accuracy of the xgmodel 
#If we gett 0 error in training then it will be over fitting problem
training_predict <- predict(xgbmodel,newdata = training_matrix)
training_predict[training_predict < 0.5] <- 0
training_predict[training_predict > 0.5] <- 1

train_accuracy <- 100 * length(which(training_predict== training_labels))/length(training_labels)

#Running prediction on Testing data
testing_predict <- predict(xgbmodel,newdata = test_matrix)
testing_predict[testing_predict < 0.5] <- 0
testing_predict[testing_predict > 0.5] <- 1

test_accuracy <- 100 * length(which(testing_predict== test_labels))/length(test_labels)



