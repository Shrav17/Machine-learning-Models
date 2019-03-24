data <- read.csv("dataset.csv")

#Removing the Unique value column
#nearZeroVar(data)
data <- data[,-c(1,22)]

#Encoding Target variable as factor
data$Result<- gsub("-1", "0", data$Result)#Phishing
data$Result<- gsub("1", "1", data$Result)
data$Result <- as.factor(data$Result) #Change from character to Factor


#Splitting the dataset into training and testing
set.seed(42)
split = sample.split(data$Result, SplitRatio = 0.70)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

install.packages("rotationForest")
library(rotationForest)

rot <- rotationForest(train[,-30],
                      train[,30], 
                      K = round(ncol(train[,-30])/5,0), 
                      L=10,
                      verbose = FALSE)



testing_predict <-predict(rot, newdata = test[,-30])
testing_predict[testing_predict < 0.5] <- 0
testing_predict[testing_predict > 0.5] <- 1
testing_predict<- factor(testing_predict)


# Making the Confusion Matrix
cm = table(test[,30], testing_predict)
confusionMatrix(testing_predict, test[,30])

