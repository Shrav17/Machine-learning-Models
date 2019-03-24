#----------------------Logistic Regression---------------------------

data <- read.csv("Social_Network_Ads.csv")
View(data)

#Concentration on the colums which is useful
data <- data[,3:5]
View(data)
#Splitting the dataset for testing and training

library(caTools)
set.seed(123)

split <- sample.split(data$Purchased, SplitRatio = 0.75)

training <- subset(data,split == TRUE)
testing <- subset(data,split == FALSE)

#Feature Scaling
training[,1:2] <- scale(training[,1:2])
testing[,1:2] <- scale(testing[,1:2])

#Fitting logistic Regression to the training set
#Classifying depended variable "Purchased" by considering all other "Independent Variable"
classifier <- glm(formula = Purchased ~ .,
                   family = binomial,
                   data = training)

#Predicting the testing set results
#We are predicting testing data using the classifier which Logistic regression classifier
probalistic_pred <- predict(classifier,type = 'response',newdata = testing[-3])

y_probalistic_pred <- ifelse(probalistic_pred >0.5 , 1 ,0)

#Making the Confusion matrix 
#Matrix of results from the classifier with the actual outcomes of the dataset
Conf_matirx <- table(testing[,3],y_probalistic_pred)

#Visualizations the training set results
install.packages("ElemStatLearn")
library(ElemStatLearn)

set = training
X1 <- seq(min(set[,1])-1,max(set[,1])+1,by =0.01)
X2 <- seq(min(set[,2])-1,max(set[,2])+1,by =0.01)

grid_set <- expand.grid(X1,X2)
colnames(grid_set) <-c('Age','EstimatedSalary')

prob_set <- predict(classifier,type = 'response',newdata = grid_set)
y_grid <- ifelse(prob_set>0.5,1,0)
plot(set[,3],
     main = "Logistic regression (Training set)",
     xlab='Age',ylab ='Estimated Salary',
     xlim = range(X1),ylim = range(X2))
       
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3'))

#Visualizations the testing set results
install.packages("ElemStatLearn")
library(ElemStatLearn)

set = testing
X1 <- seq(min(set[,1])-1,max(set[,1])+1,by =0.01)
X2 <- seq(min(set[,2])-1,max(set[,2])+1,by =0.01)

grid_set <- expand.grid(X1,X2)
colnames(grid_set) <-c('Age','EstimatedSalary')

prob_set <- predict(classifier,type = 'response',newdata = grid_set)
y_grid <- ifelse(prob_set>0.5,1,0)
plot(set[,3],
     main = "Logistic regression (Testing set)",
     xlab='Age',ylab ='Estimated Salary',
     xlim = range(X1),ylim = range(X2))

contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3'))
