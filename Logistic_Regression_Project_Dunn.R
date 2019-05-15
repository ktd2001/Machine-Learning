# Title: Logistic Regression 
# Author: Keiana Dunn
# Objective:Date: 5/8/19

# Objective: To better understand how Machine Learning algorithms work, write logistic regression algorithm
# that uses supervised learning to measure the relationship between the categorical dependent bariable and one 
# or more independent variables. Also it is a way to deal with outlier data.

# Resources used:
# http://enhancedatascience.com/2018/01/30/your-own-machine-learning-library-from-scratch-with-r/
# https://www.datacamp.com/community/tutorials/logistic-regression-R
  
#Import packages
library(ggplot2) # Used for plotting graphs
library(caTools) # Used for splitting data set
library(ROCR) # Used for plotting curve 


LogData <- read.csv("/home/ktd2001/Downloads/LogisticData_2.csv", header = TRUE)

#Retrieve the dimension of the dataframe
dim(LogData)
#1000 obs of 3 variables

LogData[1:3]=lapply(LogData[1:3], as.numeric)
#is.na(ProfData)
#which(is.na(ProfData))

set.seed(123)
# Split the data set to have training and test data
split = sample.split(LogData$X1, SplitRatio = 0.8)

training= subset(LogData, split == TRUE)
dim(training)
#800 observation  and 3 variables
test= subset(LogData, split == FALSE)
dim(test)
#200 observation  and 3 variables
head(training)
#X1         X2 Y
#1 -1.0144669 -2.1705477 0
#2 -0.3581296 -0.4525050 0
#3 -0.7487142  1.4036782 1
#6 -1.1590148 -0.1033413 0
#7 -0.3254720 -1.1978170 0
#9 -0.4366895 -0.4290545 0

#Create matrices
x = as.matrix(training[-3])
x = cbind(x,intercept=1)
y = as.matrix(training[3])

# Building Logistic Regression model using R code
model <- glm(Y~.,training, family='binomial')
summary(model) 
#Y~. our response variable, . is our predictor bariables  = The training dataset   
#Now we have coefficients BoB1 or the weights
  
# predicitng Y
response <- predict(model, test, type = "response")
#structure response gives "Y" prediction values of test dataset
str(response)

#Create confusion matrix with 50% threshold
confusionMatrix <- table(ActualValue=test$Y, PredictedValue=response>0.5)
confusionMatrix
#Model has predicted true positives = 48
# true negative = 128, false positives = 9 and false negatives = 48
# out of a total 200 Y test data.

# Commute model accuracy using 50% threshold
accuracy <-confusionMatrix
accuracy<- accuracy[1]+ accuracy[4]/sum(accuracy)
accuracy*100
# The accuracy is way off becuase can not be 128%. The model accuracy is underdetermined. I will see how 
# well the ROC curve plots accuracy.

#Plot ROCR Curve
response <- predict(model, training, type = "response")
ROCR_Prediction(response, traning$Y)
ROCR_Performance< performance(ROCR_Prediction,"tpr", "fpr")
plot(ROCR_Performance, colorize=TRUE, print.cutoffs.at=seq(0.1,by=0.1))


#The sigmoid function
#sigmoid = function(x)
  {
    x = 1/(1+exp(a)
    return(exp(X%*%a) / (1+ exp(X%*%a)))
  } 
  
# I planned to create the regression function code from scratch, function for matrix multiplication, gradient decent and test the function but ran out of time. 

