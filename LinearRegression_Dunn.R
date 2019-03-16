

# Title: "Linear Regression R NB"
# Author: Keiana Dunn
# Objective:Date: 3/16/19

# Objective: To better understand how Machine Learning algorithms work, write linear regression algorithm that is able 
# to print coefficients and accuracy metrics, plot actual versus predicted response variables, and compare R^2 results of algorithm to R^2 resluts using lm() and predict(). 

# In order to have linear regression model there should be a correlation between the data and the data should be linear. 

#Resources used:
#https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
#https://github.com/capt-calculator/linear-regression-from-scratch-r/blob/master/linear_regression_scratch.R
#https://www.quora.com/What-is-the-difference-between-matrix-inverse-and-matrix-transpose
#https://www.riskprep.com/component/exam/?view=exam&layout=detail&id=131
#https://swcarpentry.github.io/r-novice-inflammation/15-supp-loops-in-depth/
#https://www.statmethods.net/stats/regression.html
  
  
#Import packages
library(ggplot2) # Plotting graphs
library(caTools) # Can split data into testiing and training set 

ProfData <- read.csv("/home/ktd2001/Downloads/TrainData_Group1.csv", header = TRUE)

#Retrieve the dimension of the dataframe
dim(ProfData)
#1000 obs of 6 variables; X1-X5 and Y

#Setting seed to reproduce same samples everytime
set.seed(123)

#Divide the data into training and testing data:
#Training and Testing - Random Sample (using sample())
samples <- sample(1:nrow(ProfData), size=0.8*nrow(ProfData))
#Name the value sample so it does not overlap with sample()
training <- ProfData[samples,] #Take 800 rows or 80%
testing <- ProfData[-samples,] #Take the rest of the rows (200)

dim(training)
#800 obs of 6 variables

dim(testing)
#200 obs of 6 variables

rm(samples) #Just to keep environment clean

summary(training)
#Summary of data and checking for missing values

#Quick plot just to see what the data looks like:
x <- cbind(training$X1,training$X2,training$X3,training$X4, training$X5)
y <- cbind(training$Y,training$Y,training$Y,training$Y, training$Y)
matplot(x,y,type="p",main= "Professor Data")


#Separate X and Y variables of training and testing data for analysis
trainingX  <- subset(training , select = -c(Y) )
#Select all columns except the Y column - This will make up the X values for training.
trainingY  <- subset(training , select = c(Y) )
#Select only the Y column of training sample

testingX  <- subset(testing , select = -c(Y) )
#Select all columns except the Y column - This will make up the X values for testing.
testingY  <- subset(testing , select = c(Y) )
#Select only the Y column of testing sample

#Training Model using Training data 
training_R <- function(x, y) { #A function of only x and y data
  intercept <- rep(1,nrow(x)) #To add an extra column of all 1's for the same amount of rows in x data
  x <- cbind(intercept, x) #Insert intercept column to X
  #From all rows i of training(x) data and for k variables
  matrix_X <- as.matrix(x) #Create X matrix from x data
  vector_Y <- as.matrix(y) #Create Y matrix/vector from y data
  beta <- solve(t(matrix_X) %*% matrix_X) %*% t(matrix_X) %*% vector_Y #The Y matrix is actually a vector  
  #t=transpose as a matrix or take the inverse since they are of equal values, goal is to multiply the original matrix by the transposed version
  #to retrieve the inverse. 
  #The beta is the result from the matrix multiplication - %*% must be used because we want the product of matrices, not standard multiplcation.
  beta <- round(beta, 2) #Round to 2 decimal places
  message(paste("Beta:", beta)) #Print/paste text in addition to the beta numerical values when you run the function in the console
  return(beta) #Solves for the β vector
} #Ends training_R

#Make predictions using Test data 
#Similar to training model, but now we use the beta that we solved for in the training to get Y^
testing_R <- function(x,beta){
  beta_matrix <- t(as.matrix(beta)) #Transpose beta matrix
  intercept <- rep(1,nrow(x)) #To add an extra column of all 1's for the same amount of rows in x data
  x <- cbind(intercept, x) #Combine intercept with x as the new x
  matrix_X <- t(as.matrix(x)) #Transpose x data matrix
  y.hat <- beta_matrix %*% matrix_X #Ycarrot/hat - Multiply the beta matrix by the x data matrix
  #The data used is the training data to retrieve Y^
  message(paste("Y^:", y.hat))#Print/paste text in addition to the RSS numerical value when you run the function in the console
  return(y.hat) #Y^ is returned from the function
} #Ends testing_R

#Compute the beta using the training X and Y data using the function for training regression that we just created:
beta <- training_R(trainingX, trainingY)
dim(beta) #Results are 6 values of 
print(beta)
#           Y
#intercept  2.14
#X1         0.02
#X2         0.00
#X3        -1.31
#X4         0.40
#X5         2.08

#Compute the Y^ with testing regression function using the X test data and the beta values retrieved from the training regression
y.hat <- testing_R(testingX, beta)
dim(y.hat) #Results are 200 values from the test regression
print(y.hat)
#Results printed in console

#Model Testing: Compare computed results with R's built-in function, compute R^2
errors <- function(Y, y.hat_data){ #Even though we need to use y data and y.hat data, 
  #we can not use the same variables as before or the function will produce errors and use the wrong values.
  #Make sure the Y data is being read as the matrix(vector) like vector_Y
  Y <- as.matrix(Y)
  y.hat_data <- t(as.matrix(y.hat_data))
  #Understanding relationship of R^2/RSS/TSS: 
  RSS <- 0 #Residual Sum of Squares
  #res <- numeric(length = length(y)) 
  for (y in seq_along(Y)) { #?seq_along ^^^ - To be used for matrices
    RSS = RSS + (Y[y,1]-y.hat_data[y,1])**2 #Take the matrix y.hat values and subtract it from the Y data and square the result
  }#Error in Y[Y, 1] : only 0's may be mixed with negative subscripts
  #Error in Y[Y, 1] : incorrect number of dimensions
  message(paste("RSS:", RSS)) #Print/paste text in addition to the RSS numerical value when you run the function in the console
  y_hat_real <- mean(y.hat_data) #Retrieve the mean value of the Y^ generated values
  TSS <- 0 #The Total Sum of Squares
  #Y <- numeric(length = length(y)) 
  for (y in seq_along(Y)){ #Similar to above
    TSS = TSS + (Y[y,1]-y_hat_real)**2 #The actual value of y minus the mean, squared
  }
  message(paste("TSS:", TSS)) #Print/paste text in addition to the TSS numerical value when you run the function in the console
  r_squared <- 1 - (RSS/TSS) #Slide#58, 1 minus the RSS value divided by the TSS value
  message(paste("R^2:", r_squared)) #Print/paste text in addition to the R^2 numerical value when you run the function in the console
}#Ends errors function

errors(testingY,y.hat)
#Results printed in console:
#RSS: 61.9839312125315
#TSS: 7099.38355004368
#R^2: 0.991269110793127

#Before we create the function it was easier to understand the R results using the built in lm function with our data:
R_version_lm <- lm(formula = Y ~ X1 + X2 + X3 + X4 + X5, data = ProfData) 
summary(R_version_lm)
#The summary will produce residuals, various coefficients/errors at the intercepts, residual standard error, multiple
#R-squared, adjusted R-squared, F-statistic and a p-value.
?lm
?predict.lm #Function to produce predicted values used with regression

errorANDresults <- function(alldata){
  #The formula for lm is found in the powerpoint and also the resource on the next line. We have 5 X columns so we will use X1-X5, the
  #Y will depend~ on the X columns
  R_version_lm <- lm(formula = Y ~ X1 + X2 + X3 + X4 + X5, data = alldata) 
  #We know what these results look like because of summary(R_version_lm) used above
  R_version_prediction <- predict.lm(R_version_lm, newdata=NULL, type="response") 
  ProfData_Y <- ProfData[6] #All of the Y data from the original dataset (1,000obs)
  ProfData_Y <- as.matrix(ProfData_Y) #Read it in as a matrix to obtain lm R^2 
  y.hat_R <- as.matrix(R_version_prediction) #Y^ results from R's functions
  RSS <- 0 #Residual Sum of Squares
  for (y in seq_along(ProfData_Y)){ #Same as above
    RSS = RSS + (ProfData_Y[y,1]-y.hat_R[y,1])**2
  }
  message(paste("RSS:", RSS))
  y_hat_real <- mean(y.hat_R) #Take the mean of the Y^ R version matrix
  TSS <- 0 #Total Sum of Squares
  for (y in seq_along(ProfData_Y)){ #Same as above
    TSS = TSS + (ProfData_Y[y,1]-y_hat_real)**2 #The actual value of y minus the mean, squared
  }
  message(paste("TSS:", TSS))
  lm_r_squared <- 1 - (RSS/TSS) #R^2 is 1 minus the RSS divided by the TSS from above
  message(paste("lm R^2:", lm_r_squared))
}#Ends errorANDresults function

errorANDresults(ProfData)
#RSS: 374.549376547271
#TSS: 37092.1767280438
#lm R^2: 0.989902200151438

print(errors(testingY,y.hat), errorANDresults(ProfData))
#RSS: 61.9839312125315     RSS: 374.549376547271
#TSS: 7099.38355004368     TSS: 37092.1767280438
#R^2: 0.991269110793127 lm R^2: 0.989902200151438

#In conclusion, both R^2 values from the regression from scratch and also using he built in functions with R are close to 
#1 which means they values are associated, but are also very close to each other. 
#It is interesting to see how different the RSS/TSS values are but the R^2 value is still extremeley close.
#We now need to graph the association:

"The plotting refers to the actual Y values against the predicted Y values"
testingY <- as.matrix(testingY) #Actual Y values
PY <- testing_R(testingX, beta) #Name the results something different this time.
PY <- as.matrix(PY) #..because they need to be a matrix; Predicted Y values, #Does not work if they are not matrices
plot(x=testingY, y=PY, col=2, pch=1, font.lab=2, main="Compare Actual Testing Y Values to Predicted Values",xlab = "Testing Y Values", ylab="Predicted Values")
#The results are linear with a few outliers. The outliers are to blame for the R^2 results not being even closer to 1.

