# Title: PCA 
# Author: Keiana Dunn
# Objective:Date: 3/29/19

# Objective: Principal Component Analysis (PCA) is a useful technique in Machine Learning for exploratory data analysis, 
# by revealing hidden patterns in dataset by allowing you to better visualize the variation present in a dataset with many variables. 
# It is unsupervised learning technique. It is particularly helpful in the case of "wide" datasets, where you have many variables 
# for each sample. PCR reduces the noise and dimensions of the data (dimension reduction technique through feature selection and 
# feature extraction). 

# I'm tasked with using PCA on soccer data to: 
# 1) CREATE a Scree Plot in R, 
# 2) CHOOSE an appropriate number of principal components, 
# 3) SELECT an appropriate subset of the features from the original variables,  
# 4) DISCUSS how well (or poorly) I think these new components and features represent the data.

# Results will narrow the features of dataset by finding variables that are strongly correlated. 

#Resources
#http://dataaspirant.com/2017/09/01/perform-principal-component-analysis-r/
#https://www.datacamp.com/community/tutorials/pca-analysis-r#comment-5408

# Steps for PCR analysis:
# 1)Prepare the data :
  #Center the data 
  #Scale the data: If the variances of the variables in your data are significantly different, it’s a good idea to scale the 
  #data to unit variance. 
# 3)Choose principal components : eigenvectors are ordered by eigenvalues from the highest to the lowest. The number of chosen 
  #eigenvectors will be the number of dimensions of the new data set. 
# 4)Calculate the eigenvectors and the eigenvalues 

  
# Question: Which positions brings the most success for the team? 

#Import libraries
install.packages("fs")
library(dplyr)
library(readr)
library(ggplot2) # Plotting graphs


# Load data
soccerdata <- read.csv("/home/ktd2001/Downloads/FIFA 2018 Soccer Player Data.csv", header = TRUE)

# Check for Nas
any(is.na(soccerdata))
#TRUE confirms NAs

# Look at data structure  
str(soccerdata)
# We have intergers, numbers, and factors types for the variables

#Remove all Na
soccerdata<-na.omit(soccerdata)
#Nas were eliminated 

#Check NA status
any(is.na(soccerdata))
#FALSE confirms no Nas 

dim(soccerdata)
#15741 obs    76 variables

#Removing non-numeric features because PCA works best numerical data.
soccerdata<-c(12:45)
soccerdata

# Data check
head(soccerdata)

# Partition Data
set.seed(123)

# Column names
colnames(soccerdata)

# Apply pca using prcomp 
soccer.pca <-prcomp(soccerdata[,c(12:45)],center = TRUE,scale. = TRUE)

# Summary of pca data
summary(soccer.pca)
# 34 principal components. PC1 and PC1 show the highest of total variance at 33% and 18%, 
# which explain 51% of the variance in relation to the other principal components. 
# PCA structure:
# $sdev = standard deviation
# $rotation = correlation between the initial variables and the principal components
# $center = center point
# $scale = scaling
# $x = values of each sample in terms of the principal components 
# #attr = dimension name

#Plot Scree graph 
screeplot(soccer.pca, npcs = 34, type = "lines")
# 34 principal components graphed with individual variance in decreasing order of 
# contribution to total variance. PC! and PC2 are contributing the most and after PC6, there is quick decline and flattens out. 

# Create Biplot
biplot(soccer.pca, scale = 0, choices = 1:2, col = c("grey40", "blue"))
# Biplot shows relationship of variables and prinicipal components
# Eigenvalue and eigenvectors are pairs 
# Eigenvalue and eigenvectors are equal to # of dimensions
# Each observation is plotted against PC1 and PC2. 
# Blue arrows are the eigenvectors for each variable
# The density makes it very difficult to see a pattern in the data
# Eigenvectors are how variables correlate with each another. A small angle considered positive correlation, 
# a large angle considered negative correlation.

# In conclusion, with the distortion in the graph unable to determine which position on the team brought the most success.

