############ Importing the required libraries
library(dplyr)
library(ggplot2) #for the exploratory data analysis
library(stringr)
library(ROCR) # ROCR model validation
#Decision Tree Libraries
library(irr)
library(rpart)
library(caret)
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)

##Random Forest Libraries
library(randomForest)


setwd("D:\\loan_prediction")

getwd()

TTrain<-read.csv("train_u6lujuX_CVtuZ9i.csv",header = T,sep = ",",na.strings = "")
TTest<-read.csv("test_Y3wMUE5_7gLdaTN.csv",header = T,sep = ",",na.strings = "")

TTrain$Set<-"Train"
TTest$Set<-"TTest"

TTest$Loan_Status<-NA

Full<-rbind(TTrain,TTest)


########### Exploratory data analysis
str(Full)
summary(Full)

#Gender NAs 24
#Married NA 3
#Dependent NA 25
#self employed NAs 55
#Loan amount NAs 27
#Loan_Amount_term NAs 20
#Credit_History NAs 79
