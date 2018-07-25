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


plot(Full[Full$Credit_History==1,]$Loan_Status~Full[Full$Credit_History==1,]$Property_Area)
plot(Full[Full$Credit_History==0,]$Loan_Status~Full[Full$Credit_History==0,]$Property_Area)


Full$Loan_Status<-ifelse(Full$Loan_Status=='Y',1,0)
Full$Loan_Status<-as.factor(Full$Loan_Status)

Full$Credit_History_Y<-ifelse(Full$Credit_History==1,1,0)
Full$Credit_History_N<-ifelse(Full$Credit_History==0,1,0)
Full$Credit_History_M<-ifelse(is.na(Full$Credit_History),1,0)
Full[Full$Credit_History_M==1,c("Credit_History_Y","Credit_History_N")]<-0

Full$Property_Area_R<-ifelse(Full$Property_Area=="Rural",1,0)
Full$Property_Area_S<-ifelse(Full$Property_Area=="Semiurban",1,0)
Full$Property_Area_U<-ifelse(Full$Property_Area=="Urban",1,0)


table(Full$Credit_History,Full$Loan_Status)
table(Full$Property_Area,Full$Loan_Status)



plot(as.factor(Full$Credit_History),Full$Loan_Status)
plot(Full$Property_Area,Full$Loan_Status)


plot(Full$Gender,Full$Loan_Status)
#gender not an important variable

plot(Full$Married,Full$Loan_Status)
#married can be used

plot(Full$Dependents,Full$Loan_Status)
#dependends can be used

plot(Full$Self_Employed,Full$Loan_Status)
#self employed is a bad factor cannot be used

Full$Total_Income<-Full$ApplicantIncome+Full$CoapplicantIncome

boxplot(Full$Loan_Status,Full$Total_Income)

ggplot(Full,aes(y=Total_Income))+geom_boxplot()+facet_grid(.~Loan_Status)
############### Data Preparation



#################### Spliting Train, Validation and Test DataSet
set.seed(100)
index<-sample(x = 1:nrow(TTrain),size = 0.8*nrow(TTrain),replace = F)
Train<-Full[Full$Set=="Train",]

Validate<-Train[-index,]
Train<-Train[index,]
Test<-Full[Full$Set=="TTest",]

#################### Applying Decision Tree Model for initial understanding of Parameters
names(Train)
mod<-rpart(Loan_Status~Total_Income+Property_Area_U+Property_Area_R+Property_Area_S+Credit_History_M+Credit_History_N+Credit_History_Y+ApplicantIncome+CoapplicantIncome,data=Train,control=rpart.control(cp=0.002,maxdepth=7),method="class",parms=list(split="gini"))

mod
#Visualization of Model
fancyRpartPlot(mod)
summary(mod)
printcp(mod)
plotcp(mod, minline = TRUE)

### Model Pruning
mod1<-prune(mod,cp= 0.008)
fancyRpartPlot(mod1)
#### Model Accuracy on the Train Data Itself
actual<-Train$Loan_Status
predicted<-predict(mod1,type = "class")
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

################ Testing on the Validation DataSet

actual<-Validate$Loan_Status
predicted<-predict(mod1,type = "class",newdata = Validate)
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)








########################### Using the Logistic Regression Model
######## Logistic regression


mod1<-glm(formula = Loan_Status~Total_Income+Property_Area_U+Property_Area_R+Property_Area_S+Credit_History_M+Credit_History_N+Credit_History_Y+ApplicantIncome+CoapplicantIncome, family = "binomial", data = Train)
mod1
summary(mod1)
#### Model Accuracy on the Train Data Itself
actual<-Train$Loan_Status
pred<-predict(mod1,type="response")

table(Train$Loan_Status)/nrow(Train)

predicted<-ifelse(pred>0.31,1,0)
actual<-as.factor(actual)
predicted<-as.factor(predicted)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

################ Testing on the Validation DataSet

actual<-Validate$Loan_Status
predicted<-predict(mod1,type = "response",newdata = Validate)

predicted<-ifelse(predicted>0.31,1,0)
actual<-as.factor(actual)
predicted<-as.factor(predicted)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)




########################################### Using Random Forest Algorithm

model1 <- randomForest(formula = Loan_Status~Total_Income+Property_Area_U+Property_Area_R+Property_Area_S+Credit_History_M+Credit_History_N+Credit_History_Y+ApplicantIncome+CoapplicantIncome,ntry=500, data = Train, importance = TRUE)
model1
a=c()
i=5
for (i in 3:6) {
  model3 <- randomForest(formula = Loan_Status~Total_Income+Property_Area_U+Property_Area_R+Property_Area_S+Credit_History_M+Credit_History_N+Credit_History_Y+ApplicantIncome+CoapplicantIncome,ntree = 500,ntree = 500,mtry=i, data = Train, importance = TRUE)
  predValid <- predict(model3, Validate, type = "class")
  a[i-2] = mean(predValid == Validate$Loan_Status)
}

a

plot(3:6,a)


######max accuracy at mtry=3
model1 <- randomForest(formula = Survived~Sex_Male+Sex_Female+Pclass_1+Pclass_2+Pclass_3+Age+SibSp+Ticket_count,ntree = 500,mtry=3, data = Train, importance = TRUE)
model1


#### Model Accuracy on the Train Data Itself
actual<-Train$Loan_Status
predicted<-predict(model1,type = "class")
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

################ Testing on the Validation DataSet

actual<-Validate$Loan_Status
predicted<-predict(model1,type = "class",newdata = Validate)
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)


