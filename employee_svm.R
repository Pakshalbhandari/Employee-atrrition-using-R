rm(list=ls())
#Reading the dataset
empdata<-read.csv("E:\\DataScience\\dataset\\employee.csv")
empdata1<-subset(empdata,select=-c(Over18,StandardHours,TrainingTimesLastYear,EmployeeNumber,EmployeeCount))
#checking for null values
colSums(is.na(empdata1))
#making missing enteries as NA
for(i in c(1:ncol(empdata1)))
{
  empdata1[i][empdata1[i]==""]<-NA
}

str(empdata1)
library(caTools)
set.seed(123)
split = sample.split(empdata1, SplitRatio = 0.65)
traindata = subset(empdata1, split == TRUE)
dim(traindata)
testdata = subset(empdata1, split == FALSE)
dim(testdata)

library("caret")
ctrl <- trainControl(method="repeatedcv",number = 10,repeats = 3)

grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
library(e1071)
mod_svm = svm(Attrition ~ EnvironmentSatisfaction + YearsInCurrentRole  +
                JobInvolvement + OverTime + ï..Age, data = traindata,tuneGrid = grid,trainControl = ctrl)
str(testdata)
#Prediction now !!!!
#predict(model_trained,testDataSet)
svmPrediction <- predict(mod_svm,testdata[-c(2)])
svmPrediction
confusionMatrix(svmPrediction,testdata$Attrition)

summary(svmPrediction)
summary(testdata$Attrition)

