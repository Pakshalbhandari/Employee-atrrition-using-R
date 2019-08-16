rm(list=ls())
#Reading the dataset
empdata<-read.csv("E:\\DataScience\\dataset\\employee.csv")
#View(empdata)
str(empdata)
empdata1<-subset(empdata,select=-c(Over18,StandardHours,TrainingTimesLastYear,EmployeeCount))
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
split = sample.split(empdata1, SplitRatio = 0.7)
traindata = subset(empdata1, split == TRUE)
dim(traindata)
testdata = subset(empdata1, split == FALSE)
dim(testdata)


library(C50)
library(ggplot2)
library(caret)
library(descr)
attritionboost <- C5.0(traindata, traindata$Attrition,
                    trials = 10)
summary(attritionboost)

attritionboostprediction <- predict(attritionboost, testdata)
CrossTable(testdata$Attrition, attritionboostprediction,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

Model1 <- C5.0(Attrition ~., traindata)
summary(Model1)
prediction1 <- predict(Model1, testdata)
prediction1


CrossTable(testdata$Attrition, prediction1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
cm=confusionMatrix(data=prediction1,reference = testdata$Attrition)
fourfoldplot(cm$table)

