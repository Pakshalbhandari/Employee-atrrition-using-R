rm(list=ls())
#Reading the dataset
empdata<-read.csv("E:\\DataScience\\dataset\\employee.csv")

empdata1<-subset(empdata,select=-c(Over18,StandardHours,TrainingTimesLastYear,EmployeeCount))
#checking for null values
colSums(is.na(empdata1))
#making missing enteries as NA
for(i in c(1:ncol(empdata1)))
{
  empdata1[i][empdata1[i]==""]<-NA
}

str(empdata1)

#splitting the data into train and test 
library(caTools)
set.seed(123)
split = sample.split(empdata1, SplitRatio = 0.7)

traindata = subset(empdata1[], split == TRUE)
testdata = subset(empdata1, split == FALSE)


library(randomForest)
str(traindata)

besttry<- tuneRF(traindata[-2],traindata$Attrition,stepFactor = 1.5,improve = 0.01,trace = T,plot = T)
model_forest <- randomForest(Attrition ~ . -Attrition, data = traindata,mtry = 7,ntree = 250)
model_forest
predict<-predict(model_forest,testdata[-2])
predict
library(caret)
install.packages("data.table")
cm=confusionMatrix(data=predict,reference=testdata$Attrition)
cm
importance(model_forest)
varImpPlot(model_forest)
plot(model_forest)
getTree(model_forest, 1, labelVar=TRUE)

