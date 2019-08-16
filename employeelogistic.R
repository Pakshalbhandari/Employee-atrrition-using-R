rm(list=ls())


#Reading the dataset
empdata<-read.csv("E:\\DataScience\\dataset\\employee.csv")
empdata1<-subset(empdata,select=-c(Over18,StandardHours,EmployeeNumber,TrainingTimesLastYear))

#checking for null values
colSums(is.na(empdata1))

#making missing enteries as NA
for(i in c(1:ncol(empdata1)))
{
  empdata1[i][empdata1[i]==""]<-NA
}

str(empdata1)
#graph
library(ggplot2)
ggplot(empdata1, aes(y = ï..Age , x = Attrition)) +
  geom_violin(scale = "count", color = "Blue")


ggplot(empdata1, aes(x = YearsAtCompany)) +
  geom_histogram(aes(fill = Attrition), position = "dodge", binwidth = 0.5)

n
#splitting the data into train and test 
library(caTools)
set.seed(123)
split = sample.split(empdata1, SplitRatio = 0.70)
traindata = subset(empdata1, split == TRUE)
testdata = subset(empdata1, split == FALSE)

#viewing the test and train data
View(testdata)

#checking the levels of the features
str(testdata)

#Building the model for logistic regression using train data
model<-glm(formula = Attrition ~ï..Age + BusinessTravel + Department + DistanceFromHome + 
             Education + EnvironmentSatisfaction + Gender + JobInvolvement + 
             JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + 
             OverTime + RelationshipSatisfaction + TotalWorkingYears + 
             WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + 
             YearsWithCurrManager,family = binomial(link='logit'),traindata)

#prediction of employee attrition for test data
predict<-predict(model,testdata,type = "response")
predict
summary(predict)
result<-as.factor(ifelse(predict>0.5,1,0))
step(model)

#code to change yes as 1 and no as 0 for confusion matrix
library(plyr)
testdata$Attrition <- revalue(testdata$Attrition, c("Yes"=1))
testdata$Attrition <- revalue(testdata$Attrition, c("No"=0))

#to check the accuracy

library(caret)
cm=confusionMatrix(data=result,reference = testdata$Attrition)
str(testdata$Attrition)
str(result)

fourfoldplot(cm$table)

plot(empdata1$BusinessTravel)
