rm(list=ls())

#Reading the dataset
empdata<-read.csv("E:\\DataScience\\dataset\\employee.csv")
str(empdata)
summary(empdata)
empdata1<-subset(empdata,select=-c(Over18,StandardHours,TrainingTimesLastYear,EmployeeCount))
str(empdata1)
#View(empdata)

table(empdata1$Attrition, empdata1$EnvironmentSatisfaction)

library(ggplot2)
ggplot(empdata1, aes(JobInvolvement, ..count..)) +
  geom_bar(aes(fill = Attrition), position = "dodge")

#ggplot(empdata1, aes(x = JobSatisfaction)) + geom_histogram(aes(fill = Attrition), position = "dodge", binwidth = 0.5)


#ggplot(empdata1, aes(y = YearsInCurrentRole, x = ï..Age )) +  geom_point(aes(colour = Attrition))
#str(empdata1)

#ggplot(empdata1, aes(y = YearsInCurrentRole, x = Attrition)) + geom_boxplot()

#str(empdata1)


library(vcd)

mosaic(~ Attrition + OverTime +MaritalStatus , data = empdata1,
       shade = TRUE, legend = TRUE)



#to check the  missing variables
library(Amelia)
library(mlbench)
missmap(empdata1,col=c("red","blue"),legend=FALSE)

summary(empdata1)

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
set.seed(150)
split = sample.split(empdata1, SplitRatio = 0.70)
traindata = subset(empdata1, split == TRUE)
testdata = subset(empdata1, split == FALSE)


library(klaR)
nb = NaiveBayes(Attrition ~ EnvironmentSatisfaction + YearsInCurrentRole  +
                  JobInvolvement + OverTime + ï..Age, data = traindata)
summary(nb)
predictions = predict(nb, testdata)
predictions
library(caret)
cm=confusionMatrix(testdata$Attrition, predictions$class, positive = "Yes")
str(empdata1)
fourfoldplot(cm$table)
