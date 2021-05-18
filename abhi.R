getwd()

train<-read.csv("train.csv",header = TRUE)
test<-read.csv("test.csv",header = TRUE)
#install.packages("dplyr")
library(dplyr)
summary(test)

val<-test[,-1]
val<-val[,-7]
val<-scale(val)
summary(val)
colMeans(val)
#setwd("C:/Users/Dell/Desktop")
#getwd
train$Unique.ID<-NULL
train1<-train[,-7]
#View(train1)
train1<-scale(train1)

colMeans(train1)
outcome<-train$outcome
train<-cbind(train1,outcome)

summary(train)
#xtabs(~admit + rank, data = mydata)
# Check number of rows in training and validation data sets
nrow(train)
nrow(val)
class(train)
train<-as.data.frame(train)
#Run Logistic Regression
mylogistic <- glm(formula=outcome ~ ., data = train, family = "binomial")
summary(mylogistic)$coefficient


#Stepwise Logistic Regression
mylogit = step(mylogistic)

#Logistic Regression Coefficient
summary.coeff0 = summary(mylogit)$coefficient

#Calculating Odd Ratios
OddRatio = exp(coef(mylogit))
OddRatio
predi<-mylogit$fit>0.5
#Confusion Matrix - Training
ct<-table(train$outcome,predi)
ct
cat("columns are observed and rows are predicted \n")

#percentage of misclassification
(ct[1,2]+ct[2,1])/sum(ct)

#sensitivity
1-ct[2,1]/sum(train$outcome == 1)

#specificity
1-ct[1,2]/sum(train$outcome == 0)

#Confusion Matrix - Testing
pred<-predict(mylogit, newdata= test, type = "response")
pred<-pred>0.5
ct<-table(test$Outcomes..Default...1,pred)
ct
cat("columns are observed and rows are predicted \n")

#percentage of misclassification - test data
(ct[1,2]+ct[2,1])/sum(ct)

#sensitivity
1-ct[2,1]/sum(test$Outcomes..Default...1 == 1)

#specificity
1-ct[1,2]/sum(test$Outcomes..Default...1 == 0)



install.packages("caTools")    # For Logistic regression
install.packages("ROCR")       # For ROC curve to evaluate model
