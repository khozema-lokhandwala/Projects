getwd()
setwd("~/Desktop/IE 7275 Data mining/Homework 4")
library(psych)
require(psych)
install.packages("MASS")
library(MASS)
require(MASS)
library(graphics)
require(graphics)
library(parallel)
require(parallel)
install.packages("readxl")
require(readxl)
install.packages("rgl")
library("rgl")
require(rgl)
install.packages("corrplot")
require(corrplot)
install.packages("dplyr")
library(dplyr)
require(dplyr)
install.packages("clusterSim")
library(clusterSim)
require(clusterSim)
install.packages("caret")
library(caret)
require(caret)
install.packages("class")
library(class)
require(class)
require(recommenderlab)

UB<-read_excel("Universal Bank.xlsx")
UB$edu1<- UB$Education
UB$edu2<- UB$Education
UB$edu3<- UB$Education
UB$edu1<- replace(UB$edu1,UB$edu1 == 2 , 0)
UB$edu1<- replace(UB$edu1,UB$edu1 == 3 , 0)

UB$edu2<- replace(UB$edu2,UB$edu2 == 1 , 0)
UB$edu2<- replace(UB$edu2,UB$edu2 == 3 , 0)
UB$edu2<- replace(UB$edu2,UB$edu2 == 2 , 1)

UB$edu3<- replace(UB$edu3,UB$edu3 == 2 , 0)
UB$edu3<- replace(UB$edu3,UB$edu3 == 3 , 0)
UB$edu3<- replace(UB$edu3,UB$edu3 == 3 , 1)

UB$Education<- NULL

Unibank<- UB[,-c(1,5)]
Unibank

#SAMPLING DATA
set.seed(10)
n<- nrow(Unibank)
indices <- sort(sample(1:n, n * 0.6))
indices
UBtraining<- Unibank[indices,]
UBtest<- Unibank[-indices,]

#PREDICTING NEW RECORD
customer1<- data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2, Mortgage=0,`Personal Loan`= 1, `Securities Account`=0, `CD Account`=0, Online=1,Creditcard = 1,edu1= 0,edu2=1, edu3=0 )
predcust1<- knn(UBtraining, customer1, UBtraining$`Personal Loan`, k = 1)
predcust1

#PLOTTING ACCURACY VS K CURVE FOR CHOOSING BEST K
accuracy <- rep(0, 5)
k <- 1:5
for(x in k){
  UBprediction <- knn(UBtraining, UBtest, UBtraining$`Personal Loan`, k = x)
  accuracy[x] <- mean(as.numeric(as.character(UBprediction)) == UBtest$`Personal Loan`)
}

plot(k, accuracy, type = 'b')

#With k=1
UBknn1 <- knn(UBtraining,UBtest,UBtraining$`Personal Loan`,k=1)
UBknn1
table(UBknn1,UBtest$`Personal Loan`)
mean(as.numeric(as.character(UBknn1))==UBtest$`Personal Loan`)

#With k=2
UBknn2 <- knn(UBtraining,UBtest,UBtraining$`Personal Loan`,k=2)
UBknn2
table(UBknn2,UBtest$`Personal Loan`)
mean(UBknn2==UBtest$`Personal Loan`)

#With k=3
UBknn3 <- knn(UBtraining,UBtest,UBtraining$`Personal Loan`,k=3)
UBknn3
table(UBknn3,UBtest$`Personal Loan`)
mean(UBknn3==UBtest$`Personal Loan`)

#With k=4
UBknn4 <- knn(UBtraining,UBtest,UBtraining$`Personal Loan`,k=4)
UBknn4
table(UBknn4,UBtest$`Personal Loan`)
mean(UBknn4==UBtest$`Personal Loan`)

#With k=5
UBknn5 <- knn(UBtraining,UBtest,UBtraining$`Personal Loan`,k=5)
UBknn5
table(UBknn5,UBtest$`Personal Loan`)
mean(UBknn5==UBtest$`Personal Loan`)

#With k=6
UBknn6 <- knn(UBtraining,UBtest,UBtraining$`Personal Loan`,k=6)
UBknn6
table(UBknn6,UBtest$`Personal Loan`)
mean(UBknn6==UBtest$`Personal Loan`)

sum(UBknn2==UBtest$`Personal Loan`)

#BREAKING DATA INTO 3 SETS
set.seed(10)
UBtraining<- Unibank[1:2500,]
UBvalid<- Unibank[2501:4000,]
UBtest<- Unibank[4001:5000,]

#BASED ON TEST DATA
UBknnlu <- knn(UBtraining,UBtest,UBtraining$`Personal Loan`,k=1)
UBknnlu
table(UBknnlu,UBtest$`Personal Loan`)
mean(as.numeric(as.character(UBknnlu))==UBtest$`Personal Loan`)

#BASED ON VALIDATION DATA
UBknnfu <- knn(UBtraining,UBvalid,UBtraining$`Personal Loan`,k=1)
UBknnfu
table(UBknnfu,UBvalid$`Personal Loan`)
mean(as.numeric(as.character(UBknnfu))==UBvalid$`Personal Loan`)

#BASED ON TRAINING DATA
UBknncu <- knn(UBtraining,UBtraining,UBtraining$`Personal Loan`,k=1)
UBknncu
table(UBknncu,UBtraining$`Personal Loan`)
mean(as.numeric(as.character(UBknncu))==UBtraining$`Personal Loan`)



