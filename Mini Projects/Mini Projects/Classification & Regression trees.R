install.packages("rattle")
install.packages("rpart.plot")

getwd()
setwd("~/Desktop/IE 7275 Data mining/Homework 6")

require(rpart.plot)
require(stats)
require(psych)
require(readxl)
require(rpart)
require(tree)

#####Pre-Processing data######
flights<-read_excel("FlightDelays.xlsx",sheet=2)
flights.dummy <- model.matrix(~CARRIER+DEST+ORIGIN,data=flights)
new<- dummy.code(flights$DAY_WEEK)
colnames(new)<- c("DAY_WEEK1","DAY_WEEK2","DAY_WEEK3","DAY_WEEK4","DAY_WEEK5","DAY_WEEK6","DAY_WEEK7")
new_flights <- data.frame(new,flights)
flightdelay<- data.frame(flights.dummy,new_flights)
flightsdelay <- flightdelay[,-c(1,13,21,23,27,29)]
head(flightsdelay)

#####Creating dummy variables for departure times####
flightsdelay$DEP_HOUR <- trunc(flightsdelay$CRS_DEP_TIME/100)
x<-cut(flightsdelay$DEP_HOUR, c(6,8,10,12,14,16,18,20,22),include.lowest = TRUE,right=FALSE)
newdeptime<- dummy.code(x)
flightsfinal<- data.frame(newdeptime,flightsdelay)
head(newdeptime)
##final data set######
flightsfinale<- flightsfinal[,-c(1,26,27,29,33,35)]

####Partitioning data sets into training and test#####
set.seed(987654)
idx <- sample(2, nrow(flightsfinale),replace=TRUE, prob=c(0.7,0.3) )
train <- flightsfinale[idx==1,]
test <- flightsfinale[idx==2,]

####Fitting full tree model#####
model<- rpart(FlightStatus~., data=train, control=rpart.control(cp=0.001, maxdepth = 6))
model
plot(model, uniform=TRUE, main="Classification Tree for Flight Status")
text(model, use.n=TRUE, all=TRUE, cex=.8)
rpart.plot(model)
#post(model, file = "~/Desktop/IE 7275 Data mining/Homework 6/tree.ps", title = "Classification Tree for Kyphosis")

#####Predicting the full tree model on test data####
p<- predict(model,test,type="class")
####Creating confusion matrix####
table(test[,29],p)
####Calculating missclassification error####
mean(p!=test$FlightStatus)


####Removing Day_of_month predictor from set####
train1 <- flightsfinale[idx==1,-28]

####Fitting full tree model without Day_of_month predictor#####
model3<- rpart(FlightStatus~., data=train1,control=rpart.control(cp=0.0001))
model3
plot(model3, uniform=TRUE, main="Classification Tree for Flight Status")
text(model3, use.n=TRUE, all=TRUE, cex=.8,pretty=0)
rpart.plot(model3)
#post(model3, file = "~/Desktop/IE 7275 Data mining/Homework 6/tree.ps", title = "Classification Tree for Kyphosis")

#####Predicting the full tree model using train1 on test data###
p5<- predict(model3,test,type="class")
####Creating confusion matrix#####
table(test[,29],p5)
####Calculating missclassification error#####
mean(p5!=test$FlightStatus)

####Finding the best pruned tree using train1 dataset based on relative error####
printcp(model3)
plotcp(model3)
summary(model3)

#####Fitting the pruned model using train1 dataset#####
pfit2<- prune(model3,cp=0.022)
plot(pfit2, uniform=TRUE, main="Classification Tree for Flight Status")
text(pfit2, use.n=TRUE, all=TRUE, cex=.8)


#####Predicting the pruned model using train1 on test data###
p4<- predict(pfit2,test,type="class")
####Creating confusion matrix#####
table(test[,29],p4)
####Calculating missclassification error#####
mean(p4!=test$FlightStatus)
