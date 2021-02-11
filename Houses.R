#importing dataset
houses <- read.csv("C:/Users/heena/OneDrive/Documents/houses.csv")
#Exploring the dataset (EDA)
str(houses)
#looking at statistics of data
summary(houses)
#First 6 rows of dataset
head(houses)
tail(houses)
#dimensions of dataset
dim(houses)
#standard deviation of columns
stddev <- sapply(houses, sd)
format(stddev, scientific=FALSE)
stddev
mean(houses$price)
max(houses$price)
range(houses$age)

#Data preprocessing
library(dplyr)
#deleting first two columns 
houses <- houses %>% select(c(-1,-2))
head(houses)
#checking for missing values
is.na(houses)
#changing the columns to factors(categorical)
houses$air_cond <- factor(houses$air_cond,labels=c("No","Yes"))
houses$waterfront <- factor(houses$waterfront,labels=c("No","Yes"))
houses$construction <- factor(houses$construction,labels=c("No","Yes"))
head(houses)
houses$fuel <- factor(houses$fuel,labels=c("Gas","Electric","Oil"))
houses$sewer <- factor(houses$sewer,labels=c("None","Private","Public"))
houses$heat <- factor(houses$heat,labels=c("Hot air","Hot water","Electric"))
head(houses)

#Data visualization
library(ggplot2)
#plotting histogram of price on x-axis
ggplot(data=houses,aes(x=price))+geom_histogram(bins=40)
#From this we can infer that max price of house is 7.5lakhs and average price is 2lakhs
ggplot(data=houses,aes(x=price))+geom_histogram(bins=40, fill="light pink",col="red")
#Seeing relation between price of the house with respect to different variables
ggplot(data=houses,aes(y=price,x=waterfront,fill=waterfront))+geom_boxplot()
#We infer that if house has a waterfront, it will have higher price
ggplot(data=houses,aes(y=price,x=air_cond,fill=air_cond))+geom_boxplot()
ggplot(data=houses,aes(y=price,x=living_area,fill=living_area))+geom_point()+geom_smooth(method="lm",se=F)
ggplot(data=houses,aes(y=price,x=age,fill=age))+geom_point()+geom_smooth(method="lm",se=F)


ggplot(data=houses,aes(y=price,x=living_area,col=factor(rooms)))+geom_point()+geom_smooth(method="lm",se=F)+labs(col="rooms")
ggplot(data=houses,aes(y=price,x=age,col=factor(rooms)))+geom_point()+geom_smooth(method="lm",se=F)+labs(col="rooms")

#Splitting data into train and test sets
library(caTools)
split_index <- sample.split(houses$price,SplitRatio = 0.80)
train <- subset(houses,split_index==T)
test <- subset(houses,split_index==F)
nrow(train)
nrow(test)

#Building the Linear Regression model
model <- lm(price~.,data=train)
#making predictions for test data
result <- predict(model,test)
compare <- cbind(actual=test$price,predicted=result)
print(compare)
compare <- as.data.frame(compare)
error <- compare$actual-compare$predicted
compare <- cbind(compare,error)
rmse <- sqrt(mean(compare$error^2))
rmse
summary(model)

#Creating another model without unnecessary variables
model2 <- lm(price~.-fireplaces-sewer-fuel,data=train)
result2 <- predict(model2,test)
result2
compare2 <- cbind(actual=test$price,predicted=result2)
compare2 <- as.data.frame(compare2)
error2 <- compare2$actual-compare2$predicted
compare2 <- cbind(compare2,error2)
rmse2 <- sqrt(mean(compare2$error2^2))
rmse2
summary(model2)

#Logistic Regression model
logr <- glm(price~.-fireplaces-sewer-fuel,data=train)
summary(logr)
resultlog <- predict(logr,test)
compare3 <- cbind(actual=test$price,predicted=resultlog)
print(compare3)
compare3 <- as.data.frame(compare3)
error3 <- compare3$actual-compare3$predicted
compare3 <- cbind(compare3,error3)
rmse3 <- sqrt(mean(compare3$error3^2))
rmse3

#Random forest model
library(party)
library(randomForest)
forest <- randomForest(price~.-fireplaces-sewer-fuel,data=train)
forestresult <- predict(forest,test)
compare4 <- cbind(actual=test$price,predicted=forestresult)
print(compare4)
compare4 <- as.data.frame(compare4)
error4 <- compare4$actual-compare4$predicted
compare4 <- cbind(compare4,error4)
rmse4 <- sqrt(mean(compare4$error4^2))
rmse4
print(forest)


##We can conclude that RandomForest model gives highest accuracy

