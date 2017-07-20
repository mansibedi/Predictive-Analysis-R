getwd()
train <- read.csv("MarchAprilTest.csv")
dim(train)
# install.packages("Hmisc")
library(Hmisc)

library(caTools)   #split dataset into two sets in ratio of vector specified
split <- sample.split(train$type, SplitRatio = 0.75)
split

#get training and test data
datatrain <- subset(train, split == TRUE)
datatest <- subset(train, split == FALSE)



#CONVERTING FACTOR TO NUMERIC VARIABLES
library(plyr)
datatrain$platform<- revalue(datatrain$platform, c("Android"="0", "App"="1", "Desktop"="2", "IoS"="3", "Mobile Web"="4"))   

datatrain$platform <- as.numeric(as.character(datatrain$platform))
typeof(datatrain$platform)



#data visualisation
plot(train$order_status, train$platform, xlab="status", ylab="platform")

#install.packages("car")
library(car)
#modeltrain <-lm(order_status~platform+net_gmv, data = datatrain)
modeltrain <-lm(type~GA_REVENUE+platform+hour+daysSinceLastSession+sessionsToTransaction+daysToTransaction, data = datatrain)
vif(modeltrain)
summary(modeltrain)
AIC(modeltrain) #lower the better
BIC(modeltrain) #lower the better

#prediction

predictModel <- predict(modeltrain, datatest)
predictModel <- ifelse(predictModel<0.5,0,1)
predictModel

actuals_preds <- data.frame(cbind(actuals=datatest$type, predictModel))  # make actual_predict dataframe.
actuals_preds
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
#install.packages("corrplot")
library(corrplot)
corrplot(correlation_accuracy, type="lower")

#PLOTTING AND COMPARING ACTUAL AND PREDICTED DATA
plot(datatest$type, type = "l", lty= 1.8, col="green")  #plotting actual data
lines(predictModel, type= "l", col= "blue")  #plotting predicted data

Residual <- resid(modeltrain)
Residual
