getwd()
train <- read.csv("MarchAprilTest.csv", head=TRUE, sep="," )
str(train)
dim(train)
summary(train)

library(Hmisc)
library(caTools)   #split dataset into two sets in ratio of vector specified
split <- sample.split(train$type, SplitRatio = 0.75)
split

#get training and test data
datatrain <- subset(train, split == TRUE)
datatest <- subset(train, split == FALSE)
# install.packages("foreign")
library(foreign)


apply(train,2,max,na.rm=TRUE)

type <- as.logical(train$logic)


#train$logic1<- relevel(train$Product_title, ref = "New Droom Branded Helmet - Ascone")


modeltrain <- glm(type~factor(vehicle_condition_type)+factor(operatingSystem)+transactionsPerSession+uniquePurchases+hour+factor(dayOfWeekName)+daysSinceLastSession+factor(deviceCategory)+daysSinceLastSession+factor(userType)+factor(platform)+sessionsToTransaction+daysToTransaction,data=datatrain,family="binomial") #march test
summary(modeltrain)

# modellogic <- glm(type~commitment_fee+factor(platform)+factor(MakeModelTrimYear),data=datatrain,family="binomial") #RUNNING march test
# pchisq(8761.9-2465.7,20)

#sink("summaryTest2GA.txt")
#summary(modellogic)
#sink()

#PREDICTION

predictModel <- predict(modeltrain, datatest)
predictModel <- ifelse(predictModel<0.5,0,1)
predictModel

table(ActualValue=datatest$type, PredictedValue= predictModel>0.5)


actuals_preds <- data.frame(cbind(actuals=datatest$type, predictModel))  # make actual_predict dataframe.
actuals_preds
correlation_accuracy <- cor(actuals_preds,use = "complete.obs") #use argument to remove null values
correlation_accuracy
#install.packages("corrplot")
library(corrplot)
corrplot(correlation_accuracy, type="lower")

#TO ATTACH PREDICTED MODEL TO DATATEST
#output <- cbind(datatest, predictModel)



