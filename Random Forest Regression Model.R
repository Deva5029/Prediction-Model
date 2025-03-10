 

#installing packages
install.packages("randomForest")
install.packages("caret")

#loading packages
library(randomForest)
library(caret)

#my data set
mydata <- read.csv("C:/Users/Student/Documents/finaldata.csv")
#mydata <- head(mydata,10000) #CODE TO CONSIDER PARTIAL DATA

#splitting of data
set.seed(1)

#Use 80% of data set as training set and remaining 20% as testing set
sample <- sample(c(TRUE, FALSE), nrow(mydata), replace=TRUE, prob=c(0.8,0.2))
train  <- mydata[sample, ]
test   <- mydata[!sample, ]

#view train set
dim(train)
str(train)

#view test set
dim(test)
str(test)

#MODEL -- RANDOM FOREST REGRESSION
start <- proc.time()[3]

model <- randomForest(price~., data = train,ntree=1000)

end <- proc.time()[3]
print(paste("This took ", round(end-start,digits = 1), " seconds", sep = ""))

model #to get model information

#prediction
pred_rf <- predict(model , test)
print(pred_rf)

#error in data
error_rf <- test$price - pred_rf
print(error_rf)

#root mean square
RMSE <- sqrt(sum((test$price-pred_rf)^2/length(pred_rf)))
print(RMSE)

#PERCENTAGE ERROR
p_error <- (RMSE/mean(test$price))
print(p_error)
print(p_error*100)

#ACCURACY -- FOR ACCURACY WE ARE CALCULATING R2_SCORE
Accuracy <- R2(test$price,pred_rf)
print(Accuracy*100)

#RMS VALUES OF TRAIN AND TEST DATA
RMSE_train <- sqrt(mean(train$price^2))
print(RMSE_train)

RMSE_test <- sqrt(mean(test$price^2))
print(RMSE_test)

#VISUALIZATION OF MODEL

#PLOT ERROR VS TREES
plot(model, col=c("orange"))

#FEATURE IMPORTANCE PLOT
varImpPlot(model , main ='Feature Importance',col="BROWN")

#SCATTER PLOT
plot(test$price,pred_rf, main="Scatterplot", col = c("red","blue"), 
     xlab = "Actual Price", 
     ylab = "Predicted Price")

