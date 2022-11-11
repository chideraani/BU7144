#loading all required libraries
library(dplyr)
library(lubridate)
library(ggfortify)
library(forecast)
library(ggplot2)
library(tidyr)
library(insight)
library(easystats)
library(magrittr)
library(lme4)
library(Matrix)
library(relaimpo)


#reading in the data
data <- read.csv('days.csv')

#data exploration
View(data)
summary(data)


#changing dteday to a date time object
data$datetime <- parse_date_time(paste(data$dteday), orders = 'ymd')

#selecting desired columns
df <- data[, c('datetime','season','holiday','yr','mnth','weathersit','temp','cnt')]


#handling categorical variables
df$season <- as.factor(df$season)
df$holiday <- as.factor(df$holiday)
df$weathersit <- as.factor(df$weathersit)
df$mnth <- as.factor(df$mnth)
df$yr <- as.factor(df$yr)


#changing variable 'season' to 'seasons'
colnames(df) <- c('datetime','seasons','holiday','yr','mnth','weathersit','temp','cnt')

#creating time-series object
bikeday.ts<- ts(df$cnt, start = c(2011,1),frequency = 365)

#plotting time-series object
plot(bikeday.ts, xlab='Year', ylab='Total No Of Bikes Rented')

#decomposing the time-series object & plotting it
decomposed <- decompose(bikeday.ts)
plot(decomposed)


#splitting the data (65% training & 35% validation)
nTrain <- 517
nValid <- length(bikeday.ts) - nTrain

train.ts <- window(bikeday.ts, start = c(2011,121), end = c(2011,nTrain))

valid.ts <- window(bikeday.ts, start = c(2011,nTrain+1), end = c(2011, nTrain+nValid))

#plotting the train and validation sets
plot(train.ts, xlab='Train Years', ylab='Total No Of Bikes Rented')
plot(valid.ts, xlab='Test Years', ylab='Total No Of Bikes Rented')


#Model Building

#seasonal naive model
snaive.pred <- snaive(train.ts, h = nValid)
autoplot(snaive.pred)

#seasonal naive accuracy
snaive_accuracy <- accuracy(snaive.pred, valid.ts)


#time-series linear regression model with trend and seasonality
lm1 <- tslm(train.ts ~  trend+season)
lm1.pred <- forecast(lm1, h=214)

lm1.accuracy <- accuracy(lm1.pred,valid.ts)


# holt-winter model
hw<- ets(train.ts, model = "ZZZ")
hw.pred <- forecast(hw, h=214)

# holt-winter accuracy
hw.accuracy <- accuracy(hw.pred, valid.ts)


#arima model
arima<- auto.arima(train.ts)
arima.pred <- forecast(arima, h=214)

#arima accuracy
arima.accuracy <- accuracy(arima.pred, valid.ts)


#re-partitioning the data for the multivariate lm model
df2 <- df[121:731,]

#creating ID column
df2$id <- 1:nrow(df2)
rownames(df2) <- df2$datetime
df2 <- df2[,2:9]

#splitting the data
train <- df2[1:397,]
test  <- df2[398:611,]

#multivariate linear regression model without trend
lm2 <- lm(cnt ~ seasons + holiday  + yr + mnth + weathersit + temp , data = train)
lm2.pred <- forecast(lm2, newdata=test)

lm2.accuracy <- accuracy(lm2.pred,test)


#multivariate linear regression model with trend
lm3 <- lm(cnt ~ seasons + holiday + yr + mnth + weathersit + temp  + id, data = train)
lm3.pred <- forecast(lm3, newdata=test)

lm3.accuracy <- accuracy(lm3.pred,test)


# plotting seasonal naive model
par(mfrow = c(1, 1))
plot(train.ts,  ylab = "Total No Of Bikes Rented", xlab = "Year",ylim=c(0,10000), 
     xaxt='n',xlim=c(2011.5,2013), bty = "l",main = "Seasonal Naive Model")
axis(1, at = c(2011.5,2012.0,2012.5,2013), labels = c('2011-05-01','2012-01-01','2012-06-31','2013-01-01'))
lines(snaive.pred$mean, lwd = 2, col = "blue", lty = 1)
lines(valid.ts, col = "grey20", lty = 3)
lines(c(2013 - 0.58, 2013 - 0.58), c(0, 12000))
lines(c(2013, 2013), c(0, 12000))
text(2011.95, 9500, "Training")
text(2012.7, 9500, "Validation")
arrows(2013 - 0.65, 9000, 2011.5, 9000, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2013 - 0.55, 9000, 2012.95, 9000, code = 3, length = 0.1, lwd = 1,angle = 30)


#plotting forecast and residuals -- multivariate linear model with trend
par(mfrow = c(2, 1))
plot(x =  1:length(train$cnt), 
     y= train$cnt,
     xlim = c(0,611), ylim=c(0,10500),
     ylab = "Bike Count", 
     xlab = "Year",
     bty = "l",  
     main = "Multi-variate Linear Regression Model", 
     type='l', 
     xaxt = 'n')
axis(1, at = c(0,300,600), labels = c('2011-05-01', '2012-02-24', '2012-12-20'))
lines(x =  1:length(train$cnt), y=lm3.pred$fitted, lwd = 1, col = "blue")

lines(x =  length(train$cnt)+1:length(test$cnt),y=test$cnt)
lines(x =  length(train$cnt)+1:length(test$cnt),y=lm3.pred$mean, col='red')
lines(c(600 - 200, 600 - 200), c(0, 10500))
lines(c(611, 611), c(0, 10500))
text(200, 10000, "Training")
text(500, 10000, "Validation")
arrows(611 - 207, 9500, 605, 9500, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(0, 9500, 611-220, 9500, code = 3, length = 0.1, lwd = 1,angle = 30)


plot(x =  1:length(train$cnt), y=lm3.pred$residuals,  ylab = "Forecast Errors",xlab = "Year",
     bty = "l", main = "", type='l',  xaxt = 'n',xlim = c(0,611),ylim=c(-3000,4000))
axis(1, at = c(0,300,600), labels = c('2011-05-01', '2012-02-24', '2012-12-20'))
lines(x = length(train$cnt)+1:length(test$cnt), y=test$cnt - lm3.pred$mean, lwd = 1)
lines(c(600 - 200, 600 - 200), c(-3000, 4000))
lines(c(611, 611), c(-3000, 4000))
text(200, 3800, "Training")
text(500, 3800, "Validation")
arrows(611 - 207, 3500, 605, 3500, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(0, 3500, 611-220, 3500, code = 3, length = 0.1, lwd = 1,angle = 30)


#relative importance
lm3_shapley <- calc.relimp(lm3,type = "lmg")
lm3_shapley

#plotting the relative importance
barplot(sort(lm3_shapley$lmg,decreasing = TRUE),col='blue',
        main = "Relative Importance of Predictors",xlab = "Predictor Labels",ylab = "Value Regression")

