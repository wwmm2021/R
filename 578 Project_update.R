install.packages('ggplot2')
install.packages('car')
install.packages('tidyverse')
install.packages('caret')
install.packages('klaR')
install.packages('lindia')
library(lindia)
library(ggplot2)
library(car)
library(tidyverse)
library(caret)
library(klaR)

HousePrices <- read.csv(file = 'E:/My Documents/Courses/FSU/5167/Project/data5167.csv')
scatterplotMatrix(~ZipCode+Day+Beds+Baths+Size+SoldPrice,HousePrices,smooth=FALSE)
HousePrices <- HousePrices[-491,]

#Cross Validation
set.seed(123)
training.samples <- createDataPartition(HousePrices$SoldPrice, p = 0.8, list = FALSE)
train.data  <- HousePrices[training.samples, ]
test.data <- HousePrices[-training.samples, ]

#Transformation
summary(fit1<-powerTransform(cbind(Day,Size)~1, data = train.data))
testTransform(fit1, c(1,-0.5))

fit2 <- lm(SoldPrice ~ factor(ZipCode)+factor(Beds)+factor(Baths)+Day+I(1/sqrt(Size)), data = train.data)
invResPlot(fit2)
boxCox(fit2)

fit3 <- lm(1/sqrt(SoldPrice) ~ factor(ZipCode)+factor(Beds)+factor(Baths)+Day+I(1/sqrt(Size)), data = train.data)
boxCox(fit3)
summary(fit3)
ncvTest(fit3)
mmps(fit3)
residualPlots(fit3)
plot(fit3)

#Quadratic Term
fit4 <- lm(1/sqrt(SoldPrice) ~ factor(ZipCode)+factor(Beds)+factor(Baths)+Day+I(1/sqrt(Size))+I(1/Size), data = train.data)
mmps(fit4)
ncvTest(fit4)
residualPlots(fit4)
plot(fit4)
summary(fit4)

#Remove Outliers
outlierTest(fit4)
train.data.update <- train.data[c(-23,-442,-196,-340),]
nrow(train.data.update)
fit5 <- lm(1/sqrt(SoldPrice) ~ factor(ZipCode)+factor(Beds)+factor(Baths)+Day+I(1/sqrt(Size))+I(1/Size), data = train.data.update)
summary(fit5)
mmps(fit5)
ncvTest(fit5)
residualPlots(fit5)
plot(fit5)

#Stepwise Regression 
step(fit5,direction="forward",k=2,lower=~1,data = train.data.update)
step(fit5,direction="backward",k=2,lower=~1,data = train.data.update)
step(fit5,direction="both",k=2,lower=~1,data = train.data.update)
step(fit5,direction="forward",k=log(nobs(fit5)),lower=~1,data = train.data.update)
step(fit5,direction="backward",k=log(nobs(fit5)),lower=~1,data = train.data.update)
step(fit5,direction="both",k=log(nobs(fit5)),lower=~1,data = train.data.update)

fit7 <- lm(1/sqrt(SoldPrice) ~ factor(ZipCode)+I(1/sqrt(Size))+I(1/Size), data = train.data.update)
predictions <- predict(fit7, test.data)
data.frame(R2 = R2(predictions, 1/sqrt(test.data$SoldPrice)),RMSE = RMSE(predictions, 1/sqrt(test.data$SoldPrice)), MAE = MAE(predictions, 1/sqrt(test.data$SoldPrice)))
RMSE(predictions, 1/sqrt(test.data$SoldPrice))/mean(1/sqrt(test.data$SoldPrice))

#Model Diagnostics
outlierTest(fit7)
influenceIndexPlot(fit7)
mmps(fit7)
ncvTest(fit7)
residualPlots(fit7)
plot(fit7)
summary(fit7)

#gg_cooksd(fit5, label = TRUE, show.threshold = TRUE,threshold = "convention", scale.factor = 0.5)