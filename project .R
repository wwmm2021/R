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

#cross validation
set.seed(321)
training.samples <- createDataPartition(HousePrices$SoldPrice, p = 0.8, list = FALSE)
train.data  <- HousePrices[training.samples, ]
test.data <- HousePrices[-training.samples, ]

#transformation
summary(fit1<-powerTransform(cbind(Day,Beds,Baths,Size)~1, data = train.data))
testTransform(fit1, c(1,0,0,-0.5))
fit2 <- lm(SoldPrice ~ factor(ZipCode)+Day+log(Beds)+log(Baths)+I(1/sqrt(Size)), data = train.data)
invResPlot(fit2)
boxCox(fit2)
fit3 <- lm(1/sqrt(SoldPrice) ~ factor(ZipCode)+Day+log(Beds)+log(Baths)+I(1/sqrt(Size)), data = train.data)
invResPlot(fit3)
summary(fit3)
ncvTest(fit3)
mmps(fit3)
residualPlots(fit3)
plot(fit3)

fit4 <- lm(1/sqrt(SoldPrice) ~ factor(ZipCode)+Day+log(Beds)+log(Baths)+I(1/sqrt(Size))+I(1/Size), data = train.data)
mmps(fit4)
ncvTest(fit4)
residualPlots(fit4)
plot(fit4)
summary(fit4)

fit5 <- lm(1/sqrt(SoldPrice) ~ I(ZipCode==32308)+I(ZipCode==32309)+I(ZipCode==32311)+I(ZipCode==32312)+I(ZipCode==32317)+log(Baths)+I(1/sqrt(Size))+I(1/Size), data = train.data)

#remove outliers
gg_cooksd(fit5, label = TRUE, show.threshold = TRUE,threshold = "convention", scale.factor = 0.5)
train.data.update <- train.data[c(-27,-34,-40,-83,-86,-114,-140,-152,-182,-233,-311,-318,-326,-360,-363,-377,-384,-438,-450), ]
nrow(train.data.update)
fit6 <- lm(1/sqrt(SoldPrice) ~ I(ZipCode==32308)+I(ZipCode==32309)+I(ZipCode==32311)+I(ZipCode==32312)+I(ZipCode==32317)+log(Baths)+I(1/sqrt(Size))+I(1/Size), data = train.data.update)

# Stepwise Regression 
step(fit6,direction="forward",k=2,lower=~1,data = train.data.update)
step(fit6,direction="backward",k=2,lower=~1,data = train.data.update)
step(fit6,direction="both",k=2,lower=~1,data = train.data.update)
step(fit6,direction="forward",k=log(nobs(fit6)),lower=~1,data = train.data.update)
step(fit6,direction="backward",k=log(nobs(fit6)),lower=~1,data = train.data.update)
step(fit6,direction="both",k=log(nobs(fit6)),lower=~1,data = train.data.update)

predictions1 <- predict(fit6, test.data)
data.frame(R2 = R2(predictions1, 1/sqrt(test.data$SoldPrice)),RMSE = RMSE(predictions1, 1/sqrt(test.data$SoldPrice)), MAE = MAE(predictions1, 1/sqrt(test.data$SoldPrice)))
RMSE(predictions1, 1/sqrt(test.data$SoldPrice))/mean(1/sqrt(test.data$SoldPrice))

fit7 <- lm(1/sqrt(SoldPrice) ~ I(ZipCode==32308)+I(ZipCode==32309)+I(ZipCode==32311)+I(ZipCode==32312)+I(ZipCode==32317)+log(Baths)+I(1/sqrt(Size)), data = train.data.update)
predictions2 <- predict(fit7, test.data)
data.frame(R2 = R2(predictions2, 1/sqrt(test.data$SoldPrice)),RMSE = RMSE(predictions2, 1/sqrt(test.data$SoldPrice)), MAE = MAE(predictions2, 1/sqrt(test.data$SoldPrice)))
RMSE(predictions2, 1/sqrt(test.data$SoldPrice))/mean(1/sqrt(test.data$SoldPrice))

##model diagnostics
outlierTest(fit6)
influenceIndexPlot(fit6)
mmps(fit6)
ncvTest(fit6)
residualPlots(fit6)
plot(fit6)
summary(fit6)

