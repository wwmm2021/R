

training.data.raw <- read.csv('D:/R/Titanic dataset/train.csv',header=T,na.strings=c(""))
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))
missmap(training.data.raw, main = "Missing values vs observed")
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)
is.factor(data$Sex)
missmap(training.data.raw, main = "Missing values vs observed",col = red)
library(dplyr)

contrasts(data$Sex)

contrasts(data$Embarked)

data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

train <- data[1:800,]
test <- data[801:889,]

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

anova(model, test="Chisq")