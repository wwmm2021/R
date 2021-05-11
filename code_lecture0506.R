library(alr4)
data(physics)
attach(physics)
#plot(x,y)
?physics
m<-lm(y~x,weights=1/(SD^2))
plot(x,y)
lines(lowess(y~x),lty="dashed")
abline(m)
n = length(y)
1-pchisq(sum(m$res^2/SD^2),n-2)
anova(m)


m1<-lm(y~x+I(x^2),weights=1/(SD^2))
n<-length(y)
1-pchisq(sum(m1$res^2/SD^2),n-3)
anova(m1)
anova(m,m1)
plot(x,y)
lines(x,m1$fit)

m11 <- lm(y~I(x^2)+x,weights=1/(SD^2))
summary(m11)
anova(m11)


m2<-lm(y~I(x^2),weights=1/(SD^2))
n<-length(y)
1-pchisq(sum(m2$res^2/SD^2),n-3)
anova(m2)
anova(m1,m2)
plot(x,y)
lines(x,m2$fit)

# delta method
m1<-lm(y~x+I(x^2),weights=1/(SD^2))
param.names <- c("b0","b1","b2")
x.min <- "-b1/(2*b2)"
deltaMethod(m1, x.min,
            parameterNames=param.names)
x2 <- x^2
m1<-lm(y~x+x2,weights=1/(SD^2))
x.min <- "-x/(2*x2)"
deltaMethod(m1, x.min)

detach(physics)

library(lattice)
data(cakes)
attach(cakes)
m1 <- lm(Y ~ X1 + X2 + I(X1^2) + I(X2^2) + X1:X2, data=cakes)

par(mfrow=c(1, 2), mar=c(4, 3, 0, .5) + .1, mgp=c(2, 1, 0))

X1new <- seq(32, 38, len=50)
with(cakes,
   plot(X1, Y, type="n", xlab=expression(paste("(a)  ", X[1]))) )
   lines(X1new, predict(m1, newdata=data.frame(X1=X1new, X2=rep(340, 50))))
lines(X1new, predict(m1, newdata=data.frame(X1=X1new, X2=rep(350, 50))))
lines(X1new, predict(m1, newdata=data.frame(X1=X1new, X2=rep(360, 50))))
text(34, 4.7, "X2=340", adj=0,cex=0.7)
text(32.0, 5.7, "X2=350", adj=0,cex=0.7)
text(32.0, 7.6, "X2=360", adj=0,cex=0.7)


with(cakes,
   plot(X2, Y, type="n", xlab=expression(paste("(b)  ", X[2]))))
X2new <- seq(330, 370, len=50)
lines(X2new, predict(m1, newdata=data.frame(X1=rep(33, 50), X2=X2new)))
lines(X2new, predict(m1, newdata=data.frame(X1=rep(35, 50), X2=X2new)))
lines(X2new, predict(m1, newdata=data.frame(X1=rep(37, 50), X2=X2new)))
text(342, 4,   "X1=33", adj=0, cex=0.7)
text(335, 4.55, "X1=35", adj=0, cex=0.7)
text(336, 7.3, "X1=37", adj=0, cex=0.7)

m3 <- update(m1,   ~ .-X1:X2)
with(cakes,
   plot(X1, Y, type="n", xlab=expression(paste("(a)  ", X[1]))))
X1new <- seq(32, 38, len=50)
lines(X1new, predict(m3, newdata=data.frame(X1=X1new, X2=rep(340, 50))))
lines(X1new, predict(m3, newdata=data.frame(X1=X1new, X2=rep(350, 50))))
lines(X1new, predict(m3, newdata=data.frame(X1=X1new, X2=rep(360, 50))))
text(33, 4.3, "X2=340", adj=0, cex=0.7)
text(32.0, 7.2, "X2=350", adj=0, cex=0.7)
text(34.0, 7.1, "X2=360", adj=0, cex=0.7)
# (b)
with(cakes,
   plot(X2, Y, type="n", xlab=expression(paste("(b)  ", X[2]))))
X2new <- seq(330, 370, len=50)
lines(X2new, predict(m3, newdata=data.frame(X1=rep(33, 50), X2=X2new)))
lines(X2new, predict(m3, newdata=data.frame(X1=rep(35, 50), X2=X2new)))
lines(X2new, predict(m3, newdata=data.frame(X1=rep(37, 50), X2=X2new)))
text(340, 4.3,   "X1=33", adj=0, cex=0.7)
text(336, 7.0, "X1=35", adj=0, cex=0.7)
text(346, 7.3, "X1=37", adj=0, cex=0.7)

detach(cakes)


#sleep1
data(sleep1)
attach(sleep1)
sleep1$D <- factor(sleep1$D)
a1 <- lm(TS ~ D, sleep1, na.action=na.omit)
a0 <- update(a1,  ~ .-1)
compareCoefs(a1, a0)                    
plot(sleep1$D, sleep1$TS, xlab="Danger index" , ylab="Total sleep1 (h)")
anova(a0)
anova(a1)

m1 <- lm(TS ~ logb(BodyWt, 2)*D, sleep1, na.action=na.omit)
m2 <- update(m1,   ~ D  +  logb(BodyWt, 2))
m4 <- update(m1,   ~ logb(BodyWt, 2))
m3 <- update(m1,   ~ logb(BodyWt, 2):D)
compareCoefs(m1, m2, m4, m3, se=FALSE)
# all have the same Residual sum of squares:
a1<-anova(m4, m1)
a2<-anova(m3, m1)
a3<-anova(m2, m1)
a1[2, ]; a2[2, ]; a3[2, ]

n1 <- lm(TS ~ -1 + D + D:logb(BodyWt, 2), sleep1, na.action=na.omit)
n2 <- update(m1,   ~ -1 + D  +  logb(BodyWt, 2))
n4 <- update(m1,   ~ logb(BodyWt, 2))
n3 <- update(m1,   ~ logb(BodyWt, 2):D)

detach(sleep1)

