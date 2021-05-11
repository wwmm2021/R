set.seed(12345)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 0.2*x1 + rnorm(n)

m0 <- lm(y~x1+x2)
summary(m0)

########## test beta1 ##################
T.stat <- summary(m0)[["coefficients"]][2, "t value"]
2*pt(abs(T.stat),df=97,lower.tail = FALSE)
(pt(-abs(T.stat),df=97,lower.tail = TRUE) 
+ pt(abs(T.stat),df=97,lower.tail = FALSE))


m.null <- lm(y~x2)  # under the null hypothesis
yhat <- m.null$fitted.values
ehat <- m.null$residuals

B<-999
T.stat.boot<-rep(0,B)
for(i in 1:999){
  id<-sample(1:n,n,replace=T)
  yboot <- yhat + ehat[id]
  m <- lm(yboot~x1+x2)
  T.stat.boot[i]<-summary(m)[["coefficients"]][2, "t value"]
}


(1+sum(T.stat.boot>abs(T.stat))+sum(T.stat.boot< -abs(T.stat)))/(B+1)
2*(1+sum(T.stat.boot>abs(T.stat)))/(B+1)


########## test beta2 ##################

T.stat <- summary(m0)[["coefficients"]][3, "t value"]

m.null <- lm(y~x1)  # under the null hypothesis
yhat <- m.null$fitted.values
ehat <- m.null$residuals

B<-999
T.stat.boot<-rep(0,B)
for(i in 1:999){
  id<-sample(1:n,n,replace=T)
  yboot <- yhat + ehat[id]
  m <- lm(yboot~x1+x2)
  T.stat.boot[i]<-summary(m)[["coefficients"]][3, "t value"]
}


(1+sum(T.stat.boot>abs(T.stat))+sum(T.stat.boot< -abs(T.stat)))/(B+1)
2*(1+sum(T.stat.boot>abs(T.stat)))/(B+1)




########## test beta1 using (beta1)^2 ##################
my.stat <- (coef(m0)[2])^2

m.null <- lm(y~x2)  # under the null hypothesis
yhat <- m.null$fitted.values
ehat <- m.null$residuals

B<-999
my.stat.boot<-rep(0,B)
for(i in 1:999){
  id<-sample(1:n,n,replace=T)
  yboot <- yhat + ehat[id]
  m <- lm(yboot~x1+x2)
  my.stat.boot[i]<- (coef(m)[2])^2
}


(1+sum(my.stat.boot>my.stat))/(B+1)


########## test beta2 using (beta2)^2 ##################
my.stat <- (coef(m0)[3])^2

m.null <- lm(y~x2)  # under the null hypothesis
yhat <- m.null$fitted.values
ehat <- m.null$residuals

B<-999
my.stat.boot<-rep(0,B)
for(i in 1:999){
  id<-sample(1:n,n,replace=T)
  yboot <- yhat + ehat[id]
  m <- lm(yboot~x1+x2)
  my.stat.boot[i]<- (coef(m)[3])^2
}


(1+sum(my.stat.boot>my.stat))/(B+1)
