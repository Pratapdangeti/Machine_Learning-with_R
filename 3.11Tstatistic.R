

rm(list=ls())

set.seed(1)
x=rnorm(100)

y=2*x+rnorm(100)

lm.fit<-lm(y~x+0)
summary(lm.fit)

lm.fit2<-lm(y~x)
summary(lm.fit2)
