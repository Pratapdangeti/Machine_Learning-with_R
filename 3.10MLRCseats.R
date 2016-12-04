

rm(list=ls())
library(MASS)
library(ISLR)

fix(Carseats)

attach(Carseats)

str(Carseats)
names(Carseats)

lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)


lm.fit2 = lm(Sales~Price+US)
summary(lm.fit2)

# Predicting confidence interval of co-efficients
confint(lm.fit2,'Price',level=0.95)
confint(lm.fit2,'(Intercept)',level=0.95)
confint(lm.fit2,'USYes',level=0.95)


# Plotting graphs-including residuals
plot(lm.fit)



