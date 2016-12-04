

rm(list = ls())

setwd("/home/pratap/Documents/RCodes/ISLR")


library(MASS)
library(ISLR)

fix(Auto)
names(Auto)
str(Auto)
lm.fit = lm(mpg~horsepower,data=Auto)
summary(lm.fit)

predict(lm.fit,data.frame(horsepower=c(98)),interval="confidence")
predict(lm.fit,data.frame(horsepower=c(98)),interval="prediction")

attach(Auto)
plot(horsepower,mpg)
abline(lm.fit,lw=3,col="red")

plot(predict(lm.fit),residuals(lm.fit))

plot(lm.fit)





