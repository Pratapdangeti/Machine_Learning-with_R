
rm(list = ls())

setwd("/home/pratapdangeti/Documents/softwares/RCodes/ISLR")


library(MASS)
library(ISLR)
fix(Auto)
attach(Auto)

names(Auto)
plot(Auto)
cor(Auto[,-9])
auton = Auto[,-9]
lm.fit=lm(mpg~.,data=auton)
summary(lm.fit)
plot(predict(lm.fit),residuals(lm.fit))
plot(lm.fit)

# Interaction terms
lm.fit=lm(mpg~.+displacement*cylinders,data=auton)
summary(lm.fit)

lm.fit2=lm(mpg~.+displacement:cylinders,data=auton)
summary(lm.fit2)

plot(auton)

plot(auton$horsepower,auton$mpg)

auton$horsepower <- log(auton$horsepower)

plot(auton$horsepower,auton$mpg)








