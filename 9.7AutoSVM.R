


rm(list = ls())

library(ISLR)
library(e1071)

fix(Auto)
attach(Auto)
dim(Auto)
#[1] 392   9
names(Auto)
#[1] "mpg" "cylinders""displacement" "horsepower" "weight" "acceleration" "year"  "origin"      
#[9] "name" 

# a- creating binary variable
mpg01 = rep(1,392)
mpg01[mpg<median(mpg)] = 0

Auto_n = data.frame(Auto,mpg02=as.factor(mpg01))
Auto_n = Auto_n[,-1]


train = sample(392,300)

Auto_n.train = Auto_n[train,]
Auto_n.test = Auto_n[-train,]

#b - fit a support vector classifier

svmsvc = svm(mpg02~.,data = Auto_n.train,kernel = "linear",cost = 1,scale = TRUE)

table(pred = predict(svmsvc,newdata = Auto_n.train),actual = Auto_n.train$mpg02)
#     actual
#pred   0   1
#    0 141   6
#    1   3 150

table(pred = predict(svmsvc,newdata = Auto_n.test),actual = Auto_n.test$mpg02)
#    actual
#pred  0  1
#   0 49  1
#   1  3 39

svmsvc = svm(mpg02~.,data = Auto_n.train,kernel = "linear",cost = 0.1,scale = TRUE)
table(pred = predict(svmsvc,newdata = Auto_n.train),actual = Auto_n.train$mpg02)
table(pred = predict(svmsvc,newdata = Auto_n.test),actual = Auto_n.test$mpg02)




# Tuning various costs now

tune.out = tune(svm,mpg02~.,data = Auto_n.train,kernel = "linear",
                ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)),scale=TRUE)
summary(tune.out)

bestmod = tune.out$best.model
bestmod
table(pred = predict(bestmod,newdata = Auto_n.train),actual = Auto_n.train$mpg02)
#       actual
#pred   0   1
#    0 120   6
#    1  24 150
table(pred = predict(bestmod,newdata = Auto_n.test),actual = Auto_n.test$mpg02)
#     actual
#pred  0  1
#    0 47  0
#    1  5 40


#c - repeat b with polynomial & radial 

tune.out.poly = tune(svm,mpg02~.,data = Auto_n.train,kernel = "polynomial",
        ranges = list(degree=c(1,2,3,4,5),gamma=c(0.5,1,2,3,4),cost=c(0.1,1,10,100,1000)),scale=TRUE)
summary(tune.out.poly)
bestmod_poly = tune.out.poly$best.model
table(pred=predict(bestmod_poly,newdata = Auto_n.train),actual=Auto_n.train$mpg02)
#      actual
#pred   0   1
#    0 128   5
#    1  16 151
table(pred=predict(bestmod_poly,newdata = Auto_n.test),actual=Auto_n.test$mpg02)
#    actual
#pred  0  1
#    0 49  0
#    1  3 40


tune.out.rad = tune(svm,mpg02~.,data = Auto_n.train,kernel = "radial",
                     ranges = list(degree=c(1,2,3,4,5),gamma=c(0.5,1,2,3,4),cost=c(0.1,1,10,100,1000)),scale=TRUE)
summary(tune.out.rad)
bestmod_rad = tune.out.rad$best.model
table(pred=predict(bestmod_rad,newdata = Auto_n.train),actual=Auto_n.train$mpg02)
#      actual
#pred   0   1
#    0 140   2
#    1  4 154
table(pred=predict(bestmod_rad,newdata = Auto_n.test),actual=Auto_n.test$mpg02)
#    actual
#pred  0  1
#    0 49  0
#    1  3 40




















































