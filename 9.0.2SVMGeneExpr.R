

rm(list = ls())

library(ISLR)
library(e1071)
names(Khan)
#[1] "xtrain" "xtest"  "ytrain" "ytest" 

dim(Khan$xtrain)
#[1]   63 2308

dim(Khan$xtest)
#[1]   20 2308

length(Khan$ytrain)
#[1] 63

length(Khan$ytest)
#[1] 20


table(Khan$ytrain)
#1  2  3  4 
#8 23 12 20 

table(Khan$ytest)
#1 2 3 4 
#3 6 6 5 

dat = data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out = svm(y~.,data = dat,kernel="linear",cost=10,scale = FALSE)
summary(out)

table(predicted =out$fitted,Actual =dat$y)

#           Actual
#predicted  1  2  3  4
#        1  8  0  0  0
#        2  0 23  0  0
#        3  0  0 12  0
#        4  0  0  0 20


dat.te = data.frame(x=Khan$xtest,y= as.factor(Khan$ytest))

pred.te = predict(out,newdata = dat.te)
table(predicted =pred.te,actual =dat.te$y)
#          actual
#predicted 1 2 3 4
#        1 3 0 0 0
#        2 0 6 2 0
#        3 0 0 4 0
#        4 0 0 0 5






