

rm(list = ls())

library(MASS)
library(randomForest)
library(e1071)

fix(Boston)
dim(Boston)
#[1] 506  14
names(Boston)
#[1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"     
#[7] "age"     "dis"     "rad"     "tax"     "ptratio" "black"  
#[13] "lstat"   "medv"

set.seed(1)
train = sample(1:nrow(Boston),nrow(Boston)/2)
boston.test=Boston[-train,"medv"]

rf.boston = randomForest(medv~.,data = Boston,subset = train,
                         mtry=6,ntree=25)
yhat.rf = predict(rf.boston,newdata= Boston[-train,])
mean((yhat.rf-boston.test)^2)
#[1] 10.87297


tune.rf = tune(randomForest,medv~.,data = Boston[train,],
               ranges=list(mtry=c(6,7,8,9,10,11,12,13),ntree = c(20,25,50)))
smry =summary(tune.rf)


yhat.tune = predict(tune.rf$best.model,newdata = Boston[-train,])
mean((yhat.tune-boston.test)^2)
#[1] 12.82514
smry$performances

plot(smry$performances[,c(1,3)],col = (smry$performances[,2]))









