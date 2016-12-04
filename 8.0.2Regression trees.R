

rm(list = ls())

library(MASS)
library(tree)
set.seed(1)

fix(Boston)
attach(Boston)
dim(Boston)
#[1] 506  14

names(Boston)
#[1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
#[8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"

train = sample(1:nrow(Boston),nrow(Boston)/2)

tree.boston = tree(medv~.,data = Boston,subset = train)
summary(tree.boston)


plot(tree.boston)
text(tree.boston,pretty = 0)


cv.boston = cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type = 'b')


prune.boston = prune.tree(tree.boston,best = 5)
plot(prune.boston)
text(prune.boston,pretty = 0)



yhat = predict(tree.boston,newdata = Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
#[1] 25.04559


#yhat = predict(prune.boston,newdata = Boston[-train,])
#boston.test=Boston[-train,"medv"]
#plot(yhat,boston.test)
#abline(0,1)
#mean((yhat-boston.test)^2)
#[1] 26.83413































































