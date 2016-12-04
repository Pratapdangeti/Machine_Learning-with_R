
rm(list=ls())
library(ISLR)
library(class)


fix(Caravan)
attach(Caravan)
dim(Caravan)
summary(Purchase)
prop.table(table(Purchase))
names(Caravan)

# Rescaling to standardize
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]



set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
table(knn.pred,test.Y)
mean(knn.pred==test.Y)

knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)

knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)


# fitting logistic regression on the same data
glm.fit= glm(Purchase~.,data = Caravan,family = binomial,subset = -test)
glm.fit
summary(glm.fit)
glm.probs= predict(glm.fit,Caravan[test,],type = "response")
glm.pred = rep("No",1000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,test.Y)


glm.pred = rep("No",1000)
glm.pred[glm.probs>0.25]="Yes"
table(glm.pred,test.Y)






