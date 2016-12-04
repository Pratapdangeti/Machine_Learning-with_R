

rm(list=ls())
library(ISLR)
library(e1071)

# a - genarating dataset

set.seed(1)
x1 = runif(500)-0.5
x2= runif(500)-0.5
y=1*(x1^2-x2^2>0)

x1sq = x1^2  
x2sq = x2^2
x1x2 = 2*x1*x2
x=cbind(x1,x2,x1sq,x2sq,x1x2)




dat = data.frame(x=x,y=as.factor(y))
plot(x,col=y)

set.seed(3)
train = sample(500,300)
dat.train = dat[train,]
dat.test = dat[-train,]

glm.lgstc = glm(y~x.x1+x.x2,data = dat.train,family = binomial)

summary(glm.lgstc)

glm.probs.train = predict(glm.lgstc,dat.train,type = "response")

glm.pred.train = rep(1,300)
glm.pred.train[glm.probs.train<0.5]=0

table(pred=glm.pred.train,actual =dat.train$y)

#     actual
#pred  0  1
#   0 57 66
#   1 90 87

glm.probs.test = predict(glm.lgstc,dat.test,type = "response")
glm.pred.test = rep(1,200)
glm.pred.test[glm.probs.test<0.5]=0
table(pred=glm.pred.test,actual =dat.test$y)

#     actual
#pred  0  1
#   0 30 39
#   1 71 60


# Mapping to non linear

head(dat.train)
dat.train.hd = dat.train[,-c(1,2)]
head(dat.train.hd)

glm.lgstc.hd = glm(y~.,data = dat.train.hd)

glm.lgstc = glm(y~x.x1+x.x2,data = dat.train,family = binomial)

  









































































