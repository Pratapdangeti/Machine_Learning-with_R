


rm(list = ls())
library(ISLR)
library(e1071)


set.seed(1)


set.seed(1)
x1=rep(0,100)
x2=rep(0,100)
y=rep(2,100)

for (i in 1:100){
  a = i*0.0628319
  x1[i]=cos(a)+(i>50)+runif(1,0,7)*0.1
  x2[i] = sin(a)+(i>50)*0.3+runif(1,0,7)*0.1  
}

y[x1>0&x2<0.5]=1


xp=cbind(x1,x2)
dim(xp)
dat1 = data.frame(x=xp,y=as.factor(y))
plot(xp,col=y)
str(dat1)

train = sample(100,60)
dat1.train = dat1[train,]
dat1.test = dat1[-train,]

# Support vector classification - linear with penalty
#svmlp = svm(y~.,data = dat1.train,kernel="linear",cost = 1e5)

svmlp = svm(y~.,data = dat1.train,kernel="linear")
plot(svmlp,dat1.train)

table(pred = predict(svmlp,newdata = dat1.train),true = dat1.train$y)
#      true
#pred  1  2
#    1 22  0
#    2  4 34
table(pred = predict(svmlp,newdata = dat1.test),true = dat1.test$y)
#      true
#pred  1  2
#    1 21  0
#    2  1 18

svmply = svm(y~.,data = dat1.train,kernel = "polynomial",gamma=1,degree=3,cost=1e5)

table(pred = predict(svmply,newdata = dat1.train),true = dat1.train$y)
#      true
#pred  1  2
#    1 25  0
#    2  1 34

table(pred = predict(svmply,newdata = dat1.test),true = dat1.test$y)
#     true
# pred  1  2
#     1 22  0
#     2  0 18


plot(svmply,dat1.test)

svmrad = svm(y~.,data = dat1.train,kernel = "radial",gamma=1)
table(pred = predict(svmrad,newdata = dat1.train),true = dat1.train$y)
#      true
#pred  1  2
#    1 26  0
#    2  0 34
table(pred = predict(svmrad,newdata = dat1.test),true = dat1.test$y)
#     true
#pred  1  2
#   1 22  0
#   2  0 18
plot(svmrad,dat1.test)







