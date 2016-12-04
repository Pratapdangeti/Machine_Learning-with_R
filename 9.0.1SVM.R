


rm(list=ls())
library(e1071)

set.seed(1)
x=matrix(rnorm(20*2),ncol = 2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1

plot(x,col=(3-y))


dat = data.frame(x=x,y=as.factor(y))
svmfit = svm(y~.,data = dat,kernel="linear",cost=10,
             scale = FALSE)

plot(svmfit,dat)

svmfit$index
summary(svmfit)

# low cost
svmfit = svm(y~.,data = dat,kernel="linear",cost=0.1,
             scale = FALSE)

plot(svmfit,dat)

svmfit$index
summary(svmfit)



set.seed(1)
tune.out = tune(svm,y~.,data = dat,kernel="linear",
   ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))


summary(tune.out)


#- best parameters:
#  cost
#  0.1

bestmod = tune.out$best.model
summary(bestmod)

#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  0.1 
#gamma:  0.5 

#Number of Support Vectors:  16
#( 8 8 )

#Number of Classes:  2 
#Levels: 
#  -1 1



# checking with test data
xtest = matrix(rnorm(20*2),ncol = 2)
ytest = sample(c(-1,-1),20,rep=TRUE)
xtest[ytest==1,] = xtest[ytest==1,] + 1
testdat = data.frame(x=xtest,y=as.factor(ytest))

ypred = predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)



# Second example- linearly inseparable

set.seed(1)
x=matrix(rnorm(20*2),ncol = 2)

y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+0.5

plot(x,col=(y+5)/2,pch=19)

dat = data.frame(x=x,y=as.factor(y))
svmfit = svm(y~.,data = dat,kernel="linear",cost = 1e5)
summary(svmfit)


plot(svmfit,dat)


#9.6.2 - Support Vector Machine

set.seed(1)
x= matrix(rnorm(200*2),ncol = 2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat = data.frame(x=x,y=as.factor(y))
str(dat)
plot(x,col=y)
train = sample(200,100)
svmfit = svm(y~.,data = dat,kernel="radial",
             gamma=1,cost=1)


plot(svmfit,dat[train,])

summary(svmfit)


svmfit = svm(y~.,data = dat,kernel="radial",
             gamma=1,cost=1e5)


plot(svmfit,dat[train,])
summary(svmfit)


set.seed(1)
tune.out = tune(svm,y~.,data = dat[train,],kernel="radial",
    ranges = list(cost=c(0.1,1,10,100,1000),
    gamma=c(0.5,1,2,3,4)))
summary(tune.out)


table(true=dat[-train,"y"],pred = predict(tune.out$best.model,
          newx=dat[-train,]))

#pred
#true  1  2
#    1 56 21
#    2 18  5

# error% = 39%


# ROC curves
library(ROCR)

rocplot = function(pred,truth,...){
  predob = prediction(pred,truth)
  perf = performance(predob,"tpr","fpr")
  plot(perf,...)
}


svmfit.opt = svm(y~.,data=dat[train,],kernel="radial",
    gamma=2,cost=1,decision.values=T)


fitted=attributes(predict(svmfit.opt,dat[train,],
            decision.values = TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")


svmfit.flex = svm(y~.,data = dat[train,],kernel="radial",
    gamma=50,cost=1,decision.values = T)
fitted = attributes(predict(svmfit.flex,dat[train,],
            decision.values = TRUE))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")




# fitting on test data
fitted = attributes(predict(svmfit.opt,dat[-train,],
      decision.values = T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")

fitted = attributes(predict(svmfit.flex,dat[-train,],
      decision.values = T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")




#9.6.4 - SVM with Multiple Classes


set.seed(1)
x=rbind(x,matrix(rnorm(50*2),ncol = 2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat = data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

svmfit = svm(y~.,data = dat,kernel = "radial",
    cost = 10,gamma=1)
plot(svmfit,dat)










