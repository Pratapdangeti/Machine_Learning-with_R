


rm(list = ls())


library(ISLR)

fix(Auto)
attach(Auto)

names(Auto)
#[1] "mpg"          "cylinders"    "displacement" "horsepower"   "weight"      
#[6] "acceleration" "year"         "origin"       "name" 


# 5.3.1 - Validation set approach
set.seed(1)
train = sample(392,196)

lm.fit = lm(mpg~horsepower,data = Auto,subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data = Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data = Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)



set.seed(2)
train = sample(392,196)

lm.fit = lm(mpg~horsepower,data = Auto,subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#[1] 23.29559
lm.fit2=lm(mpg~poly(horsepower,2),data = Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#[1] 18.90124
lm.fit3=lm(mpg~poly(horsepower,3),data = Auto,subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
#[1] 19.2574

# 5.3.2 - Leave-one-out cross-validation
glm.fit=glm(mpg~horsepower,data = Auto)
coef(glm.fit)

lm.fit = lm(mpg~horsepower,data = Auto)
coef(lm.fit)


library(boot)
glm.fit=glm(mpg~horsepower,data = Auto)
cv.err = cv.glm(Auto,glm.fit)
cv.err$delta
#[1] 24.23151 24.23114

# Cross validation for various polynomial degrees
cv.error = rep(0,5)
for (i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}

cv.error
#[1] 24.23151 19.24821 19.33498 19.42443 19.03321


#5.3.3 - K-Fold Cross-Validation
set.seed(17)
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data = Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
#[1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609 19.71201
#[9] 18.95140 19.50196


# 5.3.4 - Bootstrap

fix(Portfolio)
attach(Portfolio)
names(Portfolio)
dim(Portfolio)
#[1] 100   2

alpha.fn = function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)
#[1] 0.5758321

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace = T))
#[1] 0.5963833


boot(Portfolio,alpha.fn,R=1000)



# Estimating accuracy of Linear regression model using Bootstrap

boot.fn = function(data,index){
  return(coef(lm(mpg~horsepower,data = data,subset = index)))
}

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace = T))

boot.fn(Auto,sample(392,392,replace = T))

# bootstrap
boot(Auto,boot.fn,1000)

# Bootstrapping for polynomial fit


boot.fn = function(data,index){
  return(coef(lm(mpg~horsepower+I(horsepower^2),data = data,subset = index)))
}

set.seed(1)
boot(Auto,boot.fn,1000)























































