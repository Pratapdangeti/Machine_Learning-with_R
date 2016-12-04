


rm(list = ls())
library(ISLR)
fix(Default)
attach(Default)
names(Default)

dim(Default)
#[1] 10000     4

# a - using simple glm and summary, determine  estimated std.error
glm.fit = glm(default~balance+income,data = Default,family = binomial)
summary(glm.fit)

#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
#  balance     5.647e-03  2.274e-04  24.836  < 2e-16 ***
#  income      2.081e-05  4.985e-06   4.174 2.99e-05 ***



#b - write a boost.fn() function, which takes data & index and write coefficients

boost.fn = function(data,index){
  glm.fit2 = glm(default~balance+income,data = data,family = binomial,subset = index)
  glm.fit2$coef
}

boost.fn(Default,sample(10000,10000,replace = T))
#(Intercept)       balance        income 
#-1.122814e+01  5.353288e-03  2.511801e-05 


library(boot)
set.seed(3)
boot(Default,boost.fn,R=100)

#Bootstrap Statistics :
#       original        bias     std. error
#t1* -1.154047e+01 -5.466614e-03 4.335906e-01
#t2*  5.647103e-03 -4.060986e-06 2.310579e-04
#t3*  2.080898e-05  3.866870e-07 4.387509e-06









