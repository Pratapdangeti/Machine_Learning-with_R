
# Need to work on this


rm(list = ls())

library(ISLR)
library(MASS)
fix(Boston)
attach(Boston)
dim(Boston)
#[1] 506  14
names(Boston)
#[1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
#[8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv" 


#a - population mean of medv
mean(medv)
#[1] 22.53281

#b- estimate of SE of question a
set.seed(3)
train = sample(506,300)
sam_medvmean = medv[train]
mean(sam_medvmean)
sqrt(var(sam_medvmean))/300

#c - bootstrap

library(boot)
boot(sam_medvmean,sqrt(var(sam_medvmean))/300,1000)

































