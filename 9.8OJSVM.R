

rm(list = ls())

library(ISLR)
library(e1071)
fix(OJ)
attach(OJ)
names(OJ)

#[1] "Purchase"       "WeekofPurchase" "StoreID"        "PriceCH"        "PriceMM"        "DiscCH"        
#[7] "DiscMM"         "SpecialCH"      "SpecialMM"      "LoyalCH"        "SalePriceMM"    "SalePriceCH"   
#[13] "PriceDiff"      "Store7"         "PctDiscMM"      "PctDiscCH"      "ListPriceDiff"  "STORE"     

dim(OJ)
#[1] 1070   18


train = sample(1070,800)

OJ.train = OJ[train,]
OJ.test = OJ[-train,]
#b
svm.simple = svm(Purchase~.,data = OJ.train,kernel="linear",cost = 0.01)
summary(svm.simple)
#c
table(pred = predict(svm.simple,newdata = OJ.train),actual = OJ.train$Purchase)
#actual
#pred  CH  MM
#CH 425  73
#MM  62 240
#error% - 16.8%
table(pred = predict(svm.simple,newdata = OJ.test),actual = OJ.test$Purchase)
#actual
#pred  CH  MM
#CH 150  27
#MM  16  77
#error % - 15.9%

#d - using tune parameter to find optimal cost

tune.out = tune(svm,Purchase~.,data = OJ.train,kernel = "linear",
          ranges = list(cost = c(0.01,0.1,1,5,10)))

summary(tune.out)
tunebm=tune.out$best.model

#e- finding test and train errors
table(pred = predict(tunebm,newdata = OJ.train),actual = OJ.train$Purchase)
#     actual
#pred  CH  MM
#   CH 421  72
#   MM  66 241
table(pred = predict(tunebm,newdata = OJ.test),actual = OJ.test$Purchase)
#actual
#pred  CH  MM
#   CH 149  27
#   MM  17  77





#f - repeat using radial kernel

svm.rad = svm(Purchase~.,data = OJ.train,kernel="radial",gamma=1,cost=0.01,degree=1)
summary(svm.rad)
table(pred = predict(svm.rad,newdata = OJ.train),actual = OJ.train$Purchase)
#actual
#pred  CH  MM
#CH 487 313
#MM   0   0

# radial is completely out of 

#trying polynomial

tune.out.poly = tune(svm,Purchase~.,data = OJ.train,kernel = "polynomial",gamma=1,cost=0.01,
                ranges = list(degree = c(4)))
table(pred = predict(tune.out.poly$best.model,newdata = OJ.train),actual = OJ.train$Purchase)
table(pred = predict(tune.out.poly$best.model,newdata = OJ.test),actual = OJ.test$Purchase)


#actual-degree 1
#pred  CH  MM
#CH 150  27
#MM  16  77

#actual-degree 2
#pred  CH  MM
#CH 153  41
#MM  13  63

#actual-degree 3
#pred  CH  MM
#CH 152  42
#MM  14  62

#actual-degree 4
#pred  CH  MM
#CH 148  41
#MM  18  63









