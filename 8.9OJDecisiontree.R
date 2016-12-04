


rm(list = ls())

library(ISLR)
library(tree)
fix(OJ)
attach(OJ)
names(OJ)
#[1] "Purchase"       "WeekofPurchase" "StoreID"        "PriceCH"       
#[5] "PriceMM"        "DiscCH"         "DiscMM"         "SpecialCH"     
#[9] "SpecialMM"      "LoyalCH"        "SalePriceMM"    "SalePriceCH"   
#[13] "PriceDiff"      "Store7"         "PctDiscMM"      "PctDiscCH"     
#[17] "ListPriceDiff"  "STORE" 

str(OJ)
dim(OJ)
#[1] 1070   18
#a
set.seed(2)
train = sample(nrow(OJ),800)
purchase.test = OJ[-train,"Purchase"]

#b
OJ.tree = tree(Purchase~.,data = OJ,subset = train)
summary(OJ.tree)
plot(OJ.tree)
text(OJ.tree,pretty = 0)

pred.OJ = predict(OJ.tree,newdata=OJ[train,],type="class")
pred.OJ
table(OJ[train,"Purchase"],pred.OJ)

#     pred.OJ
#    CH  MM
#CH 440  40
#MM  94 226

# train error % - 16.75

#e - predict response on test data
pred.testOJ = predict(OJ.tree,newdata=OJ[-train,],type="class")
table(OJ[-train,"Purchase"],pred.testOJ)
#pred.testOJ
#    CH  MM
#CH 161  12
#MM  28  69
#test error% - 14.81 %

#f - apply cross validation
set.seed(4)
cv_OJtree = cv.tree(OJ.tree,FUN = prune.misclass)
names(cv_OJtree)
plot(cv_OJtree$size,cv_OJtree$dev,type = "b")

prune_OJtree = prune.misclass(OJ.tree,best = 8)
plot(prune_OJtree)
text(prune_OJtree,pretty = 0)

pred.prune.OJ = predict(prune_OJtree,newdata=OJ[train,],type = "class")
table(OJ[train,"Purchase"],pred.prune.OJ)

#pred.prune.OJ
#    CH  MM
#CH 380 100
#MM  60 260

# train error rate - 20%

pred.prune.testOJ = predict(prune_OJtree,newdata=OJ[-train,],type = "class")
table(OJ[-train,"Purchase"],pred.prune.testOJ)
#pred.prune.testOJ
#CH  MM
#CH 140  33
#MM  21  76
#test error rate - 20%

# 4 node test error -15.5%
# 7 node test error - 14.8%
# 8 nodd test error - 14.8 %














