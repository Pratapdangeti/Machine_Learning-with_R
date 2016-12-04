
rm(list = ls())

library(ISLR)
fix(Caravan)
attach(Caravan)
dim(Caravan)
#[1] 5822   86

Caravan$Purchase = as.factor(ifelse(Purchase=="Yes",1,0))
table(Caravan$Purchase)

#a - Create training set with first 1000 obs
train = 1:1000
train_Caravan = Caravan[train,]
test_Caravan = Caravan[-train,]



#b - fitting boosting with n.trees = 1000, shrinkage = 0.01
library(gbm)
boost.Caravan = gbm(Purchase~.,data = train_Caravan,distribution = "gaussian",
        interaction.depth = 4,n.trees = 1000,shrinkage = 0.01,verbose = F)


summary(boost.Caravan)


            #var     rel.inf
#PPERSAUT PPERSAUT 7.400603703
#MKOOPKLA MKOOPKLA 5.571431944
#MGODGE     MGODGE 4.763380343
#MOPLHOOG MOPLHOOG 4.649698310
#MOSTYPE   MOSTYPE 4.400227564
#PBRAND     PBRAND 4.146411530
#MBERMIDD MBERMIDD 3.672703380
#MINK3045 MINK3045 3.394508735
#MGODPR     MGODPR 3.116595789
#MAUT2       MAUT2 2.991917664
#MSKB1       MSKB1 2.829514401
#MSKC         MSKC 2.724676301



# c - use boosting model to predict remanining training dataset
prob.caravan = predict(boost.Caravan,newdata = test_Caravan,n.trees = 1000)
prob.caravan[1:10]

summary(prob.caravan)
length(prob.caravan)
#4822

pred.caravan = rep(1,4822)
pred.caravan[prob.caravan<=1.2] = 0
table(pred.caravan)

table(actual = test_Caravan$Purchase,pred = pred.caravan)
#           pred
#actual    0    1
#      0 4256  277
#      1  240   49




