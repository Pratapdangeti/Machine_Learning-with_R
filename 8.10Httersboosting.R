

rm(list = ls())

library(ISLR)
fix(Hitters)
attach(Hitters)

dim(Hitters)
#[1] 322  20
names(Hitters)
#[1] "AtBat"     "Hits"      "HmRun"     "Runs"      "RBI"       "Walks"     "Years"     "CAtBat"    "CHits"    
#[10] "CHmRun"    "CRuns"     "CRBI"      "CWalks"    "League"    "Division"  "PutOuts"   "Assists"   "Errors"   
#[19] "Salary"    "NewLeague"

str(Hitters)
summary(Hitters$Salary)


#a - remove NA in Salary and apply log transform the salaries
Hitters_n <- subset(Hitters, !is.na(Salary) )
summary(Hitters_n$Salary)

Hitters_n$Salary = log(Hitters_n$Salary)
summary(Hitters_n$Salary)


#b - training set consiting of 200 observations

train = sample(nrow(Hitters_n),200)

Salary.train = Hitters_n$Salary[train]
Salary.test = Hitters_n$Salary[-train]

#c - perform boosting on train dataset with 1000 trees
library(gbm)

set.seed(2)
boost.Hitters = gbm(Salary~.,data = Hitters_n[train,],
        distribution = "gaussian",n.trees = 1000,interaction.depth = 4,shrinkage = 0.05)

pred.boost.hitters =predict(boost.Hitters,newdata = Hitters_n[-train,],n.trees = 1000)
mean((pred.boost.hitters-Salary.test)^2)

# Shrinkage = 0.001, [1] 0.2195488
# Shrinkage = 0.2, [1] 0.2129617
# Shrinkage = 0.1, [1] 0.1960525
# Shrinkage = 0.05, [1] 0.1651418

#e - compare test MSE with liner regression

glm_fit = glm(Salary~.,data = Hitters_n,subset = train)
summary(glm_fit)
glm_pred = predict(glm_fit,newdata = Hitters_n[-train,])
mean((glm_pred-Salary.test)^2)

# [1] 0.333478

# f Which variable appear to be most important in gbm model
summary(boost.Hitters)
par(mfrow=c(1,2))
plot(boost.Hitters,i="CRuns")
plot(boost.Hitters,i="CAtBat")
par(mfrow=c(1,1))

# g - apply bagging to the training set
library(randomForest)

rf.hitters = randomForest(Salary~.,data = Hitters_n,subset = train,
                          mtry = 19,importance = TRUE)

rf_pred = predict(rf.hitters,newdata = Hitters_n[-train,])
mean((rf_pred-Salary.test)^2)
#[1] 0.119281



