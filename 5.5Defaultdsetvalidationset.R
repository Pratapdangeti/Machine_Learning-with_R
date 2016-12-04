


rm(list = ls())

library(ISLR)
fix(Default)
attach(Default)

names(Default)
#[1] "default" "student" "balance" "income" 

dim(Default)
#[1] 10000     4

#a - fit a logistic regression
glm.fit = glm(default~balance+income,data = Default,family = binomial)
summary(glm.fit)

#b - validation set approach
set.seed(1)
train = sample(10000,7000)

default_train = Default[train,]
default_test = Default[-train,]

dim(default_train)
dim(default_test)

default01_train = Default$default[train]
default01_test = Default$default[-train]

glm.fit2 = glm(default~balance+income,data = default_train,family = binomial)
glm.probs2 = predict(glm.fit2,default_test,type = "response")

glm.predict2 = rep(1,3000)
glm.predict2[glm.probs2<0.5]=0

table(glm.predict2,default01_test)

#          default01_test
#glm.predict2   No  Yes
#            0 2882   67
#            1   21   30

# error% - 2.93%




# c - repeating b 3 times with 3 types of data splits

set.seed(3)
train = sample(10000,7000)

default_train = Default[train,]
default_test = Default[-train,]

dim(default_train)
dim(default_test)

default01_train = Default$default[train]
default01_test = Default$default[-train]

glm.fit2 = glm(default~balance+income,data = default_train,family = binomial)
glm.probs2 = predict(glm.fit2,default_test,type = "response")

glm.predict2 = rep(1,3000)
glm.predict2[glm.probs2<0.5]=0

table(glm.predict2,default01_test)

# seed(2)
#default01_test
#glm.predict2   No  Yes
#            0 2900   65
#            1    7   28
# error% = 2.4%

# seed(3)
#default01_test
#glm.predict2   No  Yes
#            0 2908   57
#            1   11   24
# error% = 2.2%


#d - includ student dummy variable and predict with validation set approach


set.seed(3)
train = sample(10000,7000)

default_train = Default[train,]
default_test = Default[-train,]

dim(default_train)
dim(default_test)

default01_train = Default$default[train]
default01_test = Default$default[-train]

glm.fit3 = glm(default~balance+income+student,data = default_train,family = binomial)
glm.probs3 = predict(glm.fit3,default_test,type = "response")

summary(glm.fit3)
glm.predict3 = rep(1,3000)
glm.predict3[glm.probs3<0.5]=0

table(glm.predict3,default01_test)

#default01_test
#glm.predict3   No  Yes
#           0 2910   57
#           1    9   24
#error% - 2.2 %



