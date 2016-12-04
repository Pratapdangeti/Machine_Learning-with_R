
rm(list = ls())



library(ISLR)
library(boot)

?Weekly
fix(Weekly)
attach(Weekly)
dim(Weekly)
#[1] 1089    9

# a- Fit a logistic regression model
glm.fit = glm(Direction~Lag1+Lag2,data = Weekly,family = binomial)

# b-Fit a logistic model all but first observation
glm.fit2 = glm(Direction~Lag1+Lag2,data = Weekly[-1,],family = binomial)


#Weekly[1,]
#  Year  Lag1  Lag2   Lag3   Lag4   Lag5   Volume Today Direction
#1 1990 0.816 1.572 -3.936 -0.229 -3.484 0.154976 -0.27      Down

one_test = Weekly[1,]
one_test
glm.pred=predict(glm.fit2,one_test)
glm.pred
# 0.287534 

#d- manual LOOCV
glm.pred = rep(0,nrow(Weekly))

for(i in 1:nrow(Weekly)){
  glm.fit = glm(Direction~Lag1+Lag2,data = Weekly[-i,],family = binomial)
  one_test = Weekly[i,]
  glm.pred[i]=predict(glm.fit,one_test)
}

glm.pred

glm.dir = rep("Up",1089)
glm.dir[glm.pred<0.5]="Down"

table(glm.dir,Weekly$Direction)
#glm.dir Down  Up
#  Down  468 578
#  Up     16  27

#e - Average of n number obained in LOOCV
new_tab=data.frame(glm.dir,Weekly$Direction)

ind = rep(1,1089)
new_tab = data.frame(new_tab,ind)
head(new_tab)
new_tab$ind[new_tab$glm.dir==new_tab$Weekly.Direction]=0

head(new_tab,100)
mean(new_tab$ind)


















