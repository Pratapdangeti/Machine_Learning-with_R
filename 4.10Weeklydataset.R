


rm(list = ls())
library(ISLR)
fix(Weekly)
attach(Weekly)
dim(Weekly)
# Rows 1089 Cols 9
names(Weekly)
summary(Weekly)
# a
cor(Weekly[,-9])
plot(Weekly)

# b - Logistic regression

args(glm)
glm.fit<-glm(Direction~Year+Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = binomial,data = Weekly)
summary(glm.fit)

args(predict)
glm.probs = predict(glm.fit,type = "response")

glm.probs[1:10]

glm.pred = rep("Down",1089)
glm.pred[glm.probs>0.5]="Up"

glm.pred[1:10]

table(glm.pred,Direction)

#c Confusion matrix of simple logistic regression
#         Direction
#glm.pred Down  Up
#Down     56  47
#Up       428 558

#d Logistic regression with training & testing datasets

train = (Year<2009)
Weekly.test = Weekly[!train,] 
head(Weekly.test)
Direction.test = Direction[!train]
head(Direction.test)

glm.fit2 <- glm(Direction~Lag2,data = Weekly,family = binomial,subset = train)
glm.probs2<-predict(glm.fit2,Weekly.test,type = "response")
Weekly.test

glm.pred2 = rep("Down",104)
glm.pred2[glm.probs2>0.5]="Up"

table(glm.pred2,Direction.test)

#Confusion matrix of Logistic regression with  train & test
#Direction.test
#glm.pred2 Down Up
#Down    9  5
#Up     34 56




#e - repeating d with LDA 

library(MASS)

train = (Year<2009)
Weekly.test = Weekly[!train,] 
head(Weekly.test)
Direction.test = Direction[!train]
head(Direction.test)


lda.fit=lda(Direction~Lag2,data = Weekly,subset = train)
lda.fit

lda.pred<-predict(lda.fit,Weekly.test)
names(lda.pred)
lda.class<-lda.pred$class
table(lda.class,Direction.test)

# Confusion matrix of LDA
#Direction.test
#lda.class Down Up
#Down    9  5
#Up     34 56



#f- repeat d with QDA
qda.fit <- qda(Direction~Lag2,data = Weekly,subset = train)
qda.class <- predict(qda.fit,Weekly.test)$class

table(qda.class,Direction.test)

# Confusion matrix of QDA
#Direction.test
#qda.class Down Up
#Down    0  0
#Up     43 61


#g - KNN

library(class)

train = (Year<2009)

Weekly.train <- Lag2[train]
Weekly.train
Weekly.test<- Lag2[!train]
Weekly.test

Direction.train = Direction[train]
Direction.train
Direction.test = Direction[!train]
Direction.test

set.seed(12345)
knn.fit = knn(data.frame(Weekly.train),data.frame(Weekly.test),Direction.train,k=10)

table(knn.fit,Direction.test)

# Confusion matrix of KNN
#Direction.test
#knn.fit Down Up
#Down   21 30
#Up     22 31




#c Confusion matrix of simple logistic regression
#         Direction
#glm.pred Down  Up
#Down     56  47
#Up       428 558
#error% = 56.3 %

#Confusion matrix of Logistic regression with  train & test
#Direction.test
#glm.pred2 Down Up
#Down    9  5
#Up     34 56
#error%=62.5%  

# Confusion matrix of LDA
#Direction.test
#lda.class Down Up
#Down    9  5
#Up     34 56
#error%= 62.5%  

# Confusion matrix of QDA
#Direction.test
#qda.class Down Up
#Down    0  0
#Up     43 61
#error%=58.6%  

# Confusion matrix of KNN with k=1
#Direction.test
#knn.fit Down Up
#Down   21 30
#Up     22 31
#error% = 50%

# Confusion matrix of KNN with k=10  
#  Direction.test
#knn.fit Down Up
#Down   18 21
#Up     25 40  
#error% = 55.7%






















