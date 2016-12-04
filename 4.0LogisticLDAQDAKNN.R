

rm(list = ls())

library(ISLR)
fix(Smarket)
attach(Smarket)

names(Smarket)
summary(Smarket)


pairs(Smarket)
cor(Smarket[,-9])
plot(Volume)


# Logistic Regression
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = binomial,data = Smarket )

summary(glm.fit)
coef(glm.fit)

summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

glm.probs=predict(glm.fit,type = "response")
glm.probs[1:10]

contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>0.5]="Up"

prop.table(table(glm.pred))

table(glm.pred,Direction)

mean(glm.pred==Direction)



# Train & test sample method

train = (Year<2005)
train
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]


glm.fit2 <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial,subset = train )
summary(glm.fit2)
glm.probs2 <- predict(glm.fit2,Smarket.2005,type = "response")
glm.pred2<- rep("Down",252)
glm.pred2[glm.probs2>0.5]="Up"
table(glm.pred2,Direction.2005)
mean(glm.pred2==Direction.2005)

# Train & test sample for significant variables
glm.fit3 <- glm(Direction~Lag1+Lag2,data = Smarket,family = binomial,subset = train )
glm.probs3<-predict(glm.fit3,Smarket.2005,type="response")
glm.pred3<-rep("Down",252)
glm.pred3[glm.probs3>0.5]="Up"
table(glm.pred3,Direction.2005)
mean(glm.pred3==Direction.2005)

predict(glm.fit3,newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type = "response")



# Linear Discriminant Analysis
library(MASS)
train = (Year<2005)
train
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

lda.fit<-lda(Direction~Lag1+Lag2,data = Smarket,subset = train)
lda.fit

lda.pred<-predict(lda.fit,Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)

sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

# we wish to predict market only we are very certain
sum(lda.pred$posterior[,1]>0.9)



# Quadratic Discriminant Analysis
qda.fit=qda(Direction~Lag1+Lag2,data = Smarket,subset = train)
qda.fit


qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)


#KNN - K Nearest Neighbour
train = (Year<2005)
train
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction = Direction[train]



set.seed(1)
knn.pred <- knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)



































































