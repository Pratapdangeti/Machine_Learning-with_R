


rm(list = ls())
library(ISLR)
fix(Auto)
attach(Auto)
names(Auto)
dim(Auto)

# dim(Auto)
# 392   9

mpg01 = rep(1,392)
mpg01[mpg<median(mpg)]=0
mpg01

mpg
median(mpg)

Auto_n=data.frame(Auto,mpg01)
head(Auto_n)

# b- exploring the relations of variables with mpg01

plot(Auto_n)
cor(Auto_n[,-9])
str(Auto_n)

#mpg  cylinders displacement horsepower     weight acceleration       year     origin      mpg01
#mpg           1.0000000 -0.7776175   -0.8051269 -0.7784268 -0.8322442    0.4233285  0.5805410  0.5652088  0.8369392
#cylinders    -0.7776175  1.0000000    0.9508233  0.8429834  0.8975273   -0.5046834 -0.3456474 -0.5689316 -0.7591939
#displacement -0.8051269  0.9508233    1.0000000  0.8972570  0.9329944   -0.5438005 -0.3698552 -0.6145351 -0.7534766
#horsepower   -0.7784268  0.8429834    0.8972570  1.0000000  0.8645377   -0.6891955 -0.4163615 -0.4551715 -0.6670526
#weight       -0.8322442  0.8975273    0.9329944  0.8645377  1.0000000   -0.4168392 -0.3091199 -0.5850054 -0.7577566
#acceleration  0.4233285 -0.5046834   -0.5438005 -0.6891955 -0.4168392    1.0000000  0.2903161  0.2127458  0.3468215
#year          0.5805410 -0.3456474   -0.3698552 -0.4163615 -0.3091199    0.2903161  1.0000000  0.1815277  0.4299042
#origin        0.5652088 -0.5689316   -0.6145351 -0.4551715 -0.5850054    0.2127458  0.1815277  1.0000000  0.5136984
#mpg01         0.8369392 -0.7591939   -0.7534766 -0.6670526 -0.7577566    0.3468215  0.4299042  0.5136984  1.0000000



# c- splitting data into training & test datasets
args(sample)
set.seed(1)

train = sample(1:nrow(Auto_n),300,replace=FALSE)

Auto_n.train = Auto_n[train,]
Auto_n.test = Auto_n[-train,]

mpg01.train = mpg01[train]
mpg01.test = mpg01[-train]

# d - performing LDA with most appropriate variables identified in b
# acceleration & year does not seems to be very much related with mpg01

names(Auto_n.test)
#[1] "mpg"          "cylinders"    "displacement" "horsepower"   "weight"       "acceleration" "year"        
#[8] "origin"       "name"         "mpg01"   

library(MASS)

# selecting all variables with correlation coefficient more than 0.5
# removing acceleration & year variables from analysis

lda.fit <- lda(mpg01~cylinders+displacement+horsepower+weight+origin,data = Auto_n.train)
 
names(lda.fit)
lda.fit

lda.predict<-predict(lda.fit,Auto_n.test)

lda.class <-lda.predict$class

table(lda.class,mpg01.test)

# mpg01.test
# lda.class  0  1
#         0 37  1
#         1  5 49

#error% = 6.52 %


# e - perform QDA with variables in LDA 
qda.fit <- qda(mpg01~cylinders+displacement+horsepower+weight+origin,data = Auto_n.train)
qda.predict<-predict(qda.fit,Auto_n.test)

qda.class <-qda.predict$class

table(qda.class,mpg01.test)
#         mpg01.test
#qda.class  0  1
#        0 39  4
#        1  3 46
#error% - 7.61%

#f - perform logistic regression with significant variables identified in b
glm.fit<-glm(mpg01~cylinders+displacement+horsepower+weight+origin,family=binomial,data = Auto_n.train)

glm.probs = predict(glm.fit,Auto_n.test,type = "response")

glm.predict = rep("Down",92)
glm.predict[glm.probs>0.5]="Up"

table(glm.predict,mpg01.test)
#           mpg01.test
#glm.predict  0  1
#       Down 35  3
#       Up    7 47

# error % - 10.8%

#g - perform KNN with appropriate variables from b
knnauto_n.train <- Auto_n.train[,-c(1,6,7,9,10)]
knnauto_n.test=Auto_n.test[,-c(1,6,7,9,10)]

library(class)
set.seed(3)
knn.fit = knn(knnauto_n.train,knnauto_n.test,mpg01.train,k=30)

table(knn.fit,mpg01.test)

#      mpg01.test
#knn.fit  0  1
#      0 34  7
#      1  8 43
# error % - 16.3%

#      mpg01.test
#knn.fit  0  1
#      0 37  3
#      1  5 47
# with k = 30 , error % = 8.69%






















