


rm(list = ls())

library(ISLR)
library(tree)


fix(Carseats)
attach(Carseats)
dim(Carseats)
#[1] 400  11
names(Carseats)
#[1] "Sales"       "CompPrice"   "Income"      "Advertising" "Population"  "Price"      
#[7] "ShelveLoc"   "Age"         "Education"   "Urban"       "US"  

set.seed(2)
train = sample(400,300)

Sales.test = Carseats[-train,"Sales"]

#b
tree.Carseats = tree(Sales~.,data = Carseats,subset = train)
summary(tree.Carseats)
tree.Carseats
plot(tree.Carseats)
text(tree.Carseats,pretty = 0)
pred.flsales = predict(tree.Carseats,newdata = Carseats[-train,])
mean((pred.flsales-Sales.test)^2)
#[1] 4.691532

#c
cv.Carseats = cv.tree(tree.Carseats)
plot(cv.Carseats$size,cv.Carseats$dev,type = "b" )

prune.Carseats = prune.tree(tree.Carseats,best = 3)
plot(prune.Carseats)
text(prune.Carseats,pretty = 0)

pred.prsales = predict(prune.Carseats,newdata=Carseats[-train,])

mean((pred.prsales-Sales.test)^2)
#[1] 5.842053

#d - bagging

#10 vars
library(randomForest)

set.seed(3)
bag.Carseats = randomForest(Sales~.,data = Carseats,subset = train,
              mtry = 10,importance=TRUE)

pred.bgsales = predict(bag.Carseats,newdata = Carseats[-train,])

mean((pred.bgsales-Sales.test)^2)
#[1] 2.277311
importance(bag.Carseats)
varImpPlot(bag.Carseats)

set.seed(4)
rf.Carseats = randomForest(Sales~.,data = Carseats,subset = train,
              mtry=3,importance= TRUE)
pred.rfsales =predict(rf.Carseats,newdata=Carseats[-train,])
mean((pred.rfsales-Sales.test)^2)



# 10 var [1] 2.279273
# 9 var [1] 2.288156
# 8 var [1] 2.327844
# 7 var [1] 2.392463
# 6 var [1] 2.486379
# 5 var [1] 2.52351
# 4 var [1] 2.744703
# 3 var [1] 2.971365


#[1] 2.52351
varImpPlot(rf.Carseats)






































