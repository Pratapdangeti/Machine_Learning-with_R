




#install.packages('/home/pratapdangeti/Downloads/rhdfs_1.0.8.tar.gz', repos = NULL, type="source")

#install.packages('/home/pratapdangeti/Downloads/rhbase_1.2.1.tar.gz', repos = NULL, type="source")

#install.packages('/home/pratapdangeti/Downloads/rmr2_3.3.1.tar.gz', repos = NULL, type="source")



rm(list = ls())

#8.3.1 - Fitting classification trees

library(tree)
library(ISLR)
fix(Carseats)
attach(Carseats)
High = ifelse(Sales<=8,"No","Yes")
dim(Carseats)
#[1] 400  11
names(Carseats)
#[1] "Sales"       "CompPrice"   "Income"      "Advertising"
#[5] "Population"  "Price"       "ShelveLoc"   "Age"        
#[9] "Education"   "Urban"       "US"
str(Carseats)

Carseats=data.frame(Carseats,High)

tree.carseats = tree(High~.-Sales,Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty = 0)

tree.carseats


# Splitting test and train datasets

set.seed(2)
train = sample(1:nrow(Carseats),200)
Carseats.test = Carseats[-train,]
High.test = High[-train]


tree.carseats = tree(High~.-Sales,data = Carseats,subset = train)
tree.pred = predict(tree.carseats,Carseats.test,type = "class")
table(tree.pred,High.test)

#High.test
#tree.pred  No Yes
#      No  102  17
#     Yes  30  51
# error% = 23.5%

set.seed(3)
cv.carseats = cv.tree(tree.carseats,FUN = prune.misclass)
names(cv.carseats)
cv.carseats


par(mfrow=c(1,1))
plot(cv.carseats$size,cv.carseats$dev,type = "b")
plot(cv.carseats$k,cv.carseats$dev,type = "b")



prune.carseats = prune.misclass(tree.carseats,best = 12)
plot(prune.carseats)
text(prune.carseats,pretty = 0)



tree.pred=predict(prune.carseats,Carseats.test,type = "class")
table(tree.pred,High.test)

#High.test
#tree.pred No Yes
#No  99  27
#Yes 33  41
#%error = 30 %





























































