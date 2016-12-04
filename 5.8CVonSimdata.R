

rm(list = ls())

#a - generating a simulated dataset

set.seed(3)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

# b- scatter plot
plot(x,y)

ran_data = data.frame(x,y)

library(boot)


#c - LOOCV erros
cv.error.4 = rep(0,4)
cv.coef = rep(" ",4)

for (i in 1:4){
glm.fit=glm(y~poly(x,i),data = ran_data)
print(glm.fit$coefficients)
print(summary(glm.fit))
cv.error.4[i] = cv.glm(ran_data,glm.fit)$delta[1]

}
cv.error.4

# seed 1
#[1] 5.890979 1.086596 1.102585 1.114772

#d - results using different seeds
# seed 2
#[1] 6.140266 1.169795 1.191309 1.180095

# seed 3
#[1] 10.038354  1.083959  1.114431  1.171444


# e - power 2 got least errors

#f - statistical significance of coefficients of all models
# variables more than power 2 got no significance, hence it is enough to use
# power 2 model































