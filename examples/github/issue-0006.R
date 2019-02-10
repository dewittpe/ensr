library(glmnet)
library(ensr)

set.seed(2785)

y = sample(c('Y','N'), 100, replace = T)
yb = as.numeric(y=='Y')

x1 = sample(c(0,1), 100, replace = T)
x2 = sample(c(0,1), 100, replace = T)
x3 = rep(0, 100)
mydata <- data.frame(y = as.factor(y), yb, x1, x2, x3 )
head(mydata)


x <- model.matrix(y ~ x1 + x2 , data = mydata)[,-1]
xb <- model.matrix(y ~ x1 + x2 + x3, data = mydata)[,-1]


### 1: glmnet runs with 2-level factor for family = "binomial, ensr does not
glmmod <- glmnet(x , y=mydata$y, alpha=1, family = "binomial")

ensr_obj <- ensr(x, y=mydata$y, family = "binomial")  

ensr_obj <- ensr(x, mydata$yb, family = "binomial")  



### 2: glmnet runs when a variable of all 0s is provided in X, ensr does not

glmmod <- glmnet(xb , y=mydata$yb, alpha=1, family = "binomial")

ensr_obj <- ensr(xb, y=mydata$yb, family = "binomial")  






