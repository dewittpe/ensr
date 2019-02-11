library(glmnet)
library(ensr)

set.seed(2785)

y  <- sample(c('Y','N'), 100, replace = TRUE)
yb <- as.numeric(y=='Y')

x1 <- sample(c(0,1), 100, replace = TRUE)
x2 <- sample(c(0,1), 100, replace = TRUE)
x3 <- rep(0, 100)

mydata <- data.frame(y = as.factor(y), yb, x1, x2, x3 )
str(mydata)

x  <- model.matrix(y ~ x1 + x2 - 1,      data = mydata)
xb <- model.matrix(y ~ x1 + x2 + x3 - 1, data = mydata)


### 1: glmnet runs with 2-level factor for family = "binomial, ensr does not
glmmod <- glmnet(x = x, y = mydata$y, alpha = 1, family = "binomial")

ensr_obj <- ensr(x = x, y = mydata$y,  family = "binomial")
# Error in seq.default(log10(lmin), log10(lmax), length = nlambda) :
#   'from' must be a finite number
# In addition: Warning messages:
# 1: In model.response(mf, "numeric") :
#   using type = "numeric" with a factor response will be ignored
# 2: In Ops.factor(y, z$residuals) : ‘-’ not meaningful for factors
# 3: In Ops.factor(stats::residuals(lmfit), stats::predict(lmfit, type = "terms")) :
#   ‘+’ not meaningful for factors

ensr_obj <- ensr(x = x, y = mydata$yb, family = "binomial")
# There were 50 or more warnings (use warnings() to see the first 50)

summary(warnings())
# 50 identical warnings:
# In regularize.values(x, y, ties, missing(ties)) : collapsing to unique 'x' values



### 2: glmnet runs when a variable of all 0s is provided in X, ensr does not

glmmod <- glmnet(xb , y=mydata$yb, alpha=1, family = "binomial")

ensr_obj <- ensr(xb, y=mydata$yb, family = "binomial")
# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
#   0 (non-NA) cases






