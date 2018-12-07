ensr: Elastic Net Searcher <img src="hexsticker/ensr.png" width=173 height=200 align="right"/>
===============================================================================

The cv.glmnet function in [`glmnet`](https://cran.r-project.org/packag=glmnet)
will perform cross validation to find the value of lambda.  A grid search for
alpha, the mixture between lasso and ridge regression, is not done in the
`glmnet` package.  The `ensr` provides this functionality: grid search for alpha
while using cv for lambda, given a value of alpha.
