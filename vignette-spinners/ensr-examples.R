#'---
#'title: "Elastic Net SearcheR Examples"
#'subtitle: "ensr package version `r packageVersion('ensr')`"
#'author: "Peter DeWitt"
#'date: "`r Sys.Date()`"
#'output:
#'  rmarkdown::html_document:
#'    toc: true
#'    toc_float: true
#'    number_sections: true
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteIndexEntry{ensr-examples}
#'  %\VignetteEncoding{UTF-8}
#'---

#+ label=setup, include = FALSE
library(knitr)
knitr::opts_chunk$set(collapse = TRUE)
options(qwraps2_markup = "markdown")

#'
# /*
# =============================================================================
# */
#'
#' The primary purpose of the ensr package is to provide methods for searching
#' for preferable values of $\lambda$ and $\alpha$ in elastice net regression.
#' This This vignette starts with a
#' summary of elastic net regression and the use/limitations of the
#' [glmnet](https://cran.r-studio.com/package=glmnet) pacakge.  Examples of data
#' set preperation follow and the vignette concludes with elastic net regression
#' results.
#'
#+label="load_and_attach_ensr"
library(ensr)
library(data.table)
library(microbenchmark)
options(datatable.print.topn  = 3L,
        datatable.print.nrows = 3L)

#'
#' # Elastic Net Regression
#'
#' Elastic Net Regression [@friedman2010regularization] is a penalized linear
#' modeling approach that is a mixture of ridge regression [@hoerl1970ridge],
#' and lasso regression [@tibshirani1996regression].  Ridge regression
#' reducing the impact of co-linearity, and lasso
#' reduces the dimensionality of the support by shrinking some of the regression
#' coefficients to zero.  Elastic net does both of these by solving the
#' following equation for gaussian responses:
#' $$\min_{\beta_0, \beta \in \mathbb{R}^{p+1}} \frac{1}{2N} \sum_{i = 1}^{N}
#' \left( y_i - \beta_0 - x_i^T \beta \right)^2 + \lambda \left[ \left(1 -
#' \alpha \right) \frac{\left \lVert \beta \right \rVert_{2}^{2}}{2} + \alpha
#' \left \lVert \beta \right \rVert_{1} \right],$$
#' where $\lambda \geq 0$ is the complexity parameter and $0 \leq \alpha \leq 1$
#' is the compromise between ridge $\left(\alpha = 0\right)$ and lasso $\left(
#' \alpha = 1 \right).$
#'
#' One advantage for $0 < \alpha < 1,$ compared to lasso is that elastic net
#' will consistently return the same set of non-zero coefficients when some of
#' the predictors are highly correlated.  Lasso, $\alpha = 1,$ has the draw back
#' of returning differing sets of non-zero coefficients when highly correlated
#' predictors are in the model.
#'
#' A benefit of elastic net regression is that the $\beta$ vector can easily be
#' programmed into any other software package capable of matrix, or even simple,
#' arithmetic.  So, while a GBM model might be a better fit of the data than the
#' linear model found via elastic net, GBM models are extremely difficult, or
#' impossible to export to other tools.
#'
#' The `cv.glmnet` call from the glmnet package is used for fitting elastic
#' nets.  The value of $\alpha$ must be defined.  To search for a preferable
#' set of $\lambda$ and $\alpha$ values require fitting several models with
#' different $\alpha$ values and then searching for a preferable model.
#' Read the "Details" section in `help("cv.glmnet")`.
#'
# /*
# =============================================================================
# */
#'
#' # Data Sets
#' This section provides a description for the example data sets in the ensr
#' package and steps needed to prepare the data for analysis.
#'
#' ## Traumatic Brain Injuries
#' A synthetic data set based based on a Traumatic Brain Injury (TBI) study data
#' set has been included with the ensr package.  All data points in this data
#' set have been randomly generated and have no association with any real TBI
#' patient.
data("tbi", package = "ensr")
str(tbi)

#'
#' The columns of this data set are:
#'
#' | Column(s)                 | Meaning                                                                                                 |
#' | :------------------------ | :------------------------------------------------------------------------------------------------------ |
#' | `age`                     | Age of the patient, in days, at time of hospital admission                                              |
#' | `female`                  | An integer flag for sex, 1 == female, 0 == male.                                                        |
#' | `pcode1`, ..., `pcode6`   | Indicator columns for the presence or absence of a procedure code sources from a trauma database.       |
#' | `ncode1`, ..., `ncode6`   | Indicator columns for the presence or absence of the same procedure codes but from a billing data base. |
#' | `injury1`, ..., `injury3` | Indicator columns for the presence or absence of three different types of TBI.                          |
#'
#' No special processing of this data is needed.  It can be used imediately in
#' examples and testing of the ensr package.
#'
#' ## Water Percolation Through a Landfill
#' The file `landfill.csv` contains results from a computer simulation of water
#' percolation through five layers in a landfill over one year.
landfill <-
  fread(file = system.file("extdata/landfill.csv", package = "ensr"), sep = ",")

#'
#' This landfill consists of five layers:
#'
#' 1. Topsoil
#' 2. Geomembrane Liner
#' 3. Clay
#' 4. Weathered Lavery Till (WLT)
#' 5. Unweathered Lavery Till (ULT)
#'
#' The outcomes modeled by the computer program are:
#+label="regex_names", include = FALSE
regex_names <- function(pattern) {
  paste(grep(pattern, names(landfill), value = TRUE), collapse = ", ")
}

#'
#' | Outcome       | Variable Name(s)           |
#' | :--------     | :----------------          |
#' | evaporation   | `r regex_names("^evap")`   |
#' | runoff        | `r regex_names("^runoff")` |
#' | percolation   | `r regex_names("^perc_")`  |
#' | drainage      | `r regex_names("^drain_")` |
#' | water content | `r regex_names("^wc_")`    |
#'
#' The set of predictors included in the data set are:
#'
#' | Predictor                          | Variable Name(s)                    | Notes                                                   |
#' | :--------                          | :----------------                   | :----                                                   |
#' | Porosity                           | `r regex_names("_porosity$")`       |                                                         |
#' | van Genuchten $\alpha$             | `r regex_names("_alpha$")`          | related to the inverse of the air entry suction         |
#' | van Genuchten $\theta_r$           | `r regex_names("_thetar$")`         | residual water content                                  |
#' | van Genuchten N                    | `r regex_names("_n$")`              | pore size                                               |
#' | Saturated hydraulic conductivity   | `r regex_names("_ks$")`             |                                                         |
#' | Relative Humidity                  | `r regex_names("^rh$")`             |                                                         |
#' | Leaf Area Index                    | `r regex_names("^lai$")`            |                                                         |
#' | Grow Start                         | `r regex_names("growstart")`        | First day of the growing season                         |
#' | Grow End                           | `r regex_names("growend")`          | Last day of the growing season                          |
#' | Wind                               | `r regex_names("wind")`             | Average wind speed in mph                               |
#' | Solar Radiation                    | `r regex_names("^rm\\w")`           | Mean of daily solar radiation for wet days, or dry days |
#' | Amplitude of daily solar radiation | `r regex_names("^ar$")`             |                                                         |
#' | SCS AMCII Curve Number             | `r regex_names("cn")`               |                                                         |
#' | Plant root beta                    | `r regex_names("^beta$")`           | Used for calculating evaporation zone depth             |
#' | Linear Defects                     | `r regex_names("^liner_defects$")`  | Number of defects per acre of liner                     |
#' | Linear Pinholes                    | `r regex_names("^liner_pinholes$")` | Number of pinholes per acre of liner                    |
#' | 100 year average Solar Radiation   | `r regex_names("weather_solrad")`   |                                                         |
#' | 100 year average Precipitation     | `r regex_names("weather_precip")`   |                                                         |
#' | 100 year average Temperature       | `r regex_names("weather_temp")`     |                                                         |
#'
#' ## Landfill Data Preparation
#'
#' The elastic net method suggests that the predictors are centered and scaled
#' and in the case of multi-variable responses, that the outcomes are centered
#' and scaled as well.  For calls to `glmnet` and `cv.glmnet` the argument
#' `standardized = TRUE` (default) to center and scale the values.  If you have
#' already centered and scaled the data you may prefer to set `standardized =
#' FALSE`.
#'
#' If you need, or want, to explicitly standardize your data you will likely
#' find use of the base R function `scale` is sufficient for most cases.
#' However, with the landfill data the simple centering and scaling could be
#' considered improper.  `scale`, will use the mean and standard deviation of
#' the input, that is fine if all the values in the input are unique
#' observations.  In the landfill data, however, the manner by which the
#' parameters where inputted into the computer program are repeated.  Centering
#' and scaling should be based on the mean and standard deviation of unique
#' values.  The ensr function `standardize` will standardize a numeric vector
#' based on unique values, by default, via either mean/standard deviation, or
#' median/IQR.
#'
#' Here is an example.  There are `r qwraps2::frmt(nrow(landfill))` rows in the
#' landfill data set.  There are `r length(unique(landfill$topsoil_alpha))`
#' unique values for the van Genuchten $\alpha$ value.  Here are the different
#' scale and centerings:
nrow(landfill)
length(unique(landfill$topsoil_alpha))

# Standard scaling, using the mean and standard deviation for the full vector.
scaled_topsoil_alpha_v1 <- as.vector(scale(landfill$topsoil_alpha))

scaled_topsoil_alpha_v2 <-
  (landfill$topsoil_alpha - mean(landfill$topsoil_alpha)) / sd(landfill$topsoil_alpha)

all.equal(target  = scaled_topsoil_alpha_v1,
          current = scaled_topsoil_alpha_v2,
          check.attributes = FALSE)

# Center and scale using the mean and standard deviation of only the unique
# values of the vector.
scaled_topsoil_alpha_v3 <-
  as.vector(scale(landfill$topsoil_alpha,
                  center = mean(unique(landfill$topsoil_alpha)),
                  scale  = sd(unique(landfill$topsoil_alpha))))

scaled_topsoil_alpha_v4 <- standardize(landfill$topsoil_alpha)

all.equal(target  = scaled_topsoil_alpha_v3,
          current = scaled_topsoil_alpha_v4,
          check.attributes = FALSE)

# Center and scale using the median and inner quartile range (IQR).
scaled_topsoil_alpha_v5 <-
  as.vector(scale(landfill$topsoil_alpha,
                  center = median(unique(landfill$topsoil_alpha)),
                  scale  = IQR(unique(landfill$topsoil_alpha))))

scaled_topsoil_alpha_v6 <-
  standardize(landfill$topsoil_alpha,
              stats = list(center = "median", scale = "IQR"))

all.equal(target  = scaled_topsoil_alpha_v5,
          current = scaled_topsoil_alpha_v6,
          check.attributes = FALSE)

#'
#' For the full landfill data set we can center and scale all the columns as:
scalled_landfill <- standardize(landfill)

#'
#' Note that the values used for centering and scaling are retained as
#' attributes for each variable in the data set.
str( scalled_landfill[, 1:2] )

#'
#' Incase you are interested, here are three ways to get the same result.  The
#' simple `standardize(landfill)` call is the quickest.
microbenchmark(
  e1 = {
    scalled_landfill <- landfill[, lapply(.SD, standardize)]
  },
  e2 = {
    scalled_landfill <- as.data.table(lapply(landfill, standardize))
  },
  e3 = {
    scalled_landfill <- standardize(landfill)
  })

#'
#' Now that we have scalled the data we can start to fit the elastic net.
#'
# /*
# =============================================================================
# */
#'
#' # Searching for $\lambda$ and $\alpha$
#'
#' The `cv.glmnet` uses k-fold cross-validation for glmnet to find a value for
#' $\lambda.$  This requires specifying an $\alpha.$  Find a preferable set of
#' $\alpha$ and $\lambda$ you will need to fit several `cv.glmnet`, one for each
#' value of $\alpha$ that you want to consider.  To illustrate how to do this we
#' will model evaporation as a function of the noted predictors in the landfill
#' data set.
#'
#' Start by building the input matrices for the predictors, `x_matrix`, and the
#' output `y_matrix`.
y_matrix <- as.matrix(scalled_landfill$evap)
x_matrix <- as.matrix(scalled_landfill[, topsoil_porosity:weather_temp])

ensr_obj <- ensr(x_matrix, y_matrix, standardize = FALSE)

summary(object = ensr_obj)
summary(object = ensr_obj)[c(which.min(cve.min), which.min(lambda.1se))]

coef(ensr_obj)
coef(ensr_obj, cve = "min")
coef(ensr_obj, cve = "1se")

coef(preferable(ensr_obj))

preferable(ensr_obj, s = "min")
preferable(ensr_obj, s = "1se")

predict(ensr_obj, newx = x_matrix[1:2, ], cve = "min")
predict(ensr_obj, newx = x_matrix[1:2, ], cve = "1se")
predict(ensr_obj, newx = x_matrix[1:2, ], cve = "no")
predict(ensr_obj, newx = x_matrix[1:2, ], cve = NULL)

#'
#' ## Cross-Validation Issues:
#'
#' This is something we need to look into. We will perform the search for a
#' preferable model with two different foldid vectors, both set up for 10-fold
#' cross validation.
foldid1 <- sample(seq(10), size = nrow(x_matrix), replace = TRUE)
foldid2 <- sample(seq(10), size = nrow(x_matrix), replace = TRUE)

#'
#' We will fit two ensr objects and report the smallest cve.min
ensr_obj_1 <- ensr(x_matrix, y_matrix, standardize = FALSE, foldid = foldid1)
ensr_obj_2 <- ensr(x_matrix, y_matrix, standardize = FALSE, foldid = foldid2)

summary(ensr_obj_1)[, .SD[cve.min == min(cve.min)]]
summary(ensr_obj_2)[, .SD[cve.min == min(cve.min)]]

#'
#' There are small differences in the cross validation errors and the in the
#' $\lambda$ values.  There is a large difference, in the $\alpha$ values.  It
#' is notable, that the differences in the regression coefficients is minor.
#' In this case there are the same number of non-zero coefficients and the
#' magnitudes are similar.
cbind(coef(ensr_obj_1), coef(ensr_obj_2))

#'
#' Consider a different output, percolation through the topsoil.
ensr_obj_1 <- ensr(x_matrix, as.matrix(scalled_landfill$perc_topsoil), standardize = FALSE, foldid = foldid1)
ensr_obj_2 <- ensr(x_matrix, as.matrix(scalled_landfill$perc_topsoil), standardize = FALSE, foldid = foldid2)

summary(ensr_obj_1)[, .SD[cve.min == min(cve.min)]]
summary(ensr_obj_2)[, .SD[cve.min == min(cve.min)]]

#'
#' One could argue there is a major differnce in the result between the two
#' foldid.  Using the cve.min, the foldid1 leads to 19 non-zero coefficients
#' whereas foldid2 leads to 22 non-zero coefficients.
#'

############################## EXPLORING ###########################

# Make a lambda_max function
lambda_max <- function(y, x, alpha) {
  # y <- scale(y)
  # x <- apply(x, 2, scale)
  N <- nrow(x) 
  # if (isTRUE(all.equal(0, alpha))) alpha <- 0.001
  # if (isTRUE(all.equal(1, alpha))) alpha <- 0.999 
  # max(t(x) %*% (y - mean(y) * (1 - mean(y)))) / (alpha * N)
  rbind(
  max( abs( t( (y - mean(y) * (1 - mean(y))) )  %*% x) ) / (alpha * N) 
  max( abs( t( (y ))   %*% x) ) / (alpha * N) 
  )
  # max( abs( t(y)  %*% x) ) / (alpha * N) 
}

lambda_max(y = matrix(tbi$injury1, ncol = 1),
           x = model.matrix( ~ . - injury1 - injury2 - injury3 - 1, data = tbi),
           alpha = c(0.001, .5, 1)) 

YVEC <-  matrix(tbi$injury1, ncol = 1)
XMAT <- model.matrix( ~ . - injury1 - injury2 - injury3 - 1, data = tbi)
f <- lm(YVEC ~  XMAT)

max(abs(t(XMAT) %*% (residuals(f) + predict(f, type = "terms")))) / (0.5 * nrow(XMAT))
max(abs(t(apply(XMAT, 2, scale)) %*% (residuals(f) + predict(f, type = "terms")))) / (0.5 * nrow(XMAT))
g <- glmnet(y = YVEC, x = XMAT, standardize = FALSE, alpha = 0.5)
max(g$lambda)
max(update(g, standardize = TRUE)$lambda)

coef(g)

n <- 500
p <- 3
b <- c(-5,3,2,0)
b <- c(3,2,0)

X <- cbind(rep(1,n),scale(matrix(rnorm(p*n),nrow=n)))
X <- cbind(scale(matrix(rnorm(p*n),nrow=n)))
Y <- rbinom(n,1,prob = exp(X%*%b)/(1 + exp(X%*%b)))

alpha <- .5

max( abs(t(Y - mean(Y)*(1-mean(Y))) %*% X ) )/ ( alpha * n) # largest lambda value
eg_fit <- glmnet(x=X,y=Y,family="poisson",alpha = alpha,standardize=FALSE)
eg_fit$lambda[1] # largest lambda value
lambda_max(Y, X, 0.5)

coef(eg_fit)

f <- lm(Y ~ X)

max(abs(residuals(f) - predict(f, type = "terms"))) / (alpha * n)

sum(X[1, ] *  coef(f)[-1] )
predict(f, type = "terms")[1]

max(abs(t(residuals(f) - predict(f, type = "terms"))) %*% X) / (alpha * n)


############################## EXPLORING ###########################
#'
#' ## Need to set up a grid of lambda and alpha values to work with and use LOOCV
fit0 <- glmnet(x = model.matrix( ~ . - injury1 - injury2 - injury3 - 1, data = tbi),
               y = matrix(tbi$injury1, ncol = 1),
               family = "binomial",
               standardize = FALSE,
               alpha = 0) 
fit1 <- update(fit0, alpha = 1)
fit5 <- update(fit0, alpha = 0.5)
max(fit0$lambda)
max(fit1$lambda)
max(fit5$lambda)

coef(fit0)
coef(fit1)
coef(fit5)

par(mfrow = c(3, 2))
hist(fit0$lambda)
hist(log10(fit0$lambda))
hist(fit1$lambda)
hist(log10(fit1$lambda))
hist(fit5$lambda)
hist(log10(fit5$lambda))

my_lambda <- sort(c(fit0$lambda, fit1$lambda))

str(fit0)

if (interactive()) {
  library(doMC)
  registerDoMC(cores = 9L)
}

cvfits <-
  lapply(seq(0, 1, by = 0.1),
         function(a) {
           cv.glmnet(x = model.matrix( ~ . - injury1 - injury2 - injury3 - 1, data = tbi),
                     y = matrix(tbi$injury1, ncol = 1),
                     alpha = a,
                     lambda = my_lambda,
                     nfolds = nrow(tbi),
                     grouped = FALSE,
                     family = "binomial",
                     parallel = TRUE)
         })


str(cvfits, max.level = 1L)
str(cvfits[[1]], max.level = 1L)
as.list(cvfits[[1]]$glmnet.fit$call)

library(dplyr)

results <-
  lapply(cvfits, function(x) {
           data.frame(lambda = x$lambda, cvm = x$cvm, nzero = x$nzero)
         }) %>%
  dplyr::bind_rows(.id = "alpha") %>%
  dplyr::mutate(alpha = seq(0, 1, by = 0.1)[as.integer(alpha)])

library(ggplot2)
library(ggforce)

ggplot(results) +
  aes(x = alpha, y = log10(lambda), color = cvm) +
  geom_point() +
  geom_point(data = {results %>% dplyr::group_by(alpha) %>% dplyr::filter(lambda == min(lambda))}, color = "red", size = 2)

ggplot(results) +
  theme_bw() +
  aes(x = alpha, y = log10(lambda), z = log10(cvm), color = log10(cvm))  +
  # geom_tile() +
  geom_point() +
  geom_contour() +
  # stat_density_2d(mapping = aes(colour = log10(cvm)))
  scale_colour_gradient(low = "blue", high = "red")

ggplot(results) +
  aes(x = nzero, y = cvm, group = factor(alpha), color = factor(alpha)) + 
  geom_point() + geom_line() +
  facet_zoom(x = nzero > 10, y = cvm < 0.3)

# thought: git the lambda ranges for


alphas <- seq(0, 1, by = 0.1)
cl <- as.list(fit0$call)

glmnets <- lapply(alphas, function(a) {cl$alpha <- a; eval(as.call(cl))})

lambdas <- lapply(glmnets, `[[`, 'lambda')

goo <- function(x) {
  unlist(
  list(
  rep(x[1], times = 2)
  ,
  rep(x[-c(1, length(x))], each = 3)
  ,
  rep(x[length(x)], times = 2)
  ) )
}
goo(lambdas)


foo <- function(x) {
  firsts <- x[-length(x)]
  seconds <- x[-1] 
  out <- c(2/3 *firsts + 1/3 * seconds, 1/3 * firsts + 2/3 * seconds, x)
  out <- sort(out)
  out
}
foo(alphas)

rep(foo(alphas), each = 2)


#'
# /*
# =============================================================================
# */
#'
#' # Session Info
#'
print(sessionInfo(), local = FALSE)

#'
#' # References
#'
# /*
# ------------------------------- End of File ----------------------------------
# */
