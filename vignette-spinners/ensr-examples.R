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
library(glmnet)
library(microbenchmark)
library(ggplot2)
library(magrittr)
options(datatable.print.topn  = 3L,
        datatable.print.nrows = 3L)
set.seed(42)

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
#' ## Other Data
#'
#' ## Other Data Preperation

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

#'
#' It is important to know that `cv.glmnet` can return different results for
#' for the same call, see the following chunck.
fits <- replicate(2,
                  {
                    cv.glmnet(x = x_matrix, y = y_matrix,
                              alpha = 0.5,
                              nfolds = 10L, # default
                              standardize = FALSE, standardize.response = FALSE,
                              family = "gaussian")
                  },
                  simplify = FALSE)

all.equal(fits[[1]], fits[[2]])

#'
#' One way to avoid this is to use leave-one-out cross validation.
fits <- replicate(2,
                  {
                    cv.glmnet(x = x_matrix, y = y_matrix,
                              alpha = 0.5,
                              nfolds = nrow(x_matrix),
                              grouped = FALSE,
                              standardize = FALSE, standardize.response = FALSE,
                              family = "gaussian")
                  },
                  simplify = FALSE)

all.equal(fits[[1]], fits[[2]])

#'
#' Leave-one-out cross-validation is just a special case of defining your own
#' folds
fits <- replicate(2,
                  {
                    cv.glmnet(x = x_matrix, y = y_matrix,
                              alpha = 0.5,
                              foldid = rep(seq(10), length.out = nrow(x_matrix)),
                              standardize = FALSE, standardize.response = FALSE,
                              family = "gaussian")
                  },
                  simplify = FALSE)

all.equal(fits[[1]], fits[[2]])

#'
#' The reason for the differences between the two objects is due to the way that
#' `cv.glmnet` builds the folds when only `nfolds` is provided.  Within the code
#' for `cv.glmnet` (at least as of package version `r packageVersion("glmnet")`
#' is the line `foldid = sample(rep(seq(nfolds), length = N))`.  The `sample`
#' call is the issue.
#'
#' This note is important in the ensr work.  To make a fair comparison between
#' two two different $\alpha$ levels the same folds need to be used in the
#' cross-validation for $\lambda.$

nfolds <- 10
foldid <- rep(seq(nfolds), length.out = nrow(x_matrix))
alphas <- seq(0.05, 0.95, by = 0.10)

folds <- lapply(seq(nfolds), function(idx) list(train = which(foldid != idx), val = which(foldid == idx)))


Map(f = function(fold, alpha) {
      fit <- glmnet(x = x_matrix[fold$train, ],
                    y = y_matrix[fold$train, ],
                    alpha = alpha,
                    standardize.response = FALSE,
                    standardize = FALSE,
                    family = "gaussian")
      pmatrix <- predict(fit, newx = x_matrix[fold$val, ], s = fit$lambda)   # rows: obs, cols = lambda
      cve <- apply(pmatrix, 2, function(x) ((x - y_matrix[fold$val, ])^2)) 
      out <- cbind(matrix(c(cve, fit$lambda), ncol = 2), alpha)
      colnames(out) <- c("cve", "lambda", "alpha")
      out
                  },
      fold = rep(folds, each = length(alphas)),
      alpha = rep(alphas, times = length(folds)))

%>%
do.call(rbind, .) %>%
as.data.table(.) %>%
.[, .SD[alpha == 0.05]] %>% print.default


ggplot(.) +
aes(x = alpha, y = log10(lambda), z = cve, color = cve) +
geom_point()



cv.glmnet(x = x_matrix, y = y_matrix, foldid = foldid, alpha = 0.95, standardize = FALSE, standardize.response = F)$cvm







#'
#' We will want to consider additional cross-validation for $\alpha$ as well.
#' Consider the following, we will use a grid of $\alpha$ values:
alphas <- seq(0.05, 0.95, by = 0.05)

#'
#' Generate a list of `cv.glmnet` objects, one for each alpha.  A quick way to
#' do this is build a list of arguments which will be coerced to a call and
#' evaluated.  We'll run `cv.glmnet` 10 times for each alpha value.
cl <- list(quote(cv.glmnet),
           family = "gaussian",
           nfolds = 10L,
           standardize = FALSE,
           standardize.response = FALSE,
           x = quote(x_matrix),
           y = quote(y_matrix))

fits <- lapply(rep(alphas, 10), function(a) {
                 cl$alpha = a;
                 f <- eval(as.call(cl))
                 f$alpha <- a
                 f
           })

#'
#' Let's extract some of the critical summary statistics from each `cv.glmnet`
#' object, the index of the model with the same $\lambda$ as `lambda.min` or
#' `lambda.1se`, the cross-validation mean error, and the number of non-zero
#' coefficients, `nzero`.
#'
lmin_idx <-
  lapply(fits,
         function(f) {
           which(sapply(f$lambda, function(l) isTRUE(all.equal(target = f$lambda.min, current = l))))
         })

l1se_idx <-
  lapply(fits,
         function(f) {
           which(sapply(f$lambda, function(l) isTRUE(all.equal(target = f$lambda.1se, current = l))))
         })

results <-
  Map(f = function(fit, lmin, l1se) {
           data.table(alpha = fit$alpha,
                      lambda.min = fit$lambda.min,
                      lambda.1se = fit$lambda.1se,
                      cvm.min = fit$cvm[lmin],
                      cvm.1se = fit$cvm[l1se],
                      nzero.min = fit$nzero[lmin],
                      nzero.1se = fit$nzero[l1se])
           },
           fit = fits,
           lmin = lmin_idx, l1se = l1se_idx) %>%
  do.call(rbind, .) %>%
  .[, alpha_rep := rep(1:10, each = length(alphas))]

#'
#' A box plot of the cross-validation error associated with `lambda.min` is
#' shown below.  It should be easy to see that different cross-validation fold
#' ids can, and do, result in a different minimum cross-validation error and
#' alpha selection.  Subsetting the `results` to show the `cvm.min` for each
#' replication illustrates the concern; $\alpha$ values can very greatly
#' depending on the cross-validation indexing.
#+fig.width = 8, fig.height = 4
ggplot(results) +
aes(x = factor(alpha), y = cvm.min) +
geom_boxplot() +
geom_point(mapping = aes(color = factor(alpha_rep))) +
theme(legend.position = "bottom")

results[, .SD[(which.min(cvm.min))], by = .(alpha_rep)] %>%
  print %$%
  table(alpha)

#'
#' To address is issue we have one of two choices, run `cv.glmnet` with the same
#' fold ids for all alpha values and select an alpha from that grouping, or, set
#' up double cross-validation, one for $\lambda$ and one for $\alpha.$

results %>%
  melt(., id.vars = c("alpha", "alpha_rep"), measure.vars = c("cvm.min", "cvm.1se")) %>% 
  ggplot(.) +
  aes(x = value, fill = variable) +
  geom_density(alpha = 0.5)

glmnet(x = x_matrix,
       y = y_matrix,
       family = "gaussian",
       alpha = 0.5,
       nlambda = 1L,
       lambda = 0.02,
       standardize = FALSE,
       standardize.response = FALSE) %>% predict(., newx = x_matrix)


      


#'
#' 1. For each model ($\alpha$) find the "best" value of $\lambda.$  There are
#' two options for this, `lambda.min`, the value of $\lambda$ that givens the
#' minimum mean cross-validated error, and `lambda.1se`, the value gives the most
#' regularized model such that the error is within one standard deviation of the
#' minimum.
lmins <- lapply(fits, `[[`, "lambda.min")
l1ses <- lapply(fits, `[[`, "lambda.1se")


out1se <-
  Map(function(model, index) {
           list(cvm    = model$cvm[index],
                lambda = model$lambda[index],
                lambda_index = index,
                alpha  = as.list(model$glmnet.fit$call)$alpha)
           },
           model = fits,
           index = l1se_idx)

out1se <- do.call(rbind, lapply(out1se, as.data.table))

out1se[cvm == min(cvm)]




#'
#' 2. Identify the index of the model with the $\lambda$ value equal to the
#' value in `lambda_1se`.  Store in the object `lambda_index`.
#'
#' 3. Construct a data.frame with the cross-validation error, $\lambda,$ and
#' $\alpha$ values.  Call this data.frame `cvm_alpha`.
#'
#' 4. Find the $\lambda$ and $\alpha$ values such that the cross validation is
#' the smallest and the $\alpha$ value is the largest.

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
#                             --- End of File ---
# */
