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

ensr_obj <- ensr(x_matrix, y_matrix, standardize = FALSE)

summary(object = ensr_obj) 
summary(object = ensr_obj)[, .SD[c(which.min(cve.min), which.min(lambda.1se))]]

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
