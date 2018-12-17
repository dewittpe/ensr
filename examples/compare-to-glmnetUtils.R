#'---
#'title: "ensr vs glmnetUtils"
#'---
#'
#' # Introduction
#'
#' This script provides some comparisons between the ensr and
#' [glmnetUtils](https://cran.r-project.org/package=glmnetUtils)
#' packages.  This script is referenced in a vignette but is not formally part of the
#' ensr package.  If this script was to be part of the ensr package, then the
#' glmnetUtils package would need to be included in the DESCRIPTION file, likely
#' in the Suggests section.  We do not want to include glmnetUtils in the
#' DESCRIPTION file and thus this example lives on the github page only.
#'
#+ lable = "needed_namespaces"
library(glmnetUtils)
library(ensr)
library(data.table)
library(magrittr)
library(ggplot2)

#'
#' For the examples we will use the tbi data from the ensr package.
data(tbi, package = "ensr")
set.seed(42)

#'
#' # Factors in the model specification?
#'
#' ensr requires users to specify a response and support matrix in the same way
#' that glmnet requires.  glmnetUtils allows the user to use a formula to
#' specifiy a model.  While the formula specificiation can be helpful it does
#' allow for a user to specify a model that likely should not be fitted via
#' elastic net.  For example

# this takes awhile to evaluate
# factor_on_rhs <- cva.glmnet(injury1 ~ factor(age), data = tbi, family = "binomial")

#'
#' The `factor_on_rhs` object was fitted, but is not meaningful.  By requiring
#' the end user to explicitly specify response and support matrices, glmnet and
#' ensr would force the end user to carfully consider fitting the above model.
#'
#' # Selecting $\lambda$ and $\alpha$ from a grid, or not?
#'
#' The search for $\alpha$ is different between ensr and glmnetUtils.
#' Effectively glmnetUtils::cva.glmnet is a `lapply`
#' wrapper about `cv.glmnet` of the form: `lapply(X = alpha, FUN =
#' glmnet::cv.glmnet)`.  This is efficient, but it means that each $\alpha$
#' value has a unique set of possible $\lambda$ values with a probability zero
#' occurance of a common $\lambda$ value assessed with two or more $\alpha$
#' values.  ensr builds a grid of $\lambda$ and $\alpha$ values such that each
#' $\alpha$ will be assessed with values of $\lambda$ common to other values of
#' $\alpha.$  ensr supports an estiamte of a ($\alpha$, $\labmda$, z) surface
#' whereas glmnetUtils results will require some imputation to do likewise.
#'
#' Example:

# ensr approach
ymat <- as.matrix(tbi[, injury1])
xmat <- as.matrix(tbi[, pcode1:ncode6])
ensr_object <- ensr(y = ymat, x = xmat, standardize = FALSE, family = "binomial")


# glmnetUtils object
glmnetUtils_object <-
  cva.glmnet(injury1 ~ pcode1 + pcode2 + pcode3 + pcode4 + pcode5 + pcode6 + ncode1 + ncode2 + ncode3 + ncode4 + ncode5 + ncode6,
             data = tbi,
             standardize = FALSE,
             family = "binomial",
             alpha = seq(0, 1, length = 11))

glmnetUtils_lambdas <- lapply(glmnetUtils_object$modlist, `[[`, "lambda")
glmnetUtils_cvm     <- lapply(glmnetUtils_object$modlist, `[[`, "cvm")
glmnetUtils_alphas  <- as.list(seq(0, 1, length = 11))

glmnetUtils_plot_data <-
  Map(data.frame,
      lambda = glmnetUtils_lambdas,
      cvm    = glmnetUtils_cvm,
      alpha  = glmnetUtils_alphas) %>%
  do.call(rbind, .) %>%
  as.data.table

glmnetUtils_plot_data[, z := (cvm - min(cvm)) / sd(cvm)]
  
ggplot(glmnetUtils_plot_data) +
  aes(x = alpha, y = lambda, z = cvm) +
  geom_point() +
  # geom_contour() +  ## Cannot be fitted ...
  scale_y_log10() 

#'
#' There is a red point on the graphs below showing the min cross validation
#' error.
#'
# ensr plot vs glmnetUtils
gridExtra::grid.arrange(
plot(ensr_object) + theme_bw() + ggtitle("ensr") 
,
ggplot(glmnetUtils_plot_data) +
  theme_bw() + 
  aes(x = alpha, y = lambda, z = z, color = log10(z)) +
  geom_point() +
  scale_y_log10() + 
  geom_point(data = glmnetUtils_plot_data[cvm == min(cvm)], size = 2, color = "red") +
  ggtitle("glmnetUtils") +
  ggplot2::scale_color_gradient2(low = "#1b7837", mid = "black", high = "#762183")
,
nrow = 1)



