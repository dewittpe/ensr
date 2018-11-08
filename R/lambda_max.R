#' Lambda Max
#'
#' Determine the lambda_max value that would be genteraged from a call to
#' \code{\link[glmnet]{glment}} without making that call.
#'
#' @param y the response vector
#' @param x the predictor matrix
#' @param alpha the glmnet alpha value
#' @param standardize logicial, should the x matrix be standardized?
#' @param ... other args
#'
#' @examples
#'
#' data(tbi)
#' Xmat <- model.matrix( ~ . - injury1 - injury2 - injury3 - 1, data = tbi)
#' Yvec <- matrix(tbi$injury1, ncol = 1)
#'
#' alphas <- seq(0, 1, length = 20)
#' lambda_max(Yvec, Xmat, alpha = alphas)
#'
#' # Look at different options for standardizing the inputs.

dat <-
  expand.grid(standardize = c(TRUE, FALSE), 
              alpha = alphas)

lmax <-
  Map(lambda_max,
      standardize = dat$standardize,
      alpha = dat$alpha,
      MoreArgs = list(y = Yvec, x = Xmat))


gmax <- 
  Map(glmnet::glmnet,
      standardize = dat$standardize,
      alpha = dat$alpha,
      MoreArgs = list(y = Yvec, x = Xmat))

dat$gmax <- sapply(gmax, function(f) f$lambda[1])
dat$lmax <- unlist(lmax)


par(mfrow = c(1, 2))

with(subset(dat, standardize == TRUE),
     {
       plot(log10(gmax), log10(lmax))
       abline(0, 1)
       title(main = "standardize == TRUE")
     })

with(subset(dat, standardize == FALSE),
     {
       plot(log10(gmax), log10(lmax))
       abline(0, 1)
       title(main = "standardize == FALSE")
     })

#'
#' @export
lambda_max <- function(y, x, standardize = TRUE, standardize.response = FALSE, alpha = 0, ...) {

  a0 <- sapply(lapply(alpha, all.equal, 0), isTRUE)
  if (any(a0)) alpha[a0] <- 0.001

  if (standardize) {
    x <- apply(x, 2, scale)
  }
  if (standardize.response) {
    y <- scale(y)
  }

  lmfit <- stats::lm(y ~ x)

  max(abs(t(x) %*% (residuals(lmfit) + predict(lmfit, type = "terms")))) / (alpha * nrow(x))
  
}
