#' Lambda Max
#'
#' Determine the lambda_max value that would be genteraged from a call to
#' \code{\link[glmnet]{glmnet}} without making that call.
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
#'
#' dat <-
#'   expand.grid(standardize = c(TRUE, FALSE),
#'               alpha = alphas)
#'
#' lmax <-
#'   Map(lambda_max,
#'       standardize = dat$standardize,
#'       alpha = dat$alpha,
#'       MoreArgs = list(y = Yvec, x = Xmat))
#'
#'
#' gmax <-
#'   Map(glmnet::glmnet,
#'       standardize = dat$standardize,
#'       alpha = dat$alpha,
#'       MoreArgs = list(y = Yvec, x = Xmat))
#'
#' dat$gmax <- sapply(gmax, function(f) f$lambda[1])
#' dat$lmax <- unlist(lmax)
#'
#' par(mfrow = c(1, 2))
#'
#' with(subset(dat, standardize == TRUE),
#'      {
#'        plot(log10(gmax), log10(lmax))
#'        abline(0, 1)
#'        title(main = "standardize == TRUE")
#'      })
#'
#' with(subset(dat, standardize == FALSE),
#'      {
#'        plot(log10(gmax), log10(lmax))
#'        abline(0, 1)
#'        title(main = "standardize == FALSE")
#'      })
#'
#' @export
lambda_max <- function(y, x, standardize = TRUE, alpha = 0, ...) {

  a0 <- sapply(lapply(alpha, all.equal, 0), isTRUE)
  if (any(a0)) alpha[a0] <- 0.001

  if (standardize) {
    x <- apply(x, 2, scale)
  }

  lmfit <- stats::lm(y ~ x)

  max(abs(t(x) %*% (stats::residuals(lmfit) + stats::predict(lmfit, type = "terms")))) / (alpha * nrow(x))

}

#' Lambda Alpha Grid
#'
#' Construct a data frame with values for lambda and alpha with an indicator to
#' know if the model is worth fitting.
#'
#' @param lambdas a vector of max values for each alpha given
#' @param alphas  a vector a alpha values corresponding to the max lambdas
#'
#' @examples
#'
#' data(tbi)
#' Xmat <- model.matrix( ~ . - injury1 - injury2 - injury3 - 1, data = tbi)
#' Yvec <- matrix(tbi$injury1, ncol = 1)
#' alphas <- seq(0, 1, length = 20)
#'
#' lga <- lambda_alpha_grid(alphas = alphas, lambdas = lambda_max(Yvec, Xmat, alpha = alphas))
#'
#' ggplot2::ggplot() +
#'   ggplot2::theme_bw() +
#'   ggplot2::aes_string(x = "a", y = "log10(l)") +
#'   ggplot2::geom_path(data = lga$top) +
#'   ggplot2::geom_point(data = lga$lgrid,
#'                       mapping = ggplot2::aes(color = cos(a) + sin(log10(l)))) +
#'   ggplot2::geom_contour(data = lga$lgrid,
#'                         mapping = ggplot2::aes(z = cos(a) + sin(log10(l)))) +
#'   ggplot2::scale_color_gradient2(low = "blue", high = "red", mid = "grey")
#'
#' @export
lambda_alpha_grid <- function(lambdas, alphas) {

  all_alphas <- sort(c(alphas, alphas[-length(alphas)] + diff(alphas) / 2))
  all_lmaxs  <- rep(lambdas, each = 2)

  top <- data.table::data.table(l = all_lmaxs[-length(all_lmaxs)], a = all_alphas)

  lgrids <-
    Map(function(a, lmax, lmin) { data.table::data.table(a = a, l = 10^seq(log10(lmin), log10(lmax), length = 50)) },
        a = top$a,
        lmax = top$l,
        MoreArgs = list(lmin = 0.001 * min(all_lmaxs)))

  alll <- unique(unlist(lapply(lgrids, `[[`, "l")))

  lgrid <-
    Map(function(a, l) { data.table::data.table(a = a, l = alll[which(alll <= l & alll >= 0.001 * l)]) }, a = top$a, l = top$l)
  lgrid <- data.table::rbindlist(lgrid)

  list(top = top, lgrid = lgrid)
}

