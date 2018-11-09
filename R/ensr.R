#' Elastic Net SearcheR
#'
#' Search for the values of alpha and lambda.
#'
#' @inheritParams glmnet::cv.glmnet
#' @param alphas a sequence alpha values
#'
#' @export
ensr <- function(y, x, alphas = seq(0.00, 1.00, length = 10), nfolds = 10L, foldid, ...) {

  # build a single set of folds
  if (missing(foldid)) {
    foldid <- rep(seq(nfolds), length.out = nrow(x))
  }

  #   glmnet_args <- formals(glmnet::glmnet)
  #   names(glmnet_args) 

  cl <- as.list(match.call()) 
  cl[[1]] <- quote(glmnet::cv.glmnet)
  cl$alphas <- NULL

  # for(a in setdiff(names(glmnet_args), names(cl))) {
  #   cl[[a]] <- glmnet_args[[a]]
  # }

  lmax <- lambda_max(y, x, alphas, standardize = cl$standardize)
  lgrid <- lambda_alpha_grid(lmax, alphas)

  l_and_a <- split(lgrid$lgrid, lgrid$lgrid$a)

  models <- lapply(l_and_a,
                   function(la) {
                     cl$alpha <- la$a[1]
                     cl$lambda <- la$l
                     eval(as.call(cl))
                   })

  names(models) <- NULL

  class(models) <- "ensr"
  models
}
x <- Xmat <- model.matrix( ~ . - injury1 - injury2 - injury3 - 1, data = tbi)
y <- Yvec <- matrix(tbi$injury1, ncol = 1)

out <- ensr(Yvec, Xmat, standardize = TRUE) 
str(out, max.level = 1)


#' @export
summary.ensr <- function(object, ...) {

  data.table::rbindlist(
  lapply(object, function(obj) {
           data.table::data.table(lambda = obj$lambda,
                cvm    = obj$cvm,
                # lambda.min = obj$lambda.min,
                nzero = obj$nzero,
                alpha = as.list(obj$glmnet.fit$call)$alpha)
                   })
  )

}
summary(out) 

sout <- summary(out)
sout[, z := standardize(cvm, stats = list(center = "median", scale = "IQR"))]
sout[, z := standardize(cvm, stats = list(center = "mean", scale = "sd"))]
sout[, z := standardize(cvm, stats = list(center = "min", scale = "sd"))]

library(ggplot2)
ggplot(sout) +
  aes(x = alpha, y = lambda, z = log(z), color = log(z)) + 
  # aes(x = alpha, y = lambda, z = nzero, color = factor(nzero)) + 
  geom_point() +
  geom_contour() +
  scale_y_log10() +
  # geom_text(mapping = aes(label = nzero, color = nzero)) +
  geom_point(data = sout[cvm == min(cvm), ], color = "red") +
  scale_color_gradient2()

standardize(summary(out)$cvm, stats = list(center = "median", scale = "IQR")) 


#' Predict Methods for ensr objects
#'
#' Using either the \code{lambda.min} or \code{lambda.1se}, find the preferable
#' model from the \code{ensr} object and return a prediction.
#'
#' The \code{glmnet::predict} argument \code{s} is ignored if specified and
#' attempted to be passed via \code{...}.  The value of \code{s} that is passed
#' to \code{glmnet::predict} is determined by the value of \code{lambda.min} or
#' \code{lambda.1se} found from a call to \code{\link{preferable}}.
#'
#' @inheritParams glmnet::predict.elnet
#' @param object a \code{ensr} object
#' @param cve a character string with the value "min" or "1se".  The default is "min".
#' @param ... other arguments passed along to \code{predict}
#' @name predict
#' @export
predict.ensr <- function(object, newx, cve = NULL, type=c("link","response","coefficients","nonzero","class"), exact = FALSE, newoffset, ...) {

  cl <- as.list(match.call())
  cl[[1]] <- quote(predict)

  if (is.null(cve)) {
    cve <- "min"
  }

  if (!(cve %in% c("min", "1se"))) {
    warning(sprintf("%s is not valid for cve in predict.ensr, setting to 'min'.", cve))
    cve <- "min"
  }

  po <- preferable(object, cve)
  cl$object <- po

  cl$s <- po[[paste0("lambda.", cve)]]
  eval(as.call(cl))
}

#' @rdname predict
#' @export
coef.ensr <- function(object, cve = NULL, exact = FALSE, ...) {
  cl <- as.list(match.call())
  cl[[1]] <- quote(predict)
  cl$type = "coefficients"
  eval(as.call(cl))
}

#' Preferable Elastic Net Model
#'
#' Find the preferable Elastic Net Model from an ensr object.  The preferable
#' model is defined as the model with the lowest mean cross validation error and
#' largest alpha value.
#'
#' @param object an ensr object
#' @param cve a character string, by default using \code{"min"} will use the
#' \code{lambda.min} for selecting a preferable model.  \code{"1se"} will use
#' \code{lambda.1se}.
#' @param ... not currently used.
#' @export
preferable <- function(object, cve = "min", ...) {
  UseMethod("preferable")
}

#' @export
preferable.ensr <- function(object, cve = "min", ...) {

  ce <- paste0("cve.", cve)
  le <- paste0("lambda.", cve)

  sm <- summary(object)
  sm <- sm[sm[[ce]] == min(sm[[ce]]), ]
  if (nrow(sm) > 1L) {
    sm <- sm[sm[['alpha']] == max(sm[['alpha']])]
  }
  model_idx <- sm$model_idx

  out <- object[[model_idx]] 
  out$alpha <- sm$alpha
  out$lambda.min_idx <- which(out$lambda == out$lambda.min)
  out$lambda.1se_idx <- which(out$lambda == out$lambda.1se)
  class(out) <- c("ensr_pref", class(out))
  out
}

