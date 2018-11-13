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

  cl <- as.list(match.call())
  cl[[1]] <- quote(glmnet::cv.glmnet)
  cl$alphas <- NULL

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

#' @export
summary.ensr <- function(object, ...) { 
  out <-
    data.table::rbindlist(
      lapply(seq_along(object),
             function(idx) {
               data.table::data.table(l_index = idx,
                                      lambda = object[[idx]]$lambda,
                                      cvm    = object[[idx]]$cvm,
                                      nzero  = object[[idx]]$nzero,
                                      alpha  = as.list(object[[idx]]$glmnet.fit$call)$alpha)
             })
    )
  class(out) <- c("ensr_summary", class(out))
  out
}

#' @export
print.ensr_summary <- function(x, ...) {
  NextMethod("print")
}

#' @export
plot.ensr_summary <- function(x, ...) {
  sout <- data.table::copy(x)
  sout[, z := standardize(cvm, stats = list(center = "min", scale = "sd"))]
  ggplot2::ggplot(sout) +
  ggplot2::aes_string(x = "alpha", y = "lambda", z = "log(z)", color = "log(z)") +
  ggplot2::geom_point() +
  ggplot2::geom_contour() +
  ggplot2::scale_y_log10() +
  ggplot2::geom_point(data = sout[cvm == min(cvm), ], color = "red") +
  ggplot2::scale_color_gradient2() 
} 

#' @export
plot.ensr <- function(x, ...) {
  plot(summary(x))
}

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
#' @param ... other arguments passed along to \code{predict}
#' @name predict
#' @export
predict.ensr <- function(object, ...) {
 
  pm <- preferable(object)

  cl <- as.list(match.call())
  cl[[1]] <- quote(predict)

  cl$object <- pm

  cl$s <- pm$cv_row$lambda
  eval(as.call(cl))
}

#' @rdname predict
#' @export
coef.ensr <- function(object, ...) {
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
#' @param ... not currently used.
#' @export
preferable <- function(object, cve = "min", ...) {
  UseMethod("preferable")
}

#' @export
preferable.ensr <- function(object, ...) {

  sm <- summary(object)
  sm <- sm[sm[["cvm"]] == min(sm[["cvm"]]), ]

  if (nrow(sm) > 1L) {
    sm <- sm[sm[['alpha']] == max(sm[['alpha']])]
  }

  model_idx <- sm$l_index

  out <- object[[model_idx]]$glmnet.fit
  out$cv_row <- sm
  attr(out$cv_row, "call") <- match.call()
  class(out) <- c("ensr_pref", class(out))
  out
}

