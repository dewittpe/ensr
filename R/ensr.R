#' Elastic Net SearcheR
#'
#' Search for the values of alpha and lambda.
#'
#' @inheritParams glmnet::cv.glmnet
#' @param alphas a sequence alpha values
#'
#' @export
ensr <- function(x, y, alphas = seq(0.05, 0.95, by = 0.05), nfolds = 10L, foldid, ...) {

  # build a single set of folds
  if (missing(foldid)) {
    foldid <- rep(seq(nfolds), length.out = nrow(x))
  }

  cl <- as.list(match.call())
  cl[[1]] <- quote(glmnet::cv.glmnet)

  models <- lapply(alphas,
                   function(a) {
                     cl$alpha = a
                     eval(as.call(cl))
                   })
  class(models) <- "ensr"
  models
}


#' @export
summary.ensr <- function(object, ...) {
  # find the index for lambda.min and lambda.1se
  lambda_idxs <-
    lapply(object,
           function(m) {
             c(lmin = which(sapply(m$lambda, function(l) isTRUE(all.equal(target = m$lambda.min, l)))),
               l1se = which(sapply(m$lambda, function(l) isTRUE(all.equal(target = m$lambda.1se, l)))))
           })

  data.table::rbindlist(
                        Map(function(model, index) {
                              data.table::data.table(lambda.min.idx = index["lmin"],
                                                     lambda.1se.idx = index["l1se"],
                                                     cve.min    = model$cvm[index["lmin"]],
                                                     lambda.min = model$lambda[index["lmin"]],
                                                     nzero.min  = unname(model$nzero[index["lmin"]]),
                                                     cve.1se    = model$cvm[index["l1se"]],
                                                     lambda.1se = model$lambda[index["l1se"]],
                                                     nzero.1se  = unname(model$nzero[index["l1se"]]),
                                                     alpha  = as.list(model$glmnet.fit$call)$alpha)
                            },
                            model = object,
                            index = lambda_idxs),
                        idcol = "model_idx"
  )
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
  # s <- sm[[le]]

  out <- object[[model_idx]]
  out$alpha <- sm$alpha
  out
}
