#' Elastic Net SearcheR
#'
#' Search for the values of alpha and lambda.
#'
#' @inheritParams glmnet::glmnet
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

#' @export
coef.ensr <- function(object, model_idx, s = "min", exact = FALSE, ...) {
  if (missing(model_idx) && is.character(s)) { 
    sm <- summary(object)
    sm <- sm[which.min(sm[[paste0('cve.', s)]]), ]
    model_idx <- sm$model_idx
    s <- sm[[paste0("lambda.", s)]]
  } 
  coef(object[[model_idx]], s = s, exact = FALSE, ...)
}



