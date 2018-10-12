#' Standardize
#'
#' Center and scale vectors by mean/standard deviation or median/IQR with the
#' option to determine the statistics based on unique observations.
#'
#' @param x numeric data to standardize
#' @param stats a list defining the centering and scaling statistics.
#' @param use_unique use only unique values of \code{x} when determining the
#' values for the \code{stats}.
#' @param margin passed to apply if \code{x} is a matrix or array.  If you want
#' to use all the data in the array for the calculation of the statistics pass
#' \code{margin = 0}.
#'
#' @examples
#' x <- 1:100
#' standardize(x)
#' standardize(x, stats = list(center = "median", scale = "IQR"))
#'
#' xmat <- matrix(1:50, nrow = 10)
#' standardize(xmat, margin = 0)
#' standardize(xmat, margin = 1)
#' standardize(xmat, margin = 2)
#'
#' xarray <- array(1:60, dim = c(5, 2, 6))
#' standardize(xarray, margin = 0)
#' standardize(xarray, margin = 1:2)
#'
#' # Standardise a data.frame
#' standardize(mtcars)
#'
#' # a generic list object
#' alist <- list(x = rep(1:10, 2), y = rnorm(100), z = matrix(1:10, nrow = 2))
#' standardize(alist, margin = 0)
#' standardize(alist, margin = 1)
#' @export
standardize <- function(x, stats = list(center = "mean", scale = "sd"), use_unique = TRUE, margin) {
  UseMethod("standardize")
}

#' @export
standardize.numeric <- function(x, stats = list(center = "mean", scale = "sd"), use_unique = TRUE, margin) {
  if (use_unique) {
    u <- unique(x)
    cnt <- match.fun(stats$center)(u)
    scl <- match.fun(stats$scale)(u)
  } else {
    cnt <- match.fun(stats$center)(x)
    scl <- match.fun(stats$scale)(x)
  }

  out <- (x - cnt) / scl
  attr(out, "center") <- cnt
  attr(out, "scale")  <- scl
  out
}

#' @export
standardize.matrix <- function(x, stats = list(center = "mean", scale = "sd"), use_unique = TRUE, margin) {
  cl <- as.list(match.call())[-1]
  do.call(standardize.array, cl)
}

#' @export
standardize.array <- function(x, stats = list(center = "mean", scale = "sd"), use_unique = TRUE, margin) {
  if (isTRUE(all.equal(margin, 0))) {
    cl <- as.list(match.call())[-1]
    out <- do.call(standardize.numeric, cl)
  } else {
    out <- apply(x, MARGIN = margin, FUN = standardize, stats = stats, use_unique = use_unique)
    out <- array(out, dim = dim(x))
  }
  out
}

#' @export
standardize.list <- function(x, stats = list(center = "mean", scale = "sd"), use_unique = TRUE, margin) {
  cl <- as.list(match.call())[-1]
  lapply(x, function(xx) {
           args <- cl
           args$x <- xx
           do.call(standardize, args)
})

}

#' @export
standardize.data.frame <- function(x, stats = list(center = "mean", scale = "sd"), use_unique = TRUE, margin) {
  out <- lapply(x, standardize, stats = stats, use_unique = use_unique, margin = margin)
  f <- match.fun(paste0("as.", class(x)[1]))
  f(out)
}
