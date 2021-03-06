% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize.R
\name{standardize}
\alias{standardize}
\title{Standardize}
\usage{
standardize(
  x,
  stats = list(center = "mean", scale = "sd"),
  use_unique = TRUE,
  margin
)
}
\arguments{
\item{x}{numeric data to standardize}

\item{stats}{a list defining the centering and scaling statistics.}

\item{use_unique}{use only unique values of \code{x} when determining the
values for the \code{stats}.}

\item{margin}{passed to apply if \code{x} is a matrix or array.  If you want
to use all the data in the array for the calculation of the statistics pass
\code{margin = 0}.}
}
\description{
Center and scale vectors by mean/standard deviation or median/IQR with the
option to base the standardization only on unique observations.
}
\examples{
x <- 1:100
standardize(x)
standardize(x, stats = list(center = "median", scale = "IQR"))

xmat <- matrix(1:50, nrow = 10)
standardize(xmat, margin = 0)
standardize(xmat, margin = 1)
standardize(xmat, margin = 2)

xarray <- array(1:60, dim = c(5, 2, 6))
standardize(xarray, margin = 0)
standardize(xarray, margin = 1:2)

# Standardize a data.frame
standardize(mtcars)

# a generic list object
alist <- list(x = rep(1:10, 2), y = rnorm(100), z = matrix(1:10, nrow = 2))
standardize(alist, margin = 0)
standardize(alist, margin = 1)
}
