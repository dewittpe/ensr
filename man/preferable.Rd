% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ensr.R
\name{preferable}
\alias{preferable}
\title{Preferable Elastic Net Model}
\usage{
preferable(object, ...)
}
\arguments{
\item{object}{an ensr object}

\item{...}{not currently used.}
}
\value{
a glmnet object associated with the smallest cvm.  If the min cvm is
not unique, then the model with the smallest cvm with largest alpha value is
returned.  If that is not unique, then is all the "preferable" models have
zero non-zero coefficients the model with the largest lambda and largest
alpha value is returned.  Lastly, if a unquie model is still not identified
an error will be thrown.
}
\description{
Find the preferable Elastic Net Model from an ensr object.  The preferable
model is defined as the model with the lowest mean cross validation error and
largest alpha value.
}
