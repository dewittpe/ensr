# ensr: Elastic Net SearcheR <img src="man/figures/ensr.png" width=173 height=200 align="right"/>

[![Build Status](https://travis-ci.com/dewittpe/ensr.svg?branch=master)](https://travis-ci.com/dewittpe/ensr)

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ensr)](https://cran.r-project.org/package=ensr)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/ensr)](http://www.r-pkg.org/pkg/ensr)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/ensr)](http://www.r-pkg.org/pkg/ensr)

Elastic Net regression models combine both the L1 and L2 penalties of lasso and
ridge regression.  There are two penalty terms, lambda and alpha.  Lambda is a
complexity parameter and alpha is a balance between lasso and ridge.

The `cv.glmnet` function in [`glmnet`](https://cran.r-project.org/package=glmnet)
will perform cross validation to find the value of lambda for a given value of
alpha.  `cv.glmnet` does not search over values of alpha.  The ensr package
builds a grid of alpha and lambda values and, using cross-validation, suggests
preferable values for both lambda and alpha.

After installing this package we encourage you to read the vignette to see
examples.

```r
vignette("ensr-examples", package = "ensr")
```

## Installing ensr

ensr is on [CRAN](https://cran.r-project.org/package=ensr).

```r
install.packages('ensr', repos = 'https://cran.rstudio.com')
```

Install the development version from github:

```r
if ('remotes' %in% rownames(installed.packages())) {
  install.packages('remotes', repos = "https://cran.rstudio.com")
}
remotes::install_github("dewittpe/ensr", build_opts = c("--no-resave-data"))
```

## Development version from Github

There are several ways you can install ensr.  If you are working on a Windows
machine you will need to have [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed.


### Clone and Install
There are detailed instructions for cloning the repo in the `CONTRIBUTING.md`
file.  Windows users need to read the details for cloning the repo so that
symbolic links will be handled correctly.  After cloning use the makefile to
build, check, and install the ensr package, e.g.,

    make install

## Contributing
Please read the `CONTRIBUTING.md` file.  There are details on the how to clone
the repo and the structure of this package.

