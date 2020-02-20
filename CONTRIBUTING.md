# Contributing to the ensr package

## Style Guide

For the most part we follow the [Style Guide](http://adv-r.had.co.nz/Style.html)
by Hadley Wickham.  Please just try to be consistent with the style you see.

## Cloning the Repo

The package structure relies on symbolic links.  If you are working on Linux of
Mac the you should have not problem cloning and working with this repo.  If,
however, you are working on a Windows machine there are a few additional steps
you will need to take.

1. Windows Vista or newer with NTFS file system, not FAT.
2. You need administrator rights or at least `SeCreateSymbolicLinkPrivilege`
   privilege
3. git bash version 2.10.2 or later.  It will be helpful to install with
   `core.symlinks` option turned on.

In the git bash shell clone the repo.  (The example below uses SSH, change the
URL as needed for https.)

    git clone -c core.symlinks=true git@github.com:dewittpe/ensr.git

## Package Structure

There is a `Makefile` at the package root which controls the build process.  If
you are working in RStudio, the `ensr.Rproj` file will set up the project to use
the Makefile.

### Vignettes
The `vignette-spinners` directory contains .R and .Rmd files.  The .R file
can be edited.  The .Rmd files **should not** be edited manually.  The .R files
are spun to .Rmd via [`knitr::spin`](https://yihui.name/knitr/demo/stitch/).

The time required to fit the models in the examples vignette will surpass the
allowed time for vignette building on CRAN.  As such, the vignette .Rmd files
are rendered to .html and the
[R.rsp::asis](https://cran.r-project.org/package=R.rsp) vignette engine will be
used to allow for static vignettes in the package itself.

#### Data Sets
The example data sets are documented in `vignette-spinners/ensr-datasets.R`.
This allows the .R script to be sourced to build the `.rda` files in the `data`
directory.

#### ensr Examples
The `vignette-spinners/ensr-example.R` script builds the examples vignette.

