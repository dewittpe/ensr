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

There is a `makefile` at the package root which controls the build process.  If
you are working in RStudio, the `ensr.Rproj` file will set up the project to use
the makefile.  There are additional makefiles in the `data-raw` and the
`vignette-spinners` directories.

The `vignette-spinners` directory contains .R and .Rmd files.  The .R file
can be edited.  The .Rmd files **should not** be edited manually.  The .R files
are spun to .Rmd via [`knitr::spin`](https://yihui.name/knitr/demo/stitch/).

There example data sets are documented in `vignette-spinners/ensr-datasets.R`
and a symbolic link in the `data-raw` directory.  This allows the .R script to
be sources within the `data-raw` directory to build the `.rda` files in the
`data` directory.  Spinning the `vignette-spinners/ensr-datasets.R` to
`vignette-spinners/ensr-datasets.Rmd` and having a symbolic link to the .Rmd
within the `vignettes` directory allows us to have one file, the .R file, to
generate the example data sets and a vignette to explain the example data sets
to the end users.

The `vignette-spinners/ensr-example.R` script builds the examples vignette.

