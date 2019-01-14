# Version 0.1.0

## Test Envirionments

* Local Debian 9.6
  * R 3.5.2
  * R devel (to be 3.6.0) 2019-01-13 r75986

* Ubuntu 14.04
  * R release
  * R devel

* win-builder

## R CMD check results
There were no ERRORs or WARNGINGs.

There was one 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Peter DeWitt <peter.dewitt@ucdenver.edu>’

New submission

## Downstream dependency
There are no downstream dependencies.

## Re-submission Notes
The initial submission was rejected with the following note from CRAN:

> Thanks, we see:
>
> checking re-building of vignette outputs ... [17m] OK
> which is too long for being regularly checked on CRAN.
>
> Can you pls reduce the runtime drastically, e.g. by using less complex
> examples or providing precomputed results?
> Otherwise we will have to disable vignette checks on CRAN.
>
> Is there some reference about the method you can add in the Description
> field in the form Authors (year) doi:.....?
>
> Best,
> Uwe Ligges

The time required to rebuild the vignettes has been drastically reduced by use
of static vignettes using R.rsp as the vignette builder.  Testing on my local
system, R CMD check --as-cran takes less than one minute.

As for the reference for the method, we do not yet have a publication related to
the ensr package.  Our plan was to release the package first with a peer
reviewed publication to follow.  We will update the Description with the
reference upon publication.
