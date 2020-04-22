## Test environments
* local OS X install, R 3.6.2
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results

No ERRORs, WARNINGs, NOTEs on R-release.

1 ERROR, 1 WARNING on R-devel.

The error and warning are caused by an upstream dependency (itsadug), which fails in R-devel.
The maintainer of itsadug is aware of the issues and working on it.

This submission of tidymv is in prospect of the release of the next version of dplyr, an upstream dependency of tidymv, which generates an error in tidymv::create_start_event().
This submission fixes that error.
dplyr is due to be released on May 1st, while the next R release will be later than that.
So, even if tidymv fails on R-devel, I am submitting it in prospect of the dplyr release which is earlier than the R-devel release.

### ERROR

* checking examples ... ERROR
Running examples in ‘tidymv-Ex.R’ failed
The error most likely occurred in:

    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    > ### Name: plot_difference
    > ### Title: Plot difference smooth from a GAM.
    > ### Aliases: plot_difference
    > 
    > ### ** Examples
    > 
    > library(mgcv)
    Loading required package: nlme
    This is mgcv 1.8-31. For overview type 'help("mgcv-package")'.
    > set.seed(10)
    > data <- gamSim(4)
    Factor `by' variable example
    > model <- gam(y ~ fac + s(x2) + s(x2, by = fac) + s(x0), data = data)
    > 
    > plot_difference(model, x2, list(fac = c("1", "2")))
    Error in table(predictors$Terms) - table(predictors[predictors$Label %in%  : 
      non-conformable arrays
    Calls: plot_difference -> %>% -> eval -> eval
    Execution halted

### WARNING

    * checking re-building of vignette outputs ... WARNING
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘plot-smooths.Rmd’ using rmarkdown
    Quitting from lines 172-177 (plot-smooths.Rmd) 
    Error: processing vignette 'plot-smooths.Rmd' failed with diagnostics:
    non-conformable arrays
    --- failed re-building ‘plot-smooths.Rmd’
    
    --- re-building ‘predict-gam.Rmd’ using rmarkdown
    --- finished re-building ‘predict-gam.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘plot-smooths.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted


## Downstream dependencies

There are currently no downstream dependencies for this package.
