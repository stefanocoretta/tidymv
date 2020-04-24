# `tidymv`: Tidy Model Visualisation for Generalised Additive Models

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/stefanocoretta/tidymv.svg?branch=master)](https://travis-ci.org/stefanocoretta/tidymv)
<!-- badges: end -->

This is the repository of the `R` package `tidymv`. This package provides functions for the visualisation of GAM(M)s and the generation of model-based predicted values using tidy tools from the `tidyverse`.

## Installation

The package is on CRAN, so you can install it from there.

**NOTE**: v3.0.0 brings breaking changes.
The function `plot_difference()` has been removed due to the archiving of one dependecy.

To install the package from GitHub, use `devtools::install_github("stefanocoretta/tidymv@v3.0.0", build_opts = c("--no-resave-data", "--no-manual"))`. To learn how to use the package, check out the vignettes (for example, `vignette("predict-gam", package = "tidymv")`).

If you wish to install the development version, use `devtools::install_github("stefanocoretta/tidymv", build_opts = c("--no-resave-data", "--no-manual"))`.
