# tidymv v3.3.2

## Developer

* Use Roxygen 7.2.1.

* Fix errors on CRAN.




# tidymv v3.3.1

## Intention to deprecate notice

* tidymv will soon be deprecated, in favour of tidygam (https://github.com/stefanocoretta/tidygam). While development for tidygam is in progress, a notice of intention to deprecate is printed when the package is attached, so that users are warned.

## Developer

* Updated renv and packages.

* Updated GitHub Action for pkgdown.




# tidymv v3.3.0

## Added

* Zenodo DOI in Readme.

## Changed

* `predict_gam()` now has the extra argument `type`, which allows the user to return the predicted values based on all terms or based on each term separately.


# tidymv v3.2.1

## Added

* Use renv for development.

* `@return` roxygen entry added to all functions documentation.

* `ci_z` argument in `plot_difference()`.

## Changed

* The output of `get_smooths_difference()` now includes a `group` column with a numeric index of significance blocks, so that plotting difference smooths with more than two alternating significance values is correct (see `plot-smooths` vignette, last example).




# tidymv v3.2.0

## Added

* ✨ - `get_smooths_difference()` which returns a tibble with the difference of two levels of a smooth (#11).

* 🍇 - `plot-smooths.Rmd` now includes a plotting example with `get_smooths_difference()`.

* 👷 - Add GitHub actions for R CMD check.

* 🖼 - Add logo!

## Changed

* ⬆️ - Use tidy evaluation.

## Removed

* 🔥 - Travis CI has been removed (now use GitHub Actions).

# tidymv v3.1.0

## Added

* reintroduced `plot_difference()`.

* imported code from itsadug for better integration with `plot_difference()` and compatibility with R 3.6.1.

* 🍇 include example of `get_gam_predictions()` in plot smooths vignette.

## Changed

* `get_gam_predictions()` now returns an `.idx` column which assigns a number to each curve (when multiple variables are used). The `.idx` column can be used to correctly group `geom_ribbon()` for plotting CIs.

# tidymv 3.0.0

## Removed

* ⚠️ BREAKING CHANGE!!! remove `plot_difference()` due to the archiving of itsadug. To plot difference smooths, you can use `mgcViz::plotDiff()`.

# tidymv 2.2.1

## Changed

* prevent error from `dplyr::lag()` when type of default is different (prepare for upcoming dplyr 1.0.0)

* roxygen version 7.1.0

# tidymv 2.2.0

## Added

* `transform` argument in `get_gam_predictions()` and `plot_smooths()` (closes https://github.com/stefanocoretta/tidymv/issues/9)

* `exlude_random` argument in `plot_difference()` (it was `TRUE` by default and it was not possible to change it)

* error message in `get_gam_predictions` when using discretised bam models and `exclude_random = TRUE` (which is the default)

* two example datasets to be used in the examples

* example of non-Gaussian GAM in `plot*smooths` vignette

* support for models with non*syntactic column names (`log(y) ~ s(log(x))`)

## Changed

* add option of setting values to `NULL` in `predict_gam()` when excluding terms to reduce computation time (also mentioned in the vignette)

* improved performance of `get_gam_predictions()` when excluding terms

* included mention to loaded packages in the vignettes

## Fixed

* wrong examples in `plot_smooths()` and `plot_difference()` (closes https://github.com/stefanocoretta/tidymv/issues/10)

* handling of `s(bs = "re")` smooths

## Removed

* import from cowplot (the function that required it has been removed)

# tidymv v2.1.0

## Added

* examples in documentation for all functions

## Changed

* alpha of 0 line in difference smooth plot (now set to 0.5)

* prepared for CRAN first release

## Fixed
* missing import in `plot_difference()`

## Deprecated

* ⚠️ `time_series` is now deprecated and replaced with `series`. `time_series` will be removed in future releases.

# tidymv v2.0.0

## Added

* `predict_gam()` to return a dataframe with all predictors and fitted values with standard error

* `geom_smooth_ci()` which provides a new `ggplot2` `geom` to conveniently plot smooths and confidence intervals from the output of `predict_gam()`

* vignette that illustrates how to use the new functions

* examples in the documentation

## Changed

* name, arguments, and output of `create_event_start()` (> `create_start_event()`, it breaks backward compatibility)

## Removed

* `plot_gamsd()` (use `plot_smooths()` and `plot_difference()`)

# tidymv v1.5.4

## Changed

* README now uses new `install_github` argument for building vignettes

# tidymv v1.5.3

## Added

* plot difference smooth in vignette

## Changed

* added deprecated notice in vignette

# tidymv v1.5.2

## Fixed

* error with updated `rlang`

# tidymv v1.5.1

## Added

* URL and BugReports in DESCRIPTION

## Changed

* `series_length` default to 100 in `plot_difference()`

# tidymv v1.5.0

## Added

* `plot_difference()` to plot difference smooths

* support for plotting single smooth

## Changed

* deprecated message with `plot_gamsd()` now mentions `plot_difference()`

## Fixed

* vignettes titles and plots

# tidymv v1.4.0

## Added

* `split` argument for separating columns (useful for interactions)

* how to plot interactions in vignette

* `exclude_terms` argument to exclude terms when predicting

* exclude `tensor.smooths` when plotting smooths

* experimental `ylim` argument to `plot_gamsd()`

## Fixed

* message in `plot_gamsd()` which said "will be deprecated"

# tidymv v1.3.1

## Fixed

* warning in `get_gam_predictions()`

# tidymv v1.3.0

## Added

* `get_gam_predictions()` for predicting with a `gam` object

* `plot_smooths()` for plotting smooths

* `plot*smooths.Rmd` vignette

* support for models with smooths with terms not in `time_series`

## Fixed

* typo in documentation of `create_event_start()`

* double call of `itsadug` in DESCRIPTION

* note about `lag` on check

* note about non*imported `itsadug`

* incompatibility with `rlang@v0.2.0`

## Deprecated

* `plot_gamsd()` will be deprecated: use `plot_smooths` instead. Plotting both smooths and difference smooth is not supported yet.

# tidymv v1.2.0

## Added

* `rm_re` parameter for removing random effects in `plot_gamsd`

* depends on `itsadug`

## Changed

* argument `event.col` to `event_col` in `create_start_event()` for style consistency

# tidymv v1.1.0

### Added

* black and white option for `plot_gamsd()`

## Changed

* `gam` model in the vignette now has a reference smooth `s(x2)`

* updated vignette

# tidymv v1.0.0

## Added

* documentation of package

## Changed

* use `lag()` for `create_event_start()`

## Fixed

* error in `plot_gamsd` about missing xmin and xmax in annotate if there is no difference

# tidymv v0.1.0

## Added

* `create_start_event` function

* `plot_gamsd` function


