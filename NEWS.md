# Change Log

## [1.4.0] - 2018-07-03
### Added
- `split` argument for separating columns (useful for interactions)
- how to plot interactions in vignette
- `exclude_terms` argument to exclude terms when predicting
- exclude `tensor.smooths` when plotting smooths
- experimental `ylim` argument to `plot_gamsd()`

### Fixed
- message in `plot_gamsd()` which said "will be deprecated"

## [1.3.1] - 2018-02-26
### Fixed
- warning in `get_gam_predictions()`

## [1.3.0] - 2018-02-26
### Added
- `get_gam_predictions()` for predicting with a `gam` object
- `plot_smooths()` for plotting smooths
- `plot-smooths.Rmd` vignette
- support for models with smooths with terms not in `time_series`

### Fixed
- typo in documentation of `create_event_start()`
- double call of `itsadug` in DESCRIPTION
- note about `lag` on check
- note about non-imported `itsadug`
- incompatibility with `rlang@v0.2.0`

### Deprecated
- `plot_gamsd()` will be deprecated: use `plot_smooths` instead. Plotting both smooths and difference smooth is not supported yet.

## [1.2.0] - 2017-12-07
### Added
- `rm_re` parameter for removing random effects in `plot_gamsd`
- depends on `itsadug`

### Changed
- argument `event.col` to `event_col` in `create_start_event()` for style consistency

## [1.1.0] - 2017-10-31
### Added
- black and white option for `plot_gamsd()`

### Changed
- `gam` model in the vignette now has a reference smooth `s(x2)`
- updated vignette

## [1.0.0] - 2017-10-21
### Added
- documentation of package

### Changed
- use `lag()` for `create_event_start()`

### Fixed
- error in `plot_gamsd` about missing xmin and xmax in annotate if there is no difference

## 0.1.0 - 2017-06-29
### Added
- `create_start_event` function
- `plot_gamsd` function

[1.4.0]: https://github.com/stefanocoretta/tidymv/compare/v1.3.1...v1.4.0
[1.3.1]: https://github.com/stefanocoretta/tidymv/compare/v1.3.0...v1.3.1
[1.3.0]: https://github.com/stefanocoretta/tidymv/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/stefanocoretta/tidymv/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/stefanocoretta/tidymv/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/stefanocoretta/tidymv/compare/v0.1.0...v1.0.0
