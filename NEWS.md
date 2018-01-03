# Change Log

## Unreleased
### Added
- `get_gam_predictions` for predicting with a `gam` object
- `plot_smooths` for plotting smooths

### Fixed
- typo in documentation of `create_event_start()`
- double call of `itsadug` in DESCRIPTION
- note about `lag` on check
- note about non-imported `itsadug`

### Deprecated
- `plot_gamsd()`: use `plot_smooths` instead. Plotting both smooths and difference smooth is not supported yet.

## 1.2.0 - 2017-12-07
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

[1.1.0]: https://github.com/stefanocoretta/tidymv/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/stefanocoretta/tidymv/compare/v0.1.0...v1.0.0
