# Change Log

## [Unreleased]
### Added
- `rm_re` parameter for removing random effects in `plot_gamsd`

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
