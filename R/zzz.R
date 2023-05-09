utils::globalVariables(c("CI", "fit", "geom_hline", "se.fit", "SE", "CI_lower", "CI_upper", ".idx"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "tidymv has been superseded by tidygam. The package tidymv is no longer maintained but will be
    kept on CRAN to ensure reproducibility of older analyses. Users should
    use the replacement package tidygam for new analyses, which is available on
    CRAN and GitHub (https://github.com/stefanocoretta/tidygam)."
  )
}
