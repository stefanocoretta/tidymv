utils::globalVariables(c("CI", "fit", "geom_hline", "se.fit", "SE", "CI_lower", "CI_upper", ".idx"))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "tidymv will be deprecated. Users are recommended
    to check out the in-progress replacement tidygam
    (https://github.com/stefanocoretta/tidygam)."
  )
}
