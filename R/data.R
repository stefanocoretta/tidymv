#' Dataset with two factors
#'
#' A dataset with a normal-distributed outcome variable and two factors.
#'
#' @docType data
#' @format A tibble with 1259 observations and 4 variables.
#' \describe{
#'   \item{`x0`}{time series}
#'   \item{`y`}{outcome variable}
#'   \item{`x1`}{factor with three levels}
#'   \item{`x2`}{factor with two levels}
#'}
"inter_df"

#' Dataset with a Poisson outcome variable
#'
#' A dataset with a Poisson-distributed outcome variable and a factor.
#'
#' @docType data
#' @format A tibble with 2500 observations and 3 variables.
#' \describe{
#'   \item{`y`}{outcome count variable}
#'   \item{`x`}{time series}
#'   \item{`fac`}{factor with two levels}
#'}
"pois_df"
