#' Dataset with two factors
#'
#' A dataset with a normal-distributed outcome variable and two factors.
#'
#' @docType data
#' @format A tibble with 1259 observations and 4 variables.
#' \describe{
#'   \item{\code{x0}}{time series}
#'   \item{\code{y}}{outcome variable}
#'   \item{\code{x1}}{factor with three levels}
#'   \item{\code{x2}}{factor with two levels}
#'}
"inter_df"

#' Dataset with a Poisson outcome variable
#'
#' A dataset with a Poisson-distributed outcome variable and a factor.
#'
#' @docType data
#' @format A tibble with 2500 observations and 3 variables.
#' \describe{
#'   \item{\code{y}}{outcome count variable}
#'   \item{\code{x}}{time series}
#'   \item{\code{fac}}{factor with two levels}
#'}
"pois_df"
