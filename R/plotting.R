#' Plot GAM smooths.
#'
#' It plots the smooths from the estimates of a \link[mgcv]{gam} or \link[mgcv]{bam} object.
#'
#' @inheritParams get_gam_predictions
#' @param comparison An unquoted expression indicating the model term for which the comparison will be plotted.
#' @param facet_terms An unquoted formula with the terms used for faceting.
#' @param conditions A list of quosures with \code{quos} specifying the levels to plot from the model terms not among \code{series}, \code{comparison}, or \code{facet_terms}.
#'
#' @return A \code{\link[ggplot2]{ggplot} object.}
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#' data <- gamSim(4)
#' model <- gam(y ~ fac + s(x2) + s(x2, by = fac) + s(x0), data = data)
#'
#' plot_smooths(model, x2, fac)
#'
#' # alternative model specification
#' model <- gam(y ~ s(fac, bs = "re") + s(x2) + s(x2, by = fac) + s(x0), data = data)
#' plot_smooths(model, x2, fac)
#'
#' # For details, see vignette
#' \dontrun{
#' vignette("plot-smooths", package = "tidymv")
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang ":="
#' @importFrom rlang "quo_name"
#' @importFrom stats "predict"
#' @export
plot_smooths <- function(model, series, comparison = NULL, facet_terms = NULL, conditions = NULL, exclude_random = TRUE, exclude_terms = NULL, series_length = 25, split = NULL, sep = "\\.", transform = NULL, ci_z = 1.96, time_series) {
    if (!missing(time_series)) {
      warning("The time_series argument has been deprecated and will be removed in the future. Please use `series` instead.")

      series_q = dplyr::enquo(time_series)
    } else {
      time_series = NULL
      series_q <- dplyr::enquo(series)
    }

    comparison_q <- dplyr::enquo(comparison)
    facet_terms_q <- dplyr::enquo(facet_terms)
    if (rlang::quo_is_null(comparison_q)) {
        comparison_q <- NULL
    }
    if (rlang::quo_is_null(facet_terms_q)) {
        facet_terms_q <- NULL
    }
    outcome_q <- model$formula[[2]]

    predicted_tbl <- get_gam_predictions(model, {{series}}, conditions, exclude_random = exclude_random, exclude_terms = exclude_terms, series_length = series_length, split = split, sep = sep, transform = transform, ci_z = ci_z, .comparison = {{comparison}})

    smooths_plot <- predicted_tbl %>%
        ggplot2::ggplot(
            ggplot2::aes_string(
              # quo_name and sym are needed for terms which are non-syntactic
              # column names like `log(x)`
              rlang::quo_name(series_q), rlang::sym(rlang::quo_name(outcome_q))
            )
        ) +
        {if (!is.null(comparison_q)) {
            ggplot2::geom_ribbon(
                ggplot2::aes_string(
                    ymin = "CI_lower",
                    ymax = "CI_upper",
                    fill = rlang::quo_name(comparison_q)
                ),
                alpha = 0.2
            )
        }} +
        {if (is.null(comparison_q)) {
            ggplot2::geom_ribbon(
                ggplot2::aes_string(
                    ymin = "CI_lower",
                    ymax = "CI_upper"
                ),
                alpha = 0.2
            )
        }} +
        {if (!is.null(comparison_q)) {
            ggplot2::geom_path(
                ggplot2::aes_string(
                    colour = rlang::quo_name(comparison_q),
                    linetype = rlang::quo_name(comparison_q)
                )
            )
        }} +
        {if (is.null(comparison_q)) {
            ggplot2::geom_path(
                ggplot2::aes_string()
            )
        }} +
        {if (!is.null(facet_terms_q)) {
            ggplot2::facet_wrap(facet_terms_q)
        }}

    return(smooths_plot)
}

#' Plot difference smooth from a GAM.
#'
#' It plots the difference smooth from a \link[mgcv]{gam} or \link[mgcv]{bam}.
#' Significant differences are marked with red areas.
#'
#' @return A \code{\link[ggplot2]{ggplot} object.}
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#' data <- gamSim(4)
#' model <- gam(y ~ fac + s(x2) + s(x2, by = fac) + s(x0), data = data)
#'
#' plot_difference(model, x2, list(fac = c("1", "2")))
#'
#' # For details, see vignette
#' \dontrun{
#' vignette("plot-smooths", package = "tidymv")
#' }
#'
#' @inheritParams get_gam_predictions
#' @param difference A named list with the levels to compute the difference of.
#' @param conditions A named list specifying the levels to plot from the model terms not among \code{series} or \code{difference}. Notice the difference with \link[tidymv]{plot_smooths}, which uses \code{quos}.
#'
#' @export
plot_difference <- function(model, series, difference, conditions = NULL, exclude_random = TRUE, series_length = 100, ci_z = 1.96, time_series) {
    if (!missing(time_series)) {
      warning("The time_series argument has been deprecated and will be removed in the future. Please use `series` instead.")

      series_q = dplyr::enquo(time_series)
    } else {
      time_series = NULL
      series_q <- dplyr::enquo(series)
    }

    series_chr <- rlang::quo_name(series_q)

    fitted <- model$model

    series_min <- dplyr::select(fitted, !!series_q) %>% min()
    series_max <- dplyr::select(fitted, !!series_q) %>% max()

    conditions <- c(conditions, rlang::ll(!!series_chr := seq(series_min, series_max, length.out = series_length)))

    diff <- suppressWarnings(tidymv::get_difference(model, difference, cond = conditions, rm.ranef = exclude_random, f = ci_z, print.summary = FALSE)) %>%
        dplyr::mutate(
            CI_upper = difference + CI,
            CI_lower = difference - CI
        )

    sig_diff <- tidymv::find_difference(
        diff$difference, diff$CI, diff[[series_chr]]
    )

    annotate <- ggplot2::annotate(
        "rect",
        xmin = sig_diff$start, xmax = sig_diff$end,
        ymin = -Inf, ymax = Inf, alpha = 0.1,
        fill = "red"
    )

    is_sig <- is.null(sig_diff) == FALSE

    diff_plot <- diff %>%
        ggplot2::ggplot(
            ggplot2::aes_string(
                rlang::quo_name(series_q), "difference"
            )
        ) +
        {if (is_sig) {annotate}} +
        ggplot2::geom_ribbon(
            ggplot2::aes_string(
                ymin = "CI_lower",
                ymax = "CI_upper"
            ),
            alpha = 0.2
        ) +
        ggplot2::geom_path(
        ) +
        ggplot2::geom_hline(yintercept = 0, alpha = 0.5)

    return(diff_plot)
}

#' Smooths and confidence intervals.
#'
#' It provides a `geom` for plotting GAM smooths with confidence intervals from the output of \link[tidymv]{predict_gam}. It inherits the following aesthetics from a call to \code{ggplot}:
#'   \itemize{
#'     \item The term defining the x-axis.
#'     \item The fitted values (the \code{fit} column in the tibble returned by \link[tidymv]{predict_gam}).
#'     \item The standard error of the fit (the \code{se.fit} column in the tibble returned by \link[tidymv]{predict_gam}).
#'   }
#'
#' @param group The optional grouping factor.
#' @param ci_z The z-value for calculating the CIs (the default is \code{1.96} for 95 percent CI).
#' @param ci_alpha Transparency value of CIs (the default is \code{0.1}).
#' @param data The data to be displayed in this layer. If \code{NULL}, it is inherited.
#' @param ... Arguments passed to \code{geom_path()}.
#'
#' @examples
#' library(mgcv)
#' library(ggplot2)
#' set.seed(10)
#' data <- gamSim(4)
#' model <- gam(y ~ fac + s(x2) + s(x2, by = fac), data = data)
#'
#' # get predictions
#' p <- predict_gam(model)
#'
#' # plot smooths and confidence intervals
#' ggplot(p, aes(x2, fit)) + geom_smooth_ci(fac)
#'
#' @export
geom_smooth_ci <- function(group = NULL, ci_z = 1.96, ci_alpha = 0.1, data = NULL, ...) {
  group_q <- rlang::enquo(group)

  if (rlang::quo_is_null(group_q)) {
    group_q <- NULL
  }

  if (is.null(group_q)) {
    list(
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = fit - (se.fit * ci_z),
          ymax = fit + (se.fit * ci_z)
        ),
        alpha = ci_alpha,
        data = data
      ),
      ggplot2::geom_path(
        data = data,
        ...
      )
    )
  } else {
    list(
      ggplot2::geom_ribbon(
        ggplot2::aes(
          ymin = fit - (se.fit * ci_z),
          ymax = fit + (se.fit * ci_z),
          group = !!group_q
        ),
        alpha = ci_alpha,
        data = data
      ),
      ggplot2::geom_path(
        ggplot2::aes(
          colour = as.factor(!!group_q), linetype = as.factor(!!group_q)
        ),
        data = data,
        ...
      ),
      ggplot2::scale_colour_discrete(name = rlang::quo_name(group_q)),
      ggplot2::scale_linetype_discrete(name = rlang::quo_name(group_q))
    )
  }

}

#' Plot GAM estimate smooths and difference curve.
#'
#' It plots comparison smooths from the estimates of a \link[mgcv]{gam} or \link[mgcv]{bam}
#' and the difference curve. Significant differences are marked with red areas.
#'
#' @param model A \code{gam} or \code{bam} model object.
#' @param view The predictor determining the time series.
#' @param comparison The levels for the comparison as a named list.
#' @param conditions The values to use for other predictors as a named list.
#' @param rm_re Whether to remove random effects (the default is \code{FALSE}).
#' @param bw Whether to plot in black and white (the default is \code{FALSE}).
#' @param ylim Limits of the y-axis of the smooths panel.
#'
#' @importFrom magrittr "%>%"
#' @name plot_gamsd-defunct
#' @seealso \code{\link{tidymv-defunct}}
#' @keywords internal
NULL

#' @rdname tidymv-defunct
#' @section This function is deprecated and has been removed. Please, use \link[tidymv]{plot_smooths} and  \link[tidymv]{plot_difference}.
#'
#' @export
plot_gamsd <- function(...) {
  .Defunct("plot_smooth", msg = "'plot_gamsd' was deprecated and has been removed, use 'plot_smooths()' and 'plot_difference()'.\n")
}
