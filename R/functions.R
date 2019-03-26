#' Create a start event column.
#'
#' Create a new column which marks the beginning of each series in a tibble (for example, time series).
#'
#' @param tibble A tibble arranged according to the series.
#' @param series_col The name of the column that defines the group of series, as an unquoted expression.
#'
#' @return A tibble with an extra column that marks the begninning of the series.
#'
#' @examples
#' library(dplyr)
#' series_tbl <- tibble(
#'   time_series = rep(1:5, 3),
#'   group = rep(c("a", "b", "c"), each = 5)
#' ) %>%
#'   create_start_event(group)
#'
#' @export
create_start_event <- function(tibble, series_col) {
  series_col_q <- dplyr::enquo(series_col)
  series_col_name <- rlang::quo_name(series_col_q)

  dplyr::mutate(
    tibble,
    start_event = ifelse(
      as.character(tibble[[series_col_name]]) == dplyr::lag(as.character(tibble[[series_col_name]]), default = FALSE),
      FALSE,
      TRUE
    )
  )
}

#' Get all predictions from a GAM model.
#'
#' It returns a tibble with the predictions from all the terms in a \link[mgcv]{gam} or \link[mgcv]{bam} model.
#'
#' @param model A \code{gam} or \code{bam} model object.
#' @param exclude_terms Terms to be excluded from the prediction. Term names should be given as they appear in the model summary (for example, \code{"s(x0,x1)"}).
#' @param length_out An integer indicating how many values along the numeric predictors to use for predicting the outcome term (the default is \code{50}).
#' @param values User supplied values for numeric terms as a named list.
#'
#' @return A tibble with predictions from a a \link[mgcv]{gam} or \link[mgcv]{bam} model.
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#' data <- gamSim(4)
#' model <- gam(y ~ fac + s(x2) + s(x2, by = fac) + s(x0), data = data)
#'
#' # get predictions
#' p <- predict_gam(model)
#'
#' # get predictions excluding x0 (the coefficient of x0 is set to 0)
#' p_2 <- predict_gam(model, exclude_terms = "s(x0)")
#'
#' # get predictions with chosen values of x0
#'
#' p_3 <- predict_gam(model, values = list(x0 = c(0.250599, 0.503313, 0.756028)))
#'
#' @export
predict_gam <- function(model, exclude_terms = NULL, length_out = 50, values = NULL) {
  n_terms <- length(model[["var.summary"]])

  term_list <- list()

  for (term in 1:n_terms) {
    term_summary <- model[["var.summary"]][[term]]
    term_name <- names(model[["var.summary"]])[term]

    if (term_name %in% names(values)) {
      new_term <- values[[which(names(values) == term_name)]]
    } else {
      if (is.numeric(term_summary)) {

        min_value <- min(term_summary)
        max_value <- max(term_summary)

        new_term <- seq(min_value, max_value, length.out = length_out)

      } else if (is.factor(term_summary)) {

        new_term <- levels(term_summary)

      } else {
        stop("The terms are not numeric or factor.\n")
      }
    }

    term_list <- append(term_list, list(new_term))

    names(term_list)[term] <- term_name
  }

  new_data <- expand.grid(term_list)

  predicted <- as.data.frame(mgcv::predict.gam(model, new_data, exclude = exclude_terms, se.fit = TRUE))

  predictions <- cbind(new_data, predicted)

  predictions <- tibble::as_tibble(predictions)

  return(predictions)
}

#' Get predictions from a GAM model.
#'
#' It returns a tibble with the predictions from a a \link[mgcv]{gam} or \link[mgcv]{bam} object.
#'
#' @param model A \code{gam} or \code{bam} model object.
#' @param series An unquoted expression indicating the model term that defines the series on which smoothing is applied. This is the term that is displayed on the x-axis when plotting.
#' @param time_series Deprecated, use \code{series} instead.
#' @param series_length An integer indicating how many values along the time series to use for predicting the outcome term.
#' @param conditions A list of quosures with \link[rlang]{quos} specifying the levels to plot from the model terms.
#' @param exclude_random Whether to exclude random smooths (the default is \code{TRUE}).
#' @param exclude_terms Terms to be excluded from the prediction. Term names should be given as they appear in the model summary (for example, \code{"s(x0,x1)"}).
#' @param split Columns to separate as a named list.
#' @param sep Separator between columns (default is \code{"\\."}, which is the default with \code{}). If character, it is interpreted as a regular expression.
#'
#' @export
get_gam_predictions <- function(model, series, series_length = 25, conditions = NULL, exclude_random = TRUE, exclude_terms = NULL, split = NULL, sep = "\\.", time_series) {
    if(!missing(time_series)) {
      warning("This argument has been deprecated and will be removed in the future. Please use `series` instead.")

      series_q = dplyr::enquo(time_series)
    } else {
      time_series = NULL
      series_q <- dplyr::enquo(series)
    }

    series_name <- rlang::quo_name(series_q)
    outcome_q <- model$formula[[2]]

    fitted <- model$model

    random_effects <- list()
    random_effects_terms <- NULL

    if (exclude_random == TRUE) {
        for (i in 1:length(model[["smooth"]])) {
            smooth_class <- attr(model$smooth[[i]],"class")[1]
            if (smooth_class %in% c("random.effect", "fs.interaction")) {
                random_effects <- c(
                    random_effects,
                    list(model$smooth[[i]]$label)
                )
                random_effects_terms <- c(
                    random_effects_terms,
                    model$smooth[[i]]$fterm
                )
            }
        }
    }

    series_min <- dplyr::select(fitted, !!series_q) %>% min()
    series_max <- dplyr::select(fitted, !!series_q) %>% max()

    fitted <- fitted %>%
        dplyr::select(-!!series_q, -!!outcome_q)

    if ("(AR.start)" %in% colnames(fitted)) {
        fitted$`(AR.start)` <- NULL
    }

    if (ncol(fitted) > 0) {
        fitted_series <- fitted %>%
            unique()
    } else {
        fitted_series <- fitted
    }

    fitted_series <- fitted_series %>%
        dplyr::mutate(
            !!rlang::quo_name(series_q) := rep(
                list(seq(series_min, series_max, length.out = series_length)),
                nrow(fitted_series)
            )
        ) %>%
        tidyr::unnest(!!series_q)

    if (ncol(fitted_series) > 0) {
        fitted_series <- fitted_series %>%
            unique()
    }

    if (exclude_random) {
        if (rlang::is_empty(random_effects)) {
            exclude_random_effects <- as.null()
        } else {
            exclude_random_effects <- random_effects
        }
    } else {
        exclude_random_effects <- as.null()
    }

    # Exclude smooth terms which are not the time series to be plotted or tensor smooths
    exclude_smooths <- as.null()
    excluded_terms <- as.null()
    for (smooth in 1:length(model[["smooth"]])) {
        smooth_term <- model[["smooth"]][[smooth]][["term"]][[1]]
        if (smooth_term != series_name) {
            excluded_terms <- c(excluded_terms, smooth_term)
            smooth_label <- model[["smooth"]][[smooth]][["label"]]
            exclude_smooths <- c(exclude_smooths, smooth_label)
        }
        smooth_class <- attr(model$smooth[[smooth]],"class")[1]
        if (smooth_class == "tensor.smooth") {
            smooth_term <- model[["smooth"]][[smooth]][["term"]][[2]]
            excluded_terms <- c(excluded_terms, smooth_term)
            smooth_label <- model[["smooth"]][[smooth]][["label"]]
            exclude_smooths <- c(exclude_smooths, smooth_label)
        }
    }

    excluded <- as.null()
    if (!is.null(exclude_terms)) {
        for (term in 1:length(exclude_terms)) {
            for (label in 1:length(model[["smooth"]])) {
                smooth_label <- model[["smooth"]][[label]][["label"]]
                if (smooth_label == exclude_terms[term]) {
                    smooth_term <- model[["smooth"]][[label]][["term"]]
                    if (length(smooth_term) > 1) {
                        smooth_term_2 <- model[["smooth"]][[label]][["term"]][[2]]
                        excluded <- c(excluded, smooth_term_2)
                    }
                }
            }
        }
    }

    exclude_these <- c(exclude_random_effects, exclude_smooths, exclude_terms)

    predicted <- stats::predict(
        model,
        fitted_series,
        se.fit = TRUE,
        exclude = exclude_these
    )

    predicted_tbl <- cbind(fitted_series, predicted) %>%
        dplyr::mutate(
            CI_upper = fit + 1.96 * se.fit,
            CI_lower = fit - 1.96 * se.fit
        ) %>%
        dplyr::rename(
            !!outcome_q := fit,
            SE = se.fit
        )

    if (!is.null(exclude_random_effects)) {
        predicted_tbl <- predicted_tbl %>%
            dplyr::select(-dplyr::one_of(random_effects_terms)) %>%
            unique()
    }

    if (!is.null(exclude_smooths)) {
        predicted_tbl <- predicted_tbl %>%
            dplyr::select(-dplyr::one_of(excluded_terms)) %>%
            unique()
    }

    if (!is.null(excluded)) {
        predicted_tbl <- predicted_tbl %>%
            dplyr::select(-dplyr::one_of(excluded)) %>%
            unique()
    }

    if (!is.null(split)) {
        for (i in 1:length(split)) {
            predicted_tbl <- tidyr::separate(
                data = predicted_tbl,
                col = names(split)[i],
                into = split[[i]],
                sep = sep
            )
        }
    }

    if (!is.null(conditions)) {
        predicted_tbl <- predicted_tbl %>%
            dplyr::filter(!!!conditions)
    }

    return(predicted_tbl)
}

#' Plot GAM smooths.
#'
#' It plots the smooths from the estimates of a \link[mgcv]{gam} or \link[mgcv]{bam} object.
#'
#' @inheritParams get_gam_predictions
#' @param comparison An unquoted expression indicating the model term for which the comparison will be plotted.
#' @param facet_terms An unquoted formula with the terms used for faceting.
#' @param conditions A list of quosures with \link[rlang]{quos} specifying the levels to plot from the model terms not among \code{series}, \code{comparison}, or \code{facet_terms}.
#'
#' @examples
#' # see vignette
#' \dontrun{
#' vignette("plot-smooths", package = "tidymv")
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang ":="
#' @importFrom rlang "quo_name"
#' @importFrom stats "predict"
#' @export
plot_smooths <- function(model, series, comparison = NULL, facet_terms = NULL, conditions = NULL, exclude_random = TRUE, exclude_terms = NULL, series_length = 25, split = NULL, sep = "\\.", time_series) {
    if(!missing(time_series)) {
      warning("This argument has been deprecated and will be removed in the future. Please use `series` instead.")

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

    predicted_tbl <- get_gam_predictions(model, !!series_q, conditions, exclude_random = exclude_random, exclude_terms = exclude_terms, series_length = series_length, split = split, sep = sep)

    smooths_plot <- predicted_tbl %>%
        ggplot2::ggplot(
            ggplot2::aes_string(
                rlang::quo_name(series_q), rlang::quo_name(outcome_q)
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
#' @examples
#' # see vignette
#' \dontrun{
#' vignette("plot-smooths", package = "tidymv")
#' }
#'
#' @inheritParams get_gam_predictions
#' @param difference A named list with the levels to compute the difference of.
#' @param conditions A named list specifying the levels to plot from the model terms not among \code{series} or \code{difference}. Notice the difference with \link[tidymv]{plot_smooths}, which uses \link[rlang]{quos}.
#'
#' @export
plot_difference <- function(model, series, difference, conditions = NULL, series_length = 100, time_series) {
    if(!missing(time_series)) {
      warning("This argument has been deprecated and will be removed in the future. Please use `series` instead.")

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

    diff <- itsadug::get_difference(model, difference, cond = conditions, print.summary = FALSE) %>%
        dplyr::mutate(
            CI_upper = difference + CI,
            CI_lower = difference - CI
        )

    sig_diff <- itsadug::find_difference(
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
#' It provides a `geom` for plotting GAM smooths with confidence intervals from the output of \link[tidymv]{predict_gam}. It inherits the following `aes` from a call to `ggplot`:
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
