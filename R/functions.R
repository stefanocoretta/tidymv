#' Create a start event column.
#'
#' Create a new column which marks the beginning of each time series in a tibble.
#' The effect is the same as \code{start_event} from \code{itsadug}, but it works
#' with tibbles.
#'
#' @param tibble A tibble arranged according to the time series.
#' @param event_col A string with the name of the column that defines the time series.
#'
#' @export
create_event_start <- function(tibble, event_col) {
    dplyr::mutate(
        tibble,
        start.event = ifelse(
            as.character(tibble[[event_col]]) == dplyr::lag(as.character(tibble[[event_col]]), default = FALSE),
            FALSE,
            TRUE
            )
    )
}

#' Get predictions from a GAM model.
#'
#' It returns a tibble with the predictions from a a \link[mgcv]{gam} or \link[mgcv]{bam} object.
#'
#' @param model A \code{gam} or \code{bam} model object.
#' @param time_series An unquoted expression indicating the model term that defines the time series.
#' @param series_length An integer indicating how many values along the time series to use for predicting the outcome term.
#' @param conditions A list of quosures with \link[rlang]{quos} specifying the levels to plot from the model terms.
#' @param exclude_random Whether to exclude random smooths (the default is \code{TRUE}).
#' @param exclude_terms Terms to be excluded from the prediction. Term names should be given as they appear in the model summary (for example, \code{"s(x0,x1)"}).
#' @param split Columns to separate as a named list.
#' @param sep Separator between columns (default is \code{"\\."}, which is the default with \code{}). If character, it is interpreted as a regular expression.
#'
#' @export
get_gam_predictions <- function(model, time_series, series_length = 25, conditions = NULL, exclude_random = TRUE, exclude_terms = NULL, split = NULL, sep = "\\.") {
    time_series_q <- dplyr::enquo(time_series)
    time_series_name <- dplyr::quo_name(time_series_q)
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

    time_series_min <- dplyr::select(fitted, !!time_series_q) %>% min()
    time_series_max <- dplyr::select(fitted, !!time_series_q) %>% max()

    fitted <- fitted %>%
        dplyr::select(-!!time_series_q, -!!outcome_q)

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
            !!dplyr::quo_name(time_series_q) := rep(
                list(seq(time_series_min, time_series_max, length.out = series_length)),
                nrow(fitted_series)
            )
        ) %>%
        tidyr::unnest(!!time_series_q)

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
        if (smooth_term != time_series_name) {
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
#' @param conditions A list of quosures with \link[rlang]{quos} specifying the levels to plot from the model terms not among \code{time_series}, \code{comparison}, or \code{facet_terms}.
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang ":="
#' @importFrom stats "predict"
#' @export
plot_smooths <- function(model, time_series, comparison = NULL, facet_terms = NULL, conditions = NULL, exclude_random = TRUE, exclude_terms = NULL, series_length = 25, split = NULL, sep = "\\.") {
    time_series_q <- dplyr::enquo(time_series)
    comparison_q <- dplyr::enquo(comparison)
    facet_terms_q <- dplyr::enquo(facet_terms)
    if (comparison_q == dplyr::quo(NULL)) {
        comparison_q <- NULL
    }
    if (facet_terms_q == dplyr::quo(NULL)) {
        facet_terms_q <- NULL
    }
    outcome_q <- model$formula[[2]]

    predicted_tbl <- get_gam_predictions(model, !!time_series_q, conditions, exclude_random = exclude_random, exclude_terms = exclude_terms, series_length = series_length, split = split, sep = sep)

    smooths_plot <- predicted_tbl %>%
        ggplot2::ggplot(
            ggplot2::aes_string(
                dplyr::quo_name(time_series_q), dplyr::quo_name(outcome_q)
            )
        ) +
        {if (!is.null(comparison_q)) {
            ggplot2::geom_ribbon(
                ggplot2::aes_string(
                    ymin = "CI_lower",
                    ymax = "CI_upper",
                    fill = dplyr::quo_name(comparison_q)
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
                    colour = dplyr::quo_name(comparison_q),
                    linetype = dplyr::quo_name(comparison_q)
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
#' @export
plot_gamsd <- function(model, view, comparison, conditions = NULL, rm_re = FALSE, bw = FALSE, ylim = NULL) {
    .Deprecated("plot_smooth", msg = "'plot_gamsd' is deprecated and will be removed, use 'plot_smooths()' and 'plot_difference'.\n")

    diff.df <- itsadug::plot_diff(
        model,
        view = view,
        comp = comparison,
        cond = conditions,
        rm.ranef = rm_re,
        plot = FALSE,
        print.summary = FALSE)

    main.condition <- list(
        seq(min(diff.df[[view]]), max(diff.df[[view]]), length = 100)
    )
    names(main.condition) <- view

    condition = c(main.condition, conditions)

    smooth.df <- itsadug::get_predictions(
        model,
        cond = c(comparison, condition),
        rm.ranef = rm_re
    )

    sig.diff <- itsadug::find_difference(
        diff.df$est, diff.df$CI, diff.df[[view]]
    )

    ymin.sm <- smooth.df$fit - smooth.df$CI
    ymax.sm <- smooth.df$fit + smooth.df$CI

    fit <- "fit"
    comp.column <- names(comparison)

    annotate <- ggplot2::annotate(
        "rect",
        xmin = sig.diff$start, xmax = sig.diff$end,
        ymin = -Inf, ymax = Inf, alpha = 0.1,
        fill = ifelse(bw == FALSE, "red", "black")
    )

    is.sig <- is.null(sig.diff) == FALSE

    smooth.plot <- smooth.df %>%
        ggplot2::ggplot(
            ggplot2::aes_string(view, fit)
        ) +
        {if (is.sig) {annotate}} +
        ggplot2::geom_ribbon(
            ggplot2::aes(ymin = ymin.sm,
                         ymax = ymax.sm,
                         group = smooth.df[[comp.column]]
                        ),
            alpha = 0.2,
            colour = "NA"
        ) +
        {if (bw == FALSE) {
            ggplot2::aes(fill = smooth.df[[comp.column]])}
        } +
        {if (bw == FALSE) {
            ggplot2::geom_line(
                ggplot2::aes(colour = smooth.df[[comp.column]])
            )
            }
            else {
                ggplot2::geom_line(
                ggplot2::aes(linetype = smooth.df[[comp.column]])
                )
            }
        } +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            legend.position = "top"
        ) +
        ggplot2::xlim(min(diff.df[[view]]), max(diff.df[[view]])) +
        {if (bw == FALSE) {
            ggplot2::scale_colour_discrete(name = names(comparison))
            }
            else {
                ggplot2::scale_linetype_discrete(name = names(comparison))
                }
        } +
        {if (bw == FALSE) {ggplot2::scale_fill_discrete(name = names(comparison))}} +
        {if (!is.null(ylim)) {ggplot2::ylim(ylim[1], ylim[2])}}

    ymin.di <- diff.df$est - diff.df$CI
    ymax.di <- diff.df$est + diff.df$CI

    est <- "est"

    diff.plot <- diff.df %>%
        ggplot2::ggplot(
            ggplot2::aes_string(view, est)
        ) +
        ggplot2::geom_hline(yintercept = 0, size = 0.3) +
        {if (is.sig) {annotate}} +
        ggplot2::geom_ribbon(
            ggplot2::aes(
                ymin = ymin.di,
                ymax = ymax.di
            ),
            alpha = 0.2
        ) +
        ggplot2::geom_line() +
        ggplot2::xlim(min(diff.df[[view]]), max(diff.df[[view]])) +
        ggplot2::theme_bw()

    cowplot::plot_grid(smooth.plot, diff.plot, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
}

#' Plot difference smooth from a GAM.
#'
#' It plots the difference smooth from a \link[mgcv]{gam} or \link[mgcv]{bam}.
#' Significant differences are marked with red areas.
#'
#' @inheritParams get_gam_predictions
#' @param time_series An unquoted expression indicating the model term that defines the time series.
#' @param difference A named list with the levels to compute the difference of.
#' @param conditions A named list specifying the levels to plot from the model terms not among \code{time_series} or \code{difference}. Notice the difference with \link[tidymv]{plot_smooths}, which uses \link[rlang]{quos}.
#'
#' @export
plot_difference <- function(model, time_series, difference, conditions = NULL, series_length = 25) {
    time_series_q <- dplyr::enquo(time_series)
    time_series_chr <- quo_name(time_series_q)

    fitted <- model$model

    time_series_min <- dplyr::select(fitted, !!time_series_q) %>% min()
    time_series_max <- dplyr::select(fitted, !!time_series_q) %>% max()

    conditions <- c(conditions, rlang::ll(!!time_series_chr := seq(time_series_min, time_series_max, length.out = series_length)))

    diff <- itsadug::get_difference(model, difference, cond = conditions, print.summary = FALSE) %>%
        dplyr::mutate(
            CI_upper = difference + CI,
            CI_lower = difference - CI
        )

    sig_diff <- itsadug::find_difference(
        diff$difference, diff$CI, diff[[time_series_chr]]
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
                dplyr::quo_name(time_series_q), "difference"
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
        geom_hline(yintercept = 0)

    return(diff_plot)
}