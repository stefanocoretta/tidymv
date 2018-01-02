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

#' Plot GAM smooths.
#'
#' It plots the smooths from the estimates of a \link[mgcv]{gam} or \link[mgcv]{bam} object.
#'
#' @param model A \code{gam} or \code{bam} model object.
#' @param time_series An unquoted expression indicating the model term that defines the time series.
#' @param comparison An unquoted expression indicating the model term for which the comparison will be plotted.
#' @param facet_terms An unquoted formula with the terms used for faceting.
#'
#' @importFrom magrittr "%>%"
#' @export
plot_smooths <- function(model, time_series, comparison, facet_terms = NULL, plot_random = FALSE) {
    if (plot_random) {
        stop("Plotting of random smooths not supported yet.")
    }

    time_series_q <- dplyr::enquo(time_series)
    comparison_q <- dplyr::enquo(comparison)
    facet_terms_q <- dplyr::enquo(facet_terms)
    outcome_q <- model$formula[[2]]

    fitted <- model$model

    for (i in 1:length(model[["smooth"]])) {
        random_effects <- list()
        random_effects_terms <- NULL
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

    time_series_min <- dplyr::select(fitted, !!time_series_q) %>% min()
    time_series_max <- dplyr::select(fitted, !!time_series_q) %>% max()

    fitted <- fitted %>%
        dplyr::select(-!!time_series_q, -!!outcome_q)

    fitted_series <- fitted %>%
        unique() %>%
        dplyr::mutate(
            !!dplyr::quo_name(time_series_q) := rep(
                list(seq(time_series_min, time_series_max, length.out = 25)),
                nrow(.)
            )
        ) %>%
        tidyr::unnest(Timeinterval)

    predicted <- predict(
        model,
        fitted_series,
        se.fit = TRUE,
        exclude = ifelse(plot_random, NULL, random_effects)
    )

    predicted_tbl <- cbind(fitted_series, predicted) %>%
        dplyr::mutate(
            CI_upper = fit + 1.96 * se.fit,
            CI_lower = fit - 1.96 * se.fit
        ) %>%
        select(-!!!rlang::syms(random_effects_terms)) %>%
        unique()

    smooths_plot <- predicted_tbl %>%
        ggplot2::ggplot(
            ggplot2::aes_string(
                dplyr::quo_name(time_series_q), "fit"
            )
        ) +
        ggplot2::geom_ribbon(
            ggplot2::aes_string(
                ymin = "CI_lower",
                ymax = "CI_upper",
                fill = dplyr::quo_name(comparison_q)
            ),
            alpha = 0.2
        ) +
        ggplot2::geom_path(
            aes_string(colour = dplyr::quo_name(comparison_q))
        ) +
        {if (!is.null(facet_terms)) {
            ggplot2::facet_wrap(facet_terms_q)
        }}

    print(smooths_plot)
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
#'
#' @importFrom magrittr "%>%"
#' @export
plot_gamsd <- function(model, view, comparison, conditions = NULL, rm_re = FALSE, bw = FALSE) {
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
        {if (bw == FALSE) {ggplot2::scale_fill_discrete(name = names(comparison))}}

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

#' Plot GAM estimate smooths of an interaction.
#'
#' It plots estimate smooths of the interaction terms from a \link[mgcv]{gam} or \link[mgcv]{bam}.
#'
#' @param model A \code{gam} or \code{bam} model object.
#' @param view A string with the predictor determining the time series.
#' @param interactions A named list the levels to be plotted from the main comparison predictor and the interactions.
#' @param conditions A named list with rhe values to use for other predictors.
#' @param rm_re Whether to remove random effects (the default is \code{FALSE}).
#' @param print_summary Wether to print a summary of the values selected for each predictor (the default is \code{TRUE}).
#'
#' @export
plot_gami <- function(model, view, interactions, conditions = NULL, rm_re = FALSE, print_summary = TRUE) {
    view_series <- list(
        seq(min(model[["model"]][[view]]), max(model[["model"]][[view]]), length = 100)
    )
    names(view_series) <- view

    condition = c(view_series, conditions)

    smooth_df <- itsadug::get_predictions(
        model,
        cond = c(interactions, condition),
        rm.ranef = rm_re,
        print.summary = print_summary
    )

    gami_plot <- ggplot2::ggplot()

    fit <- "fit"

    hues <- seq(15, 375, length = length(interactions[[1]]) + 1)
    colours <- grDevices::hcl(h = hues, l = 65, c = 100)

    for (i in 1:length(interactions[[1]])) {
        filtered_data <- smooth_df

        for (j in 1:length(interactions)) {
            names_interaction <- names(interactions)[[j]]
            filtered_data <- dplyr::filter(
                filtered_data,
                filtered_data[[names_interaction]] == interactions[[j]][i]
            )
        }

        ymin_ci <- filtered_data$fit - filtered_data$CI
        ymax_ci <- filtered_data$fit + filtered_data$CI

        gami_plot <- gami_plot +
            ggplot2::geom_ribbon(
                data = filtered_data,
                ggplot2::aes_string(
                    view,
                    ymin = ymin_ci,
                    ymax = ymax_ci
                ),
                alpha = 0.2,
                fill = colours[i]
            ) +
            ggplot2::geom_line(
                data = filtered_data,
                ggplot2::aes_string(view, fit),
                colour = colours[i]
            )
    }

    gami_plot
}

#' Plot all levels of an interaction of a GAM.
#'
#' It plots the smooths from all the levels of an interaction from a \link[mgcv]{bam} or \link[mgcv]{gam}.
#'
#' @param model A \code{gam} or \code{bam} model object.
#' @param view The predictor determining the time series as a string.
#' @param main The predictor for which individual plots need to be produced as a string.
#' @param comparison The comparison predictor as a string.
#' @param interaction The predictor which defines the interaction as a string.
#' @param conditions The values to use for other predictors as a named list.
#' @param rm_re Whether to remove random effects (the default is \code{FALSE}).
#' @param print_summaries Wether to print a summary of the values selected for each predictor (the default is \code{TRUE}).
#'
#' @export
plot_gami_all <- function(model, view, main, comparison, interaction, conditions = NULL, rm_re = FALSE, print_summaries = FALSE) {
    main_levels <- levels(model[["model"]][[main]])
    comparison_levels <- levels(model[["model"]][[comparison]])

    plot_list <- list()

    for (i in 1:length(main_levels)) {
        interaction_list = list(
            c(comparison_levels),
            c(
                paste(main_levels[i], comparison_levels[1], sep = "."),
                paste(main_levels[i], comparison_levels[2], sep = ".")
            )
        )
        names(interaction_list) <- c(comparison, interaction)

        main_list <- list(main_levels[i])
        names(main_list) <- main

        this_plot <- tidymv::plot_gami(
            model = model,
            view = view,
            interaction = interaction_list,
            conditions = c(
                conditions,
                main_list
            ),
            rm_re = rm_re,
            print_summary = print_summaries
        ) +
            ggplot2::labs(title = main_levels[i])

        plot_list <- c(plot_list, list(this_plot))

    }

    y_ranges <- NULL

    for (i in 1:length(main_levels)) {
        y_ranges <- c(y_ranges, ggplot2::ggplot_build(plot_list[[i]])$layout$panel_ranges[[1]]$y.range)
    }

    for (i in 1:length(main_levels)) {
        plot_list[[i]] <- plot_list[[i]] + list(ggplot2::ylim(min(y_ranges), max(y_ranges)))
    }

    cowplot::plot_grid(plotlist = plot_list)
}