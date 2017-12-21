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


#' Plot GLM estimate smooths and difference curve.
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
