#' Create a start event column.
#'
#' Create a new column which marks the beginning of each series in a tibble (for example, time series).
#'
#' @param tibble A tibble arranged according to the series.
#' @param series_col The name of the column that defines the group of series, as an unquoted expression.
#'
#' @return A tibble with an extra column that marks the beginning of the series.
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
      as.character(tibble[[series_col_name]]) == dplyr::lag(as.character(tibble[[series_col_name]]), default = "FALSE"),
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
#' @param values User supplied values for specific terms as a named list. If the value is \code{NULL}, the first value of the term is selected (useful when excluding terms).
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
#' # get predictions excluding x0 (the coefficient of x0 is set to 0);
#' # setting the value for the excluded term to NULL with the argument 'values'
#' # reduces computation time
#' p_2 <- predict_gam(model, exclude_terms = "s(x0)", values = list(x0 = NULL))
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

      if (is.null(new_term)) {
        new_term <- model[["var.summary"]][[term]][[1]]
      }

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
#' @param transform Function used to transform the fitted values (useful for getting plots on the response scale).
#' @param ci_z The z-value for calculating the CIs (the default is \code{1.96} for 95 percent CI).
#' @param .comparison Internal parameter, passed from plot_smooths().
#'
#' @examples
#' library(mgcv)
#' set.seed(10)
#' data <- gamSim(4)
#' model <- gam(y ~ fac + s(x2) + s(x2, by = fac) + s(x0), data = data)
#'
#' pred <- get_gam_predictions(model, x2)
#'
#' @export
get_gam_predictions <- function(model, series, series_length = 25, conditions = NULL, exclude_random = TRUE, exclude_terms = NULL, split = NULL, sep = "\\.", time_series, transform = NULL, ci_z = 1.96, .comparison = NULL) {
  if (!missing(time_series)) {
    warning("This argument has been deprecated and will be removed in the future. Please use `series` instead.")

    series_q = dplyr::enquo(time_series)
  } else {
    time_series = NULL
    series_q <- dplyr::enquo(series)
  }
  .comparison_q <- dplyr::enquo(.comparison)

  if (!is.null(model$dinfo) && (exclude_random || !is.null(exclude_terms))) {
    stop("Excluding random effects and/or terms is not currently supported with discretised models (fitted with discrete = TRUE). Please, set 'exclude_random' to FALSE and/or 'exclude_terms' to NULL.")
  }

  series_name <- rlang::quo_name(series_q)
  outcome_q <- model$formula[[2]]
  # Comparison and conditions terms are collected here, so that they are not
  # excluded when also part of random effects.
  cond_terms <- NULL
  if (!is.null(conditions)) {
    for (cond_i in 1:length(conditions)) {
      cond_term <- as.character(rlang::quo_get_expr(conditions[[cond_i]])[2])
      cond_terms <- c(cond_terms, cond_term)
    }
  }
  if (!(rlang::quo_is_null(.comparison_q))) {
    cond_terms <- c(cond_terms, rlang::as_name(.comparison_q))
  }

  random_effects <- list()
  random_effects_terms <- NULL
  re_term <- NULL

  # Get a list of the terms that are random effect
  if (exclude_random == TRUE) {
    for (i in 1:length(model[["smooth"]])) {
      smooth_term <- model[["smooth"]][[i]][["term"]][[1]]
      smooth_class <- attr(model$smooth[[i]],"class")[1]
      # If smooth term is one of those in conditions or the comparison
      # term of plot_smooths(), it should not be excluded
      if (smooth_class == "fs.interaction" && !(smooth_term %in% cond_terms)) {
        random_effects <- c(
          random_effects,
          list(model$smooth[[i]]$label)
        )
        random_effects_terms <- c(
          random_effects_terms,
          model$smooth[[i]]$fterm
        )
      }
      if (smooth_class == "random.effect" && !(smooth_term %in% cond_terms)) {
        random_effects <- c(
          random_effects,
          list(model[["smooth"]][[i]]$label)
        )
        random_effects_terms <- c(
          random_effects_terms,
          model$smooth[[i]]$term
        )
        # Used later to remove terms that were in s(bs = "re") from the
        # list of columns to remove, since they don't get their column in
        # the predicted tibble.
        re_term <- c(re_term, model$smooth[[i]]$term)
      }
    }
  }

  # Get a list of the smooth term labels that are random effects (like
  # 's(x0,fac)'), necessary in predict.gam() which takes the label of the
  # smooths as they appear in the summary output.
  if (exclude_random) {
    if (rlang::is_empty(random_effects)) {
      exclude_random_effects <- as.null()
    } else {
      exclude_random_effects <- random_effects
    }
  } else {
    exclude_random_effects <- as.null()
  }

  # Get smooth term labels which are not in the series, comparison, or conditions, or
  # terms in tensor smooths.
  exclude_smooths <- as.null()
  excluded_terms <- as.null()
  for (smooth in 1:length(model[["smooth"]])) {
    smooth_class <- attr(model$smooth[[smooth]],"class")[1]
    smooth_term <- model[["smooth"]][[smooth]][["term"]][[1]]
    # Do not include factor terms that are in s(bs = "re"), these are dealt
    # by the random effect code above. Not sure it's safe.
    if (smooth_class == "random.effect") {
      exclude_smooths <- exclude_smooths
      excluded_terms <- excluded_terms
    } else if (smooth_term != series_name && !(smooth_term %in% cond_terms)) {
      excluded_terms <- c(excluded_terms, smooth_term)
      smooth_label <- model[["smooth"]][[smooth]][["label"]]
      exclude_smooths <- c(exclude_smooths, smooth_label)
    } else if (smooth_class == "tensor.smooth") {
      smooth_term <- model[["smooth"]][[smooth]][["term"]][[2]]
      excluded_terms <- c(excluded_terms, smooth_term)
      smooth_label <- model[["smooth"]][[smooth]][["label"]]
      exclude_smooths <- c(exclude_smooths, smooth_label)
    }
  }

  # Get excluded terms
  excluded <- as.null()
  if (!is.null(exclude_terms)) {
    for (term in 1:length(exclude_terms)) {
      for (label in 1:length(model[["smooth"]])) {
        smooth_label <- model[["smooth"]][[label]][["label"]]
        if (smooth_label == exclude_terms[term]) {
          smooth_term <- model[["smooth"]][[label]][["term"]]
          if (!(smooth_term %in% cond_terms)) {
            if (length(smooth_term) > 1) {
              smooth_term_2 <- model[["smooth"]][[label]][["term"]][[2]]
              excluded <- c(excluded, smooth_term_2)
            }
          }
        }
      }
    }
  }

  exclude_these <- c(exclude_random_effects, exclude_smooths, exclude_terms)
  # Used for debugging.
  # print(exclude_random_effects)
  # print(exclude_smooths)

  # Prapare the new data frame for prediction. Only one value is selected
  # in excluded terms (the first level in factors, the minimum value in numeric
  # variables)
  var_list <- list()

  for (var in 1:length(model[["var.summary"]])) {
    var_class <- class(model[["var.summary"]][[var]])

    if (var_class == "numeric") {
      if (names(model[["var.summary"]][var]) %in% c(random_effects_terms, excluded_terms, excluded)) {
        var_values <- model[["var.summary"]][[var]][[1]]
      } else {
        var_values <- seq(model[["var.summary"]][[var]][[1]], model[["var.summary"]][[var]][[3]], length.out = series_length)
      }
    } else if (var_class == "factor") {
      if (names(model[["var.summary"]][var]) %in% c(random_effects_terms, excluded_terms, excluded)) {
        var_values <- model[["var.summary"]][[var]][[1]]
      } else {
        var_values <- levels(model[["var.summary"]][[var]])
      }
    }
    var_values_list <- list(var_values)
    names(var_values_list)[[1]] <- names(model[["var.summary"]][var])

    var_list <- c(var_list, var_values_list)
  }

  fitted_df <- expand.grid(var_list)

  if ("(AR.start)" %in% colnames(fitted_df)) {
    fitted_df$`(AR.start)` <- NULL
  }

  predicted <- stats::predict(
    model,
    fitted_df,
    se.fit = TRUE,
    exclude = exclude_these
  )

  predicted_tbl <- cbind(fitted_df, predicted) %>%
    dplyr::mutate(
      CI_upper = fit + ci_z * se.fit,
      CI_lower = fit - ci_z * se.fit
    )

  if (!is.null(transform)) {
    trans_fun <- transform
    predicted_tbl$CI_upper <- trans_fun(predicted_tbl$CI_upper)
    predicted_tbl$CI_lower <- trans_fun(predicted_tbl$CI_lower)
    predicted_tbl$fit <- trans_fun(predicted_tbl$fit)
  }

  predicted_tbl <- predicted_tbl %>%
    dplyr::rename(
      # quo_name is needed for terms that are non-syntactic column names like
      # `log(x)`
      !!rlang::quo_name(outcome_q) := fit,
      SE = se.fit
    )

  # Get rid of terms that were in s(bs = "re") to avoid error from one_off(NULL)
  if (!is.null(exclude_random_effects)) {
    for (term in 1:length(random_effects_terms)) {
      if (random_effects_terms[term] %in% re_term) {
        random_effects_terms <- random_effects_terms[-term]
      }
    }
    predicted_tbl <- predicted_tbl %>%
      dplyr::select(-dplyr::one_of(random_effects_terms)) %>%
      unique()
  }

  if (!is.null(exclude_smooths)) {
    for (term in 1:length(excluded_terms)) {
      if (excluded_terms[term] %in% re_term) {
        excluded_terms <- excluded_terms[-term]
      }
    }
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

  if (ncol(predicted_tbl) > 5) {
    predicted_tbl <- tidyr::unite(
      predicted_tbl,
      ".idx",
      c(-CI_lower, -CI_upper, -SE, -!!rlang::quo_name(outcome_q), -!!rlang::quo_name(series_q)),
      remove = FALSE
    ) %>%
      dplyr::mutate(.idx = as.numeric(as.factor(.idx)))
  }

  return(predicted_tbl)
}
