#' Get model predictions for differences between conditions.
#'
#' @import mgcv
#' @import stats
#' @param model A gam object, produced by \code{\link[mgcv]{gam}} or
#' \code{\link[mgcv]{bam}}.
#' @param comp A named list with the two levels to compare.
#' @param cond A named list of the values to use for the other predictor
#' terms. Variables omitted from this list will have the closest observed
#' value to the median for continuous variables, or the reference level for
#' factors.
#' @param rm.ranef Logical: whether or not to remove random effects.
#' Default is TRUE. Alternatively a vector of numbers with the
#' mdoelterm number of the random effect(s) to remove.
#' (See notes.)
#' @param se Logical: whether or not to return the confidence interval or
#' standard error around the estimates.
#' @param sim.ci Logical: Using simultaneous confidence intervals or not
#' (default set to FALSE). The implementation of simultaneous CIs follows
#' Gavin Simpson's blog of December 15, 2016:
#' \url{https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/}.
#' This interval is calculated from simulations based.
#' Please specify a seed (e.g., \code{set.seed(123)}) for reproducable results.
#' In addition, make sure to specify at least 200 points for each smooth
#' for the simulations when using simultaneous CI.
#' Note: in contrast with Gavin Simpson's code, here the Bayesian posterior
#' covariance matrix of the parameters is uncertainty corrected
#' (\code{unconditional=TRUE}) to reflect the uncertainty on the estimation of
#' smoothness parameters.
#' @param f A number to scale the standard error. Defaults to 1.96, resulting
#' in 95\% confidence intervals. For 99\% confidence intervals use a value of
#' 2.58.
#' @param return.n.posterior Numeric: N samples from
#' the posterior distribution of the fitted model are returned.
#' Default value is 0 (no samples returned).
#' Only workes when \code{sim.ci=TRUE}.
#' @param print.summary Logical: whether or not to print a summary of the
#' values selected for each predictor.
#' Default set to the print info messages option
#' (see \code{infoMessages}).
#' @return Returns a data frame with the estimates of the difference and
#' optionally the confidence intervals around that estimate.
#' @section Notes:
#' Other, not specified effects and random effects are generally canceled
#' out, when calculating the difference. When the predictors that
#' specify the conditions to compare are involved in other interactions
#' or included as random slopes, it may be useful to specify the values
#' of other predictors with \code{cond} or remove the random effects with
#' \code{rm.ranef}.
#' @author Jacolien van Rij, Martijn Wieling
#' @export
#' @keywords internal
#' @importFrom magrittr "%>%"
get_difference <- function(model, comp, cond = NULL, rm.ranef = TRUE, se = TRUE, sim.ci = FALSE, f = 1.96,
                           return.n.posterior = 0, print.summary = FALSE) {
  if (!"lm" %in% class(model)) {
    stop("This function does not work for class %s models.", class(model)[1])
  } else {
    newd <- NULL
    su <- model$var.summary
    dat <- model$model
    # check comp
    if (is.null(names(comp))) {
      stop("Predictor specified in 'comp' unknown. Please provide a named list for 'comp', in the form of 'comp=list(Predictor=c('level1', 'level2'))'.")
    }
    if (all(names(comp) %in% colnames(dat))) {
      for (i in 1:length(comp)) {
        if (length(comp[[i]]) < 2) {
          stop(sprintf("Provide two levels for %s to calculate difference.", names(comp)[i]))
        } else if (length(comp[[i]]) > 2) {
          warning(sprintf("More than two levels provided for predictor %s. Only first two levels are being used.",
                          names(comp)[i]))
        }
      }
    } else {
      errname <- paste(which(!names(comp) %in% colnames(dat)), collapse = ", ")
      stop(sprintf("Grouping predictor(s) not found in model: %s.", errname))
    }
    if (any(names(cond) %in% names(comp))) {
      for (i in names(cond)[names(cond) %in% names(comp)]) {
        cond[[i]] <- NULL
        warning(sprintf("Predictor %s specified in comp and cond. (The value in cond will be ignored.)",
                        i))
      }
    }
    new.cond1 <- list()
    new.cond2 <- list()
    for (i in names(su)) {
      if (i %in% names(comp)) {
        new.cond1[[i]] <- comp[[i]][1]
        new.cond2[[i]] <- comp[[i]][2]
      } else if (i %in% names(cond)) {
        new.cond1[[i]] <- new.cond2[[i]] <- cond[[i]]
      } else {
        if (class(su[[i]]) == "factor") {
          new.cond1[[i]] <- as.character(su[[i]][1])
          new.cond2[[i]] <- as.character(su[[i]][1])
        } else if (class(su[[i]]) == "numeric") {
          new.cond1[[i]] <- su[[i]][2]
          new.cond2[[i]] <- su[[i]][2]
        }
      }
    }
    newd1 <- expand.grid(new.cond1)
    newd2 <- expand.grid(new.cond2)
    p1 <- mgcv::predict.gam(model, newd1, type = "lpmatrix")
    p2 <- mgcv::predict.gam(model, newd2, type = "lpmatrix")
    newd <- as.data.frame(newd1, stringsAsFactors = TRUE)
    newd.names <- colnames(newd)
    for (nn in newd.names) {
      if (nn %in% names(comp)) {
        newd[, nn] <- NULL
      }

    }
    mysummary <- summary_data(newd, print = FALSE)
    # Check for random effects:
    if (class(rm.ranef) == "logical") {
      if (rm.ranef[1] == FALSE) {
        rm.ranef <- NULL
      }
    }
    if (!is.null(rm.ranef)) {
      # get random effects columns:
      smoothlabels.table <- as.data.frame(do.call("rbind", lapply(model$smooth, function(x) {
        data.frame(Label = x[["label"]], Dim = x[["null.space.dim"]], Class = attr(x, "class")[1],
                   stringsAsFactors = FALSE)
      })), stringsAsFactors = FALSE)
      # smoothlabels <- as.vector( smoothlabels.table[smoothlabels.table$Class %in%
      # c('random.effect','fs.interaction'), 'Label'] )
      smoothlabels <- as.vector(smoothlabels.table[smoothlabels.table$Dim == 0, "Label"])
      if (class(rm.ranef) == "logical") {
        if (rm.ranef[1] == TRUE) {
          rm.ranef <- smoothlabels
        } else {
          rm.ranef <- ""
        }
      } else if (inherits(rm.ranef, c("numeric", "integer"))) {
        smoothlabels.table <- smoothlabels.table[rm.ranef, ]
        smoothlabels <- as.vector(smoothlabels.table[smoothlabels.table$Class %in% c("random.effect",
                                                                                     "fs.interaction"), "Label"])
      }
      rm.col <- unlist(lapply(rm.ranef, function(x) {
        colnames(p1)[grepl(x, colnames(p1), fixed = TRUE)]
      }))
      rm.col <- unlist(lapply(smoothlabels, function(x) {
        rm.col[grepl(x, rm.col, fixed = TRUE)]
      }))
      # cancel random effects
      p1[, rm.col] <- 0
      p2[, rm.col] <- 0
      # find terms that only occur in random effects:
      predictors <- do.call("rbind", lapply(model$smooth, function(x) {
        data.frame(Label = x[["label"]], Terms = x[["term"]], stringsAsFactors = TRUE)
      }))
      test <- table(predictors$Terms) - table(predictors[predictors$Label %in% rm.ranef, ]$Terms)
      for (pred in names(test[test == 0])) {
        if (pred %in% names(mysummary)) {
          mysummary[[pred]] <- paste(mysummary[[pred]], "(Might be canceled as random effect, check below.)")
        }
      }
      if (length(rm.col) > 0) {
        mysummary[["NOTE"]] = sprintf("The following random effects columns are canceled: %s\n", paste(smoothlabels,
                                                                                                       collapse = ","))
      } else {
        mysummary[["NOTE"]] = "No random effects in the model to cancel.\n"
        # warning('No random effects to cancel.\n')
      }
    }
    # calculate the difference:
    p <- p1 - p2
    newd$difference <- as.vector(p %*% coef(model))
    if (se) {
      newd$CI <- f * sqrt(rowSums((p %*% vcov(model)) * p))
    }
    # simultaneous CI See http://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
    stackFits <- NULL
    if (sim.ci == TRUE) {
      Vb <- vcov(model, freq = FALSE, unconditional = TRUE)
      se.fit <- sqrt(rowSums((p %*% Vb) * p))
      sim <- mgcv::rmvn(10000, mu = rep(0, nrow(Vb)), V = Vb)
      # Cg <- predict(model, newd, type='lpmatrix') Cg replaced by p
      simDev <- p %*% t(sim)
      # Evaluate the basis function at g and compute the deviations between the fitted and true parameters. Then
      # we find the absolute values of the standardized deviations from the true model:
      absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
      # maximum of the absolute standardized deviations:
      masd <- apply(absDev, 2L, max)
      # set simultaneous X% CI on the basis of original se multiplication specified by f:
      crit <- quantile(masd, prob = 1 - round(2 * (1 - pnorm(f)), 2), type = 8)
      newd$sim.CI <- crit * se.fit

      if ((return.n.posterior > 0) | (print.summary == TRUE)) {
        # coverage pointwise and simultaneous CIs:
        sims <- rmvn(max(10000, return.n.posterior), mu = coef(model), V = Vb)
        fits <- p %*% t(sims)
        inCI <- function(x, upr, lwr) {
          all(x >= lwr & x <= upr)
        }
        fitsInPCI <- apply(fits, 2L, inCI, upr = newd$fit + newd$CI, lwr = newd$fit - newd$CI)
        fitsInSCI <- apply(fits, 2L, inCI, upr = newd$fit + newd$sim.CI, lwr = newd$fit - newd$sim.CI)
        mysummary[[paste("Simultaneous ", 100 * (1 - round(2 * (1 - pnorm(f)), 2)), "%-CI used", sep = "")]] = sprintf("\n\t\t%s\n\t\t%s\n\t\t%s\n",
                                                                                                                       paste("Critical value: ", round(crit, 3), sep = ""), paste("Proportion posterior simulations in pointwise CI: ",
                                                                                                                                                                                  round(sum(fitsInPCI)/length(fitsInPCI), 2), " (10000 samples)", sep = ""), paste("Proportion posterior simulations in simultaneous CI: ",
                                                                                                                                                                                                                                                                   round(sum(fitsInSCI)/length(fitsInSCI), 2), " (10000 samples)", sep = ""))

        if (return.n.posterior > 0) {
          rnd <- sample(max(10000, return.n.posterior), return.n.posterior)
          fits <- utils::stack(as.data.frame(fits[, rnd], stringsAsFactors = FALSE))[, 1]
          stackFits <- newd[rep(1:nrow(newd), length(rnd)), colnames(newd)[!colnames(newd) %in% c("fit",
                                                                                                  "CI", "sim.CI", "rm.ranef")]]
          row.names(stackFits) <- NULL
          stackFits$posterior.fit <- fits
          stackFits$draw <- rep(rnd, each = nrow(newd))
        }
      }
    }
    # print summary of chosen values
    # if (print.summary == TRUE) {
    #   print_summary(mysummary)
    # }

    if (return.n.posterior > 0) {
      return(list(newd = newd, posterior.fit = stackFits))
    } else {
      return(newd)
    }
  }
}


#' Return the regions in which the smooth is significantly different from zero.
#'
#' @import grDevices
#' @import graphics
#' @param mean A vector with smooth predictions.
#' @param se A vector with the standard error on the smooth predictions.
#' @param xVals Optional vector with x values for the smooth.
#' When \code{xVals} is provided, the regions are returned in terms of x-
#' values, otherwise as indices.
#' @param f A number to multiply the \code{se} with, to convert the \code{se}
#' into confidence intervals. Use 1.96 for 95\% CI and 2.58 for 99\%CI.
#' @param as.vector Logical: whether or not to return the data points as
#' vector, or not. Default is FALSE, and a list with start and end points will
#'  be returned.
#' @return The function returns a list with start points of each region
#' (\code{start}) and end points of each region (\code{end}). The logical
#' \code{xVals} indicates whether the returned values are on the x-scale
#' (TRUE) or indices (FALSE).
#' @author Jacolien van Rij
#' @keywords internal
#' @export
find_difference <- function(mean, se, xVals = NULL, f = 1, as.vector = FALSE) {
  if (length(mean) != length(se)) {
    stop("The vectors mean and se are not equal in length.")
  } else {
    ub <- mean + f * se
    lb <- mean - f * se

    n <- which(!(ub >= 0 & lb <= 0))
    if (as.vector) {
      if (length(n) == 0) {
        return(rep(FALSE, length(mean)))
      } else {
        out <- rep(FALSE, length(mean))
        out[n] <- TRUE
        return(out)
      }
    } else {
      if (length(n) == 0) {
        return(NULL)
      } else {
        n_prev <- c(NA, n[1:(length(n) - 1)])
        n_next <- c(n[2:length(n)], NA)
        if (!is.null(xVals) & (length(xVals) == length(mean))) {
          return(list(start = xVals[n[which(is.na(n - n_prev) | (n - n_prev) > 1)]], end = xVals[n[which(is.na(n_next -
                                                                                                                 n) | (n_next - n) > 1)]], xVals = TRUE))
        } else {
          return(list(start = n[which(is.na(n - n_prev) | (n - n_prev) > 1)], end = n[which(is.na(n_next -
                                                                                                    n) | (n_next - n) > 1)], xVals = FALSE))
        }

      }
    }
  }
}

#' Print a descriptive summary of a data frame.
#'
#' @export
#' @keywords internal
#' @description The function prints a summary of the data.
#' Similar to the function \code{\link[utils]{str}}, but easier readable.
#' @param data A data frame.
#' @param print Logical: whether or not to print the summary.
#' @param n Number: maximum number of values being mentioned in the summary.
#' If NULL all values are being mentioned. Defaults to 10.
#' @return Optionally returns a named list with info.
#' @author Jacolien van Rij
summary_data <- function(data, print = TRUE, n = 10) {

  labelColumns <- function(x, data) {
    out <- NULL
    cn <- ifelse(is.numeric(x), colnames(data)[x], x)
    cl <- class(data[, cn])
    if (inherits(data[, cn], "factor")) {
      vals <- sort(unique(as.character(data[, x])))
      n.cur <- length(vals) + 1
      if (!is.null(n)) {
        n.cur <- n
      }
      if (length(vals) > n.cur) {
        out <- sprintf("factor with %d values; set to the value(s): %s, ...", length(vals), paste(vals[1:n.cur],
                                                                                                  collapse = ", "))
      } else {
        out <- sprintf("factor; set to the value(s): %s.", paste(vals, collapse = ", "))
      }
    } else if (inherits(data[, cn], "numeric")) {
      if (length(unique(data[, x])) > 2) {
        out <- sprintf("numeric predictor; with %d values ranging from %f to %f.", length(unique(data[,
                                                                                                      x])), min(data[, x], na.rm = TRUE), max(data[, x], na.rm = TRUE))
      } else {
        out <- sprintf("numeric predictor; set to the value(s): %s.", paste(unique(data[, x]), collapse = ", "))
      }
    } else if (inherits(data[, cn], "matrix")) {
      if (length(unique(data[, x])) > 2) {
        out <- sprintf("a matrix predictor; with %d values ranging from %f to %f.", length(unique(data[,
                                                                                                       x])), min(data[, x], na.rm = TRUE), max(data[, x], na.rm = TRUE))
      } else {
        out <- sprintf("matrix predictor; set to the value(s): %s.", paste(unique(data[, x]), collapse = ", "))
      }
    } else {
      vals <- sort(unique(data[, x]))
      n.cur <- length(vals) + 1
      if (!is.null(n)) {
        n.cur <- n
      }
      if (length(vals) > n.cur) {
        out <- sprintf("%s vector with %d values; set to the value(s): %s, ...", class(data[, cn])[1],
                       length(vals), paste(vals[1:n.cur], collapse = ", "))
      } else {
        out <- sprintf("%s vector; set to the value(s): %s.", class(data[, cn])[1], paste(vals, collapse = ", "))
      }
    }
    return(out)
  }
  mysummary <- sapply(colnames(data), function(x) {
    labelColumns(x, data)
  })
  # if (print) {
  #   print_summary(mysummary)
  # }
  invisible(mysummary)
}
