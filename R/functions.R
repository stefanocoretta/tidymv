#' Create a start event column
#'
#' Create a new column which marks the beginning of each time series in a tibble.
#' The effect is the same as \code{start_event} from \code{itsadug}, but it works
#' with tibbles.
#'
#' @param tibble A tibble arranged according to the time series.
#' @param event.col A string with the name of the column that defines the time series.
#' @export
create_event_start <- function(tibble, event.col) {
    event <- as.character(tibble[[event.col]])
    previous <- ""
    event.start <- NULL

    for (i in 1:length(event)) {
        current <- event[i]
        if (current == previous) {
            event.start.current <- FALSE
        } else {
            event.start.current <- TRUE
        }

        previous <- current

        event.start <- c(event.start, event.start.current)
    }

    dplyr::mutate(tibble, start.event = event.start)
}

