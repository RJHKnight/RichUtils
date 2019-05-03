#' Get the previous weekday
#'
#' @param date - the date to offset
#' @return The previous weekday
#' @examples
#' getPreviousWeekday(today())
#' #' @export
getPreviousWeekday <- function(date) {

  dow <- wday(date, week_start = 6)
  offset <- if_else(dow > 3, 1, dow)

  return (date - offset)
}

#' Get the next weekday
#'
#' @param date - the date to offset
#' @return The next weekday
#' @examples
#' getNextWeekday(today())
#' #' @export
getNextWeekday <- function(date) {

  dow <- wday(date, week_start = 5)
  offset <- if_else(dow > 3, 1, abs(dow-4))

  return (date + offset)
}
