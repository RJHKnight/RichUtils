#' Get the previous weekday
#'
#' @param date - the date to offset
#' @return The previous weekday
#' @examples
#' getPreviousWeekday(today())
#' @export
getPreviousWeekday <- function(date) {

  dow <- lubridate::wday(date, week_start = 6)
  offset <- if_else(dow > 3, 1, dow)

  return (date - offset)
}

#' Get the next weekday
#'
#' @param date - the date to offset
#' @return The next weekday
#' @examples
#' getNextWeekday(today())
#' @export
getNextWeekday <- function(date) {

  dow <- lubridate::wday(date, week_start = 5)
  offset <- if_else(dow > 3, 1, abs(dow-4))

  return (date + offset)
}

#' Return the nth weekday from now
#'
#' @param date - the date to offset
#' @param  n - the number of days to offset (a negative n means reverse the meaning of the forward parameter)
#' @param  forward - forward or backward offset
#' @return The resultant weekday
#' @examples
#' offsetByWeekdays(today(), 4, forward = FALSE)
#' offsetByWeekdays(today(), -4)
#' @export
offsetByWeekdays <- function(date, n, forward = TRUE) {

  # Handle negative n
  if (n < 0) {
    n <- -1 * n
    forward <- !forward
  }

  dates <- .iterateOverWeekdays(date, n, forward)

  return (tail(dates,1))
}


#' Return a list of the n adjacent weekdays
#'
#' @param date - the date to offset
#' @param  n - the number of days to offset (a negative n means reverse the meaning of the forward parameter)
#' @param  forward - forward or backward offset
#' @return The next weekday(s)
#' @examples
#' getAdjacentWeekdays(today(), 4, forward = FALSE)
#' getAdjacentWeekdays(today(), -4)
#' @export
getAdjacentWeekdays <- function(date, n, forward = TRUE) {

  # Handle negative n
  if (n < 0) {
    n <- -1 * n
    forward <- !forward
  }

  dates <- .iterateOverWeekdays(date, n, forward)

  return (dates[-1])
}

# Internal method to do the actual iteration.
.iterateOverWeekdays <- function(date, n, forward = TRUE) {

  if (n == 0)
    return (date)

  newDate <- ifelse(forward, getNextWeekday(tail(date,1)), getPreviousWeekday(tail(date, 1)))

  return (c(date, .iterateOverWeekdays(newDate, n-1, forward)))
}

#' Check if a date falls on a weekend
#'
#' @param date - the date(s) to test
#' @return A boolean (or list of booleans)
#' @examples
#' isWeekend(c(ymd("20190505"), ymd("20190506")))
#' @export
isWeekend <- function(date) {
  return (!isWeekday(date))
}

#' Check if a date falls on a weekday
#'
#' @param date - the date(s) to test
#' @return A boolean (or list of booleans)
#' @examples
#' isWeekday(c(ymd("20190505"), ymd("20190506")))
#' @export
isWeekday <- function(date) {
  return (lubridate::wday(date, week_start = 1) < 6)
}
