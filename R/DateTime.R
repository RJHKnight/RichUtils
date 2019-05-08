#' Get the previous weekday
#'
#' @param date - the date to offset
#' @return The previous weekday
#' @examples
#' getPreviousWeekday(today())
#' @export
getPreviousWeekday <- function(date) {

  date <- handleDate(date)
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

  date <- handleDate(date)
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

  date <- handleDate(date)

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

  date <- handleDate(date)

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

  date <- handleDate(date)

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

  date <- handleDate(date)
  return (lubridate::wday(date, week_start = 1) < 6)
}


handleDate <- function(date) {

  if (any(class(date) == "Date") | any(class(date) == "POSIXct")) {
    return (date)
  }

  tryCatch(
   lubridate::ymd(date),
   warning = function(w) return (lubridate::ymd_hms(date)),
   return (lubridate::ymd(date))
  )
}

#' Get all the week days between two points
#'
#' @param startDate - start date
#' @param endDate - end date
#' @return A list of Dates
#' @examples
#' getWeekdays("2019.01.01", "2019.01.05")
#' @export
getWeekdays <- function(startDate, endDate) {

  startDate <- handleDate(startDate)
  endDate <- handleDate(endDate)

  allDates <- seq(from = startDate, to = endDate, by = "day")

  return (allDates[isWeekday(allDates)])
}

#' Convert a date into a Tic compatable string
#'
#' @param date - date to convert
#' @return The string to pass to tic
#' @examples
#' toTicFormat(ymd("2019-01-01"))
#' @export
toTicFormat <- function(date) {

  ticFormat <- "%Y.%m.%d"
  return (format(date, ticFormat))
}

#' Create a date from a Tic compatable string
#'
#' @param date - date string to convert
#' @return The parsed date
#' @examples
#' fromTicFormat("2019.01.01")
#' @export
fromTicFormat <- function(date) {

  return (lubridate::ymd(date))
}
