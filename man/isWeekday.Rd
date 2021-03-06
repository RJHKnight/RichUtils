% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DateTime.R
\name{isWeekday}
\alias{isWeekday}
\title{Check if a date falls on a weekday}
\usage{
isWeekday(date)
}
\arguments{
\item{date}{- the date(s) to test}
}
\value{
A boolean (or list of booleans)
}
\description{
Check if a date falls on a weekday
}
\examples{
isWeekday(c(ymd("20190505"), ymd("20190506")))
}
