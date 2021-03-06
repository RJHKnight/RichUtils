% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimLoader.R
\name{getSimsToLoad}
\alias{getSimsToLoad}
\title{Prepare the input file for a batch of simulations}
\usage{
getSimsToLoad(syms, startDate, endDate, strategies, sides = c("BUY",
  "SELL"), pasteToClipboard = TRUE, ...)
}
\arguments{
\item{syms}{- the names to simulate}

\item{startDate}{- the start date}

\item{endDate}{- the end date}

\item{strategies}{- the list of strategies to simulate}

\item{pasteToClipboard}{- paste the results to the clipboard or return as a data frame.}

\item{...}{- extra parameters to set on the simulation}

\item{side}{- the sides to simulate}
}
\value{
The resultant simulations (if pasteToClipboard is FALSE)
}
\description{
Prepare the input file for a batch of simulations
}
\examples{
getSimsToLoad(c("BHP.AX","RIO.AX"), "2019.05.01", "2019.05.18", "VWAP", urgency = "HIGH")
}
