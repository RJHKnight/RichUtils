
#' Prepare the input file for a batch of simulations
#'
#' @param syms - the names to simulate
#' @param startDate - the start date
#' @param endDate - the end date
#' @param strategies - the list of strategies to simulate
#' @param side - the sides to simulate
#' @param pasteToClipboard - paste the results to the clipboard or return as a data frame.
#' @param ... - extra parameters to set on the simulation
#'
#' @return The resultant simulations (if pasteToClipboard is FALSE)
#'
#' @examples
#' getSimsToLoad(c("BHP.AX","RIO.AX"), "2019.05.01", "2019.05.18", "VWAP", urgency = "HIGH")
#' @export
getSimsToLoad <- function(syms, startDate, endDate, strategies, sides = c("BUY", "SELL"), pasteToClipboard = TRUE, ...) {

  dates <- getWeekdays(startDate, endDate)
  baseParams = data.frame(expand.grid(sym = syms, date = dates, side = sides, strategy = strategies, stringsAsFactors = FALSE))

  extraParams <- as.data.frame(list(...))

  result <- NULL

  if (length(extraParams) != 0) {



    baseParams <- cbind(baseParams, extraParams)
  }

  if (pasteToClipboard) {
    clipr::write_clip(baseParams, col.names = FALSE)
  }

  else {
    return (baseParams)
  }
}
