get_file_logger <- function()
{
  if (! exists("file_logger"))
  {
    # File name
    log_location <- config::get("log_file_location")
    date_string <- format(today(), "%Y-%m-%d")
    log_name <- paste0(config::get("log_file_name"), "_", date_string, ".log")
    file_name <- paste(log_location, log_name, sep = .Platform$file.sep)

    # Create logger
    log_level <- config::get("log_level")
    file_logger <- log4r::logger(log_level, appenders = log4r::file_appender(file_name, append = TRUE))
  }

  return (file_logger)
}

#' Info level logging to file.
#'
#' @export
log_info <- function(...)
{
  logger <- get_file_logger()
  log4r::info(logger, paste(list(...), collapse = " "))
}

#' Debug level logging to file.
#'
#' @export
log_debug <- function(...)
{
  logger <- get_file_logger()
  log4r::debug(logger, paste(list(...), collapse = " "))
}

#' Warn level logging to file.
#'
#' @export
log_warn <- function(...)
{
  logger <- get_file_logger()
  log4r::warn(logger, paste(list(...), collapse = " "))
}

#' Error level logging to file.
#'
#' @export
log_error <- function(...)
{
  logger <- get_file_logger()
  log4r::error(logger, paste(list(...), collapse = " "))
}
