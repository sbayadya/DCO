#' Filter a Data Frame by Date
#'
#' @description This function filters a data frame, keeping only the rows where the date in a specified column
#' is before a given cut-off date.
#'
#' @param .data A data frame to filter.
#' @param date_var The name of the date column to filter by (unquoted).
#' @param cutoff_date The cut-off date. Rows with dates before this date will be kept.
#'
#' @return A filtered data frame containing only rows where the date in the specified column
#' is before the cut-off date.
#'
#' @importFrom dplyr mutate filter select
#' @importFrom rlang ensym as_string
#' @importFrom lubridate parse_date_time
#'
#' @export
filter_by_date <- function(.data, date_var, cutoff_date) {
  date_sym <- rlang::ensym(date_var)
  col_name <- rlang::as_string(date_sym)

  .data <- dplyr::mutate(.data,
                         .safe_date = ifelse(trimws(.data[[col_name]]) == "", NA, .data[[col_name]]),
                         .parsed_date = suppressWarnings(lubridate::parse_date_time(
                           .safe_date,
                           orders = c(
                             "Ymd HMS", "Ymd HM", "Ymd",
                             "Y-m", "Ym", "my", "mY", "Y",
                             "mdy HMS", "mdy HM", "mdy",
                             "dmy HMS", "dmy HM", "dmy",
                             "dbY", "dbY HMS", "dbY HM"
                           ),
                           exact = FALSE
                         ))
  )

  filtered_data <- dplyr::filter(.data,
                                 is.na(.parsed_date) |
                                   .parsed_date <= as.Date(cutoff_date)
  )

  filtered_data <- dplyr::select(filtered_data, - .parsed_date, - .safe_date)

  return(filtered_data)
}
