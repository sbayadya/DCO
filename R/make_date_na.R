#' Update End Date in Data Frame
#'
#' @description This function keeps the end date in the specified column if it is before the cut-off date.
#'
#' @param .data A data frame to update.
#' @param start_date The name of the start date column (unquoted).
#' @param end_date The name of the end date column (unquoted).
#' @param cutoff_date The cut-off date in format "YYYY-MM-DD". Rows with dates before this date will be kept.
#'
#' @return An updated data frame where `end_date` values are set to `NA` if the `end_date` is greater than the cut-off date
#' and the cut-off date is greater than the `start_date`.
#'
#' @importFrom dplyr mutate if_else select
#' @importFrom rlang ensym as_name
#' @importFrom lubridate parse_date_time
#'
#' @export
make_date_na <- function(.data, start_date, end_date, cutoff_date) {
  start_sym <- rlang::ensym(start_date)
  end_sym   <- rlang::ensym(end_date)

  start_col <- rlang::as_name(start_sym)
  end_col   <- rlang::as_name(end_sym)

  cutoff_date <- as.Date(cutoff_date)

  updated_data <- dplyr::mutate(.data,
                                .start_parsed = suppressWarnings(lubridate::parse_date_time(.data[[start_col]], orders = c(
                                  "Ymd HMS", "Ymd HM", "Ymd",
                                  "Y-m", "Ym", "my", "mY", "Y",
                                  "mdy HMS", "mdy HM", "mdy",
                                  "dmy HMS", "dmy HM", "dmy",
                                  "dbY", "dbY HMS", "dbY HM"
                                ))),
                                .end_parsed = suppressWarnings(lubridate::parse_date_time(.data[[end_col]], orders = c(
                                  "Ymd HMS", "Ymd HM", "Ymd",
                                  "Y-m", "Ym", "my", "mY", "Y",
                                  "mdy HMS", "mdy HM", "mdy",
                                  "dmy HMS", "dmy HM", "dmy",
                                  "dbY", "dbY HMS", "dbY HM"
                                ))),
                                !!end_sym := dplyr::if_else(
                                  !is.na(.end_parsed) & .end_parsed > cutoff_date & cutoff_date > .start_parsed,
                                  NA_character_,
                                  as.character(.data[[end_col]])
                                )
  )

  updated_data <- dplyr::select(updated_data, - .start_parsed, - .end_parsed)

  return(updated_data)
}
