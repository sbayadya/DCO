#' Convert Dates to Standardized Format Internally (No Imputation)
#'
#' @description Parses a variety of date formats, including partial dates (e.g., "YYYY-MM", "YYYY"),
#' and returns the original date string unchanged. Used for internal filtering/comparison only, not for imputation.
#'
#' @param date_str A character vector of date strings in various formats.
#'
#' @return A character vector of original date strings if partial; otherwise, returns a cleaned YMD or YMD HMS string.
#'
#' @importFrom lubridate parse_date_time year month
#' @export
convert_dates_to_ymd <- function(date_str) {
  parse_attempt <- function(x) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x,
      orders = c(
        "Ymd HMS", "Ymd HM", "Ymd",         # full date and datetime
        "Y-m", "Ym", "my", "mY",            # partial: year-month
        "Y",                                # year only
        "mdy HMS", "mdy HM", "mdy",         # US formats
        "dmy HMS", "dmy HM", "dmy",         # EU formats
        "dbY", "dbY HMS", "dbY HM"          # textual dates
      ),
      exact = FALSE
    ))

    if (is.na(parsed)) {
      return(NA_character_)
    }

    # If parsed day is 01 and original string lacks a day part, assume it's partial and return original
    if (format(parsed, "%d") == "01" && !grepl("\\b\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}\\b", x)) {
      return(x)
    }

    # Return with time if present in original string
    if (grepl("\\d{2}:\\d{2}", x)) {
      return(format(parsed, "%Y-%m-%d %H:%M:%S"))
    }

    return(format(parsed, "%Y-%m-%d"))
  }

  vapply(date_str, parse_attempt, character(1))
}
