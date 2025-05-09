#' Update Variable in Data Frame Based on Date Logic
#'
#' @description Updates the values of an outcome column in a data frame if the end date is after the cut-off date,
#' but the cut-off date is still after the start date. Handles partial or inconsistent date formats automatically.
#'
#' @param .data A data frame to update.
#' @param start_date The name of the start date column (unquoted).
#' @param end_date The name of the end date column (unquoted).
#' @param update_var The name of the column whose values should be updated (unquoted).
#' @param cutoff_date The cut-off date (Date object or string in "YYYY-MM-DD" format).
#'
#' @return An updated data frame where values in `update_var` are changed to "Not Recovered/Not Resolved"
#' if `end_date > cutoff_date > start_date`.
#'
#' @importFrom rlang ensym as_name
#' @importFrom dplyr mutate if_else
#' @importFrom lubridate parse_date_time
#' @export
update_var <- function(.data, start_date, end_date, update_var, cutoff_date) {
  start_sym  <- ensym(start_date)
  end_sym    <- ensym(end_date)
  update_sym <- ensym(update_var)

  start_col  <- as_name(start_sym)
  end_col    <- as_name(end_sym)
  update_col <- as_name(update_sym)

  # Validate input columns exist
  missing_cols <- setdiff(c(start_col, end_col, update_col), names(.data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure cutoff_date is Date
  cutoff_date <- as.Date(cutoff_date)

  # Define supported date formats
  date_orders <- c(
    "Ymd HMS", "Ymd HM", "Ymd",
    "Y-m", "Ym", "my", "mY",
    "Y",
    "mdy HMS", "mdy HM", "mdy",
    "dmy HMS", "dmy HM", "dmy",
    "dbY", "dbY HMS", "dbY HM"
  )

  updated_data <- .data %>%
    mutate(
      .start_parsed = suppressWarnings(lubridate::parse_date_time(.data[[start_col]], orders = date_orders)),
      .end_parsed   = suppressWarnings(lubridate::parse_date_time(.data[[end_col]], orders = date_orders)),
      !!update_sym := if_else(
        !is.na(.end_parsed) & .end_parsed > cutoff_date & cutoff_date > .start_parsed,
        "Not Recovered/Not Resolved",
        .data[[update_col]]
      )
    ) %>%
    select(-.start_parsed, -.end_parsed)

  return(updated_data)
}
