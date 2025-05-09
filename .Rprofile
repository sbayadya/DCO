if (interactive()) {
  conflicting_functions <- c(
    "convert_dates_to_ymd", "execute_dco_rules", "filter_by_date",
    "make_date_na", "perform_dco", "update_var"
  )
  existing <- conflicting_functions[conflicting_functions %in% ls(envir = globalenv())]
  if (length(existing) > 0) {
    rm(list = existing, envir = globalenv())
    message("<U+26A0><U+FE0F>  Removed conflicting functions from global environment.")
  }
}
