#' Apply DCO Methods from YAML File to Data Frames
#'
#' @description Reads a YAML configuration file containing method instructions
#' and applies the specified methods to data frames stored in a given directory.
#'
#' @param config_path Character. Path to the YAML configuration file defining rules and variables.
#' @param data_path Character. Path to the directory where the data files are stored.
#' @param out_path Character. Path to the directory where the output files should be saved.
#' @param out_file_suffix Character. The suffix to add to the output file names.
#' @param dco Date or character. The data cut-off date (in yyyy-mm-dd format).
#'
#' @return This function does not return a value. It saves processed datasets to disk.
#'
#' @importFrom dplyr filter sym
#' @importFrom yaml read_yaml
#' @importFrom lubridate ymd
#'
#' @export
execute_dco_rules <- function(config_path, data_path, out_path, out_file_suffix = "_filtered.rds", dco) {
  if (nchar(dco) != 10) {
    stop("Invalid DCO date format. Expected yyyy-mm-dd.")
  }

  # Ensure DCO is in Date format
  dco <- as.Date(dco)

  # Read the YAML file into a list
  config <- yaml::read_yaml(config_path)

  # Extract method instructions and date variables
  method_df <- config$dco_rules
  dat_variable_list <- config$dat_variable_list
  data_names <- names(method_df)

  # Process each dataset
  for (dataset_name in data_names) {
    method_list <- method_df[[dataset_name]]

    # Load the data frame
    data_file_path <- file.path(data_path, paste0(dataset_name, ".rds"))
    data <- readRDS(data_file_path)

    # Apply each method defined for the dataset
    for (rule in method_list) {
      method_name <- rule$method
      var_names <- rule$var

      # Convert relevant variables to standard date format
      for (var_name in var_names) {
        if (var_name %in% names(data) &&
            dataset_name %in% names(dat_variable_list) &&
            var_name %in% dat_variable_list[[dataset_name]]) {
          data[[var_name]] <- convert_dates_to_ymd(as.character(data[[var_name]]))
        }
      }

      # Dynamically apply the method
      if (exists(method_name, mode = "function")) {
        data <- do.call(method_name, c(list(data), lapply(var_names, rlang::sym), list(dco)))
      } else {
        warning(paste("Method", method_name, "not found. Skipping."))
      }
    }

    # Validate output path
    if (is.null(out_path) || is.na(out_path) || out_path == "" || !dir.exists(out_path)) {
      message("Invalid or missing output_path. Defaulting to data_path.")
      out_path <- data_path
    }

    # Save the processed dataset
    saveRDS(data, file = file.path(out_path, paste0(dataset_name, out_file_suffix)))
  }
}
