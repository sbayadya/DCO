#' Apply Rules from YAML File to Data Frames
#'
#' @description Reads a YAML configuration file and applies
#' specified DCO rules to datasets as defined in the configuration.
#'
#' @param config_path The path to the `config.yaml` file.
#'
#' @importFrom dplyr filter sym
#' @importFrom yaml read_yaml
#'
#' @return This function does not return a value; it triggers the data cut-off process and saves results to disk.
#'
#' @export
perform_dco <- function(config_path) {
  # Read the YAML file into a list
  config <- yaml::read_yaml(config_path)

  # Extract variables from the general_info section of the YAML file
  data_path       <- config$general_info$input_path
  out_path        <- config$general_info$output_path
  out_file_suffix <- config$general_info$out_file_suffix
  dco             <- config$general_info$dco_date

  # Error handling for missing required keys
  if (is.null(dco)) {
    stop("Error: dco_date not found in config.yaml")
  }

  if (is.null(data_path) || is.null(out_path) || is.null(out_file_suffix)) {
    stop("Error: One or more required fields (input_path, output_path, out_file_suffix) are missing in config.yaml")
  }

  # Execute DCO rules on the input datasets
  execute_dco_rules(config_path, data_path, out_path, out_file_suffix, dco)
}
