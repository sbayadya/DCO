#' Create a config.yaml File from Template
#'
#' @description This function copies a template `config.yaml` file to the user's working directory
#' if it doesn't already exist. The user can then modify the file with their own configuration details.
#'
#' @param config_path The path where the config file will be saved. Defaults to `"config.yaml"`.
#' If the file already exists at this location, the function will prompt the user not to overwrite it.
#'
#' @return A message indicating whether the config file was successfully created or if it already exists.
#'
#' @details This function is useful for setting up the required configuration for the `dco` package.
#' The template config file contains placeholders for paths, dates, and methods that need to be filled out
#' by the user.
#'
#' @export
create_config_file <- function(config_path = "config.yaml") {
  if (!file.exists(config_path)) {
    # Copy the template from the inst/extdata folder to the user's directory
    template_path <- system.file("extdata", "config.yaml", package = "dco")
    if (template_path == "") {
      stop("Template file 'config_template.yaml' not found in the package.")
    }
    file.copy(template_path, config_path)
    message("Config file created. Please edit it with your details.")
  } else {
    message("Config file already exists. Please modify it as needed.")
  }
}
