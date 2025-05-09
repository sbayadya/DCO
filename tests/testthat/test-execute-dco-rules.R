library(testthat)
library(mockery)
library(yaml)
library(dplyr)

test_that("execute_dco_rules applies methods correctly and saves output", {
  # --- Create temporary data and paths ---
  temp_dir <- tempdir()
  input_data <- data.frame(
    id = 1:3,
    event_date = as.Date(c("2023-04-01", "2023-05-15", "2023-06-01")),
    stringsAsFactors = FALSE
  )

  dataset_name <- "ae"
  input_file <- file.path(temp_dir, paste0(dataset_name, ".rds"))
  saveRDS(input_data, input_file)

  # --- Create temporary YAML config ---
  config_list <- list(
    general_info = list(
      input_path = temp_dir,
      output_path = temp_dir,
      dco_date = "2023-05-01",
      out_file_suffix = "_filtered.rds"
    ),
    dco_rules = list(
      ae = list(
        list(method = "filter_by_date", var = list("event_date"))
      )
    ),
    dat_variable_list = list(
      ae = list("event_date")
    )
  )

  config_path <- file.path(temp_dir, "config.yaml")
  write_yaml(config_list, config_path)

  # --- Define mock functions ---
  mock_filter <- function(data, col_sym_list, dco) {
    col_sym <- col_sym_list[[1]]
    dplyr::filter(data, !!col_sym <= as.Date(dco)) |> head(2)
  }




  mock_convert_dates <- function(x) {
    as.Date(x)  # mimic actual conversion
  }

  # --- Stub the functions in execute_dco_rules ---
  stub(execute_dco_rules, "filter_by_date", mock_filter)
  stub(execute_dco_rules, "convert_dates_to_ymd", mock_convert_dates)

  # --- Run the function ---
  execute_dco_rules(
    config_path = config_path,
    data_path = temp_dir,
    out_path = temp_dir,
    out_file_suffix = "_filtered.rds",
    dco = "2023-05-01"
  )

  # --- Check the output ---
  output_file <- file.path(temp_dir, paste0(dataset_name, "_filtered.rds"))
  expect_true(file.exists(output_file))

  result <- readRDS(output_file)
  expect_equal(nrow(result), 1) # Only one date is <= "2023-05-01"
  expect_equal(names(result), c("id", "event_date"))
  expect_equal(result$id, 1)
  expect_equal(as.character(result$event_date), "2023-04-01")
})
