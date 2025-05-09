library(testthat)
library(yaml)
library(mockery)

test_that("perform_dco handles all scenarios correctly", {
  # ------------------------
  # 1. Test reading of YAML configuration
  # ------------------------
  config_content <- "
  general_info:
    input_path: 'path/to/your/raw_data'
    output_path: 'path/to/your/output_data'
    dco_date: '2023-05-01'
    out_file_suffix: '_filt.rds'
  dco_rules:
    dataset_name:
      - method: filter_by_date
        var: ['date_var']
  dat_variable_list:
    dataset_name:
      - date_var
  "

  config_path <- tempfile(fileext = ".yaml")
  writeLines(config_content, config_path)

  # Mock execute_dco_rules to avoid file system dependency
  mock_execute_dco_rules <- mockery::mock(return_value = NULL)
  mockery::stub(perform_dco, "execute_dco_rules", mock_execute_dco_rules)

  # Call the function and verify correct arguments are passed
  perform_dco(config_path)

  # Verify that execute_dco_rules was called once
  mockery::expect_called(mock_execute_dco_rules, 1)

  # Verify that the correct arguments were passed to execute_dco_rules
  mockery::expect_args(mock_execute_dco_rules, 1, config_path, 'path/to/your/raw_data', 'path/to/your/output_data', '_filt.rds', '2023-05-01')

  # ------------------------
  # 2. Test for missing required keys (e.g., dco_date)
  # ------------------------
  config_path_missing_dco <- tempfile(fileext = ".yaml")
  config_content_missing_dco_date <- "
  general_info:
    input_path: 'path/to/your/raw_data'
    output_path: 'path/to/your/output_data'
    out_file_suffix: '_filt.rds'
  dco_rules:
    dataset_name:
      - method: filter_by_date
        var: ['date_var']
  dat_variable_list:
    dataset_name:
      - date_var
  "
  writeLines(config_content_missing_dco_date, config_path_missing_dco)
  expect_error(perform_dco(config_path_missing_dco), "Error: dco_date not found in config.yaml")
})
