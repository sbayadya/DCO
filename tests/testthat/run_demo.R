# -------------------------------
# Example: Running DCO on Test Data
# -------------------------------
# This script demonstrates simple usage of `dco` package
# using a pre-filled test configuration file with test raw data which contains edge cases.
# The test config is located in `tests/testthat/metadata/test_config.yaml`.
# -------------------------------

# Setting DCO library
library(dco)

# Originally you should run this function below, but as it gives just structure without actual info needed
# for this testing, we skip this part and take config file which is correctly filled for test data.
# create_config_file()

perform_dco(normalizePath("tests/testthat/metadata/test_config.yaml"))

