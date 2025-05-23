Data-Cut-off (DCO) process as a tool in R
================

> **This R package significantly simplifies the DCO process in clinical
> trials by replacing complex, error-prone SAS macros with a clean,
> configurable, and extensible workflow in R.**

## Overview

Clinical trials rely heavily on timely and precise Data Cut-Off (DCO)
processes to ensure both participant safety and the validity of trial
outcomes. The DCO marks the date after which no new data are considered
for a specific analysis. It is a critical component in the ongoing
evaluation of safety profiles and efficacy, particularly during interim
analyses or final reporting.

Traditionally, the DCO process has been implemented using SAS macros,
which, while robust, can be time-consuming to debug and rigid to
customize. With the rise of open-source tools in the clinical domain, R
has become an increasingly attractive alternative for clinical
programmers.

This project introduces an R package designed specifically to manage the
DCO process in compliance with industry standards. The tool streamlines
the filtering and transformation of clinical datasets based on a
specified DCO date using a user-friendly, YAML-configurable interface.

## Key Features

-   **YAML-driven configuration**: Define datasets, date variables, and
    transformation rules without modifying the code.
-   **Predefined DCO methods**: Apply common DCO transformations
    out-of-the-box.
-   **Custom method support**: Integrate your own filtering logic when
    needed.
-   **Flexible architecture**: Easily extend or adapt to new data
    standards or formats.
-   **Time-saving automation**: Eliminate the need for manual filtering
    scripts or debugging SAS macros.

## Installation

Make sure required packages are installed:

``` r
install.packages(c("dplyr", "lubridate", "rlang", "yaml"))
```

Then, if you have access to the local package:

``` r
library(dco)
```

# How to Use

## Step 1: Create a Configuration Template

Run the following function to generate a YAML configuration file in your
working directory:

``` r
create_config_file()
```

This file allows you to specify:

-   The datasets to process

-   Date variables and DCO methods

-   Input and output paths

-   The DCO date

Actual representation of it looks like this:

``` yaml
# -----------------------
# General configuration
# -----------------------
general_info:
  input_path: "path/to/your/raw_data"      # <-- Set the input folder path
  output_path: "path/to/your/output_data"  # <-- Set the output folder path
  dco_date: "YYYY-MM-DD"                   # <-- Enter your cut-off date here
  out_file_suffix: "_filt.rds"             # <-- Suffix to append to filtered datasets

# -----------------------
# Rules for each dataset
# -----------------------
dco_rules:
  dataset_name:                            # <-- Set the dataset name that needs DCO process
    - method: method_name                  # <-- Set the method name 
      var: [variables_affected]            # <-- Set variable names that need to be cutted


# -----------------------
# Date variable mapping
# -----------------------
dat_variable_list:
  dataset_name:                            # <-- Set the dataset name that needs DCO process
    - variable name                        # <-- Set date variable names
 yaml: content
```

## Step 2: Perform the DCO Process

Once your config.yaml file is completed with the appropriate parameters,
execute:

``` r
perform_dco("path/to/your/config.yaml")
```

The specified datasets will be filtered according to the defined logic,
and saved to the output path with modified variables as needed.

# Sample example

To demonstrate the overall workflow of the DCO process, a test script
and example datasets are provided. These can be found in the file
dco/tests/testthat/run_demo.R. Running this script will showcase how the
datasets located in the raw_data directory are processed and filtered
according to the DCO logic defined in the package.

## Author: Seda Bayadyan, Supervisor: Erik Torosyan

A capstone project submitted in fulfillment of the requirements for the
degree of Bachelor in Data Science
