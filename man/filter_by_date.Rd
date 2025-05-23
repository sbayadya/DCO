% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_by_date.R
\name{filter_by_date}
\alias{filter_by_date}
\title{Filter a Data Frame by Date}
\usage{
filter_by_date(.data, date_var, cutoff_date)
}
\arguments{
\item{.data}{A data frame to filter.}

\item{date_var}{The name of the date column to filter by (unquoted).}

\item{cutoff_date}{The cut-off date. Rows with dates before this date will be kept.}
}
\value{
A filtered data frame containing only rows where the date in the specified column
is before the cut-off date.
}
\description{
This function filters a data frame, keeping only the rows where the date in a specified column
is before a given cut-off date.
}
