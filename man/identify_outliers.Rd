% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outliers_checks.R
\name{identify_outliers}
\alias{identify_outliers}
\title{check outliers of the dataset}
\usage{
identify_outliers(
  df,
  kobo_tool_location = NULL,
  cols_to_report,
  maximum_standard_deviations = 3,
  minumum_unique_value_of_variable = 10,
  cols_to_remove = NULL
)
}
\arguments{
\item{df}{data frame}

\item{kobo_tool_location}{kobo tool path. Defult is null}

\item{cols_to_report}{Variables those must be included in the output}

\item{minumum_unique_value_of_variable}{Default is 10, means for any variable where number of unique value is less than 10, then the variable won't be considered for outlier checking.}

\item{cols_to_remove}{Variables those must not consider for outlier checks even though they are numeric}
}
\value{
Outliers
}
\description{
check outliers of the dataset
}
