% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey_analysis.R
\name{survey_collapse_categorical_long}
\alias{survey_collapse_categorical_long}
\title{Collapse categorical data into tidy long format}
\usage{
survey_collapse_categorical_long(df, x, disag = NULL, na_val = NA_character_)
}
\arguments{
\item{x}{columns to collapse}

\item{disag}{the columns to collapse/ subset by(analagous to [[dplyr::group_by]] to [[dplyr::summarise]]) flow
#' @param na_val if you want NA replaced by value. By default NA values will be removed prior to aggregation. It is recommended
that you do not adjust this value and deal with na values as a separate step}

\item{dfsvy}{a survey or preferably srvyr object}

\item{sm_sep}{select multiple parent child separator. This is specific for XLSForm data (default = /).
If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'}
}
\value{
a long format data frame containing the collapsed data.
}
\description{
`survey_collapse_categorical)long()` uses the srvyr [srvyr::survey_mean] & survey package [survey::svyciprop]   methods
to collapse/or aggregate cateogrical data. This function can be used on its own, but was build mainly to for its use in [butteR::survey_collapse]
which is meant to help batch analyze data
}
