
# illuminate <img src='man/figures/illuminate.png' align="right" height="80.5" />

<!-- <!-- badges: start -->
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/dplyr)](https://cran.r-project.org/package=dplyr) -->
<!-- [![R-CMD-check](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml) -->
<!-- [![Codecov test coverage](https://codecov.io/gh/tidyverse/dplyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/dplyr?branch=main) -->
<!-- <!-- badges: end -->

## Overview

illuminate is designed for making the data analysis easy and less time
consuming. The package is based on tidyr, dplyr,srvyr packages. Most
common functions are-

## ***1. Read and write file***

-   `read_sheets()` read all sheet in an excel file and makes sure that
    data stores in appropriate data type.
-   `write_formatted_excel()` write formatted excel.

## ***2. Data cleanining***

-   `outlier_checks()`
-   `others_checks()`
-   `survey_duration_from_audit()`

## ***3. Clearting cleanining log***

## ***4. Implementing cleanining log***

## ***5. Data Analysis***

-   `survey_analysis()` calculate the weighted mean/proporation and
    unweighted count for all existing variable is the dataset.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mhkhan27/illuminate")
```

## EXAMPLE :: Survey analysis

##### Step 0:: Call libraries

``` r
library(srvyr)
library(illuminate)
library(tidyverse)
library(purrr)
library(openxlsx)
```

##### Step 1:: Read data

Read data with `read_sheets()`. This will make sure your data is stored
as appropriate data type. It is important to make sure that all the
select multiple are stored as logical, otherwise un weighted count will
be wrong. It is recommended to use the `read_sheets()` to read the data.
use \``?read_sheets()` for more details.

``` r
read_sheets("data/data.xlsx",data_type_fix = T,remove_all_NA_col = T,na_strings = c("NA",""," "))
```

The avobe code will give a dataframe called `data_up`

##### Step OPTIONAL:: Preaparing the data

<span style="color: red;">ONLY APPLICABLE WHEN YOU ARE NOT READING YOUR
DATA WITH `read_sheets()`</span>

``` r
# data_up <- read_excel("data/data.xlsx")
data_up <- fix_data_type(df = data_up)
```

##### Step 2:: Weight calculation

To do the weighted analysis, you will need to calculate the weights. If
your dataset already have the weights column then you can ignore this
part

###### Read sampleframe

``` r
read_sheets("data/sampling_frame.xlsx")
```

This will give a dataframe called `sampling frame`

``` r
weights <- data_up %>% group_by(governorate1) %>% summarise(
  survey_count = n()
) %>% left_join(sampling_frame,by =c("governorate1"="strata.names")) %>% 
  mutate(sample_global=sum(survey_count),
         pop_global=sum(population),
         survey_weight= (population/pop_global)/(survey_count/sample_global)) %>% select(governorate1,survey_weight)
```

###### Add weights to the dataframe

``` r
data_up <- data_up %>% left_join(weights)
```

##### Step 3.1:: Weighted analysis

``` r
overall_analysis <- survey_analysis(df = data_up,weights = T,weight_column = "survey_weight",strata = "governorate1")
```

##### Step 3.2:: Unweighted analysis

``` r
overall_analysis <- survey_analysis(df = data_up,weights = F)
```

##### Step 3.3:: Weighted and disaggregated by gender analysis

Use `?survey_analysis()` to know about the perameters.

``` r
columns_to_analyze <- data_up[20:30] %>% names() # dummy code. Please define the name of the columns which you would like to analyze. Default will analyze all the variables exist in the dataset. 

analysis_by_gender <-  survey_analysis(df = data_up,weights = T,weight_column = "survey_weight",vars_to_analyze = columns_to_analyze,
                                       strata = "governorate1",disag = "gender_speaker")
```

##### Step 4:: Export with `write_formatted_excel()`

You can use any function to export the analysis however
`write_formatted_excel()` can export formatted file. See the
documentation for more details

``` r
write_list <- list(overall_analysis ="overall_analysis",
                   analysis_by_gender ="analysis_by_gender"
)

write_formatted_excel(write_list,"analysis_output.xlsx",
                      header_front_size = 12,
                      body_front = "Times")
```
