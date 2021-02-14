
# illuminate <img src='man/figures/logo.jpg' align="right" height="64.5" />

illuminate is designed for making the data analysis easy and time
consuming. The package is based on tidyr, dplyr, stringr packages. The
package now have different function including-

<p>

1.  freq\_by\_questions
2.  freq\_by\_choices
3.  merge\_kml
4.  kml\_to\_shp
5.  pic\_to\_pdf
6.  quick\_survey
7.  simple\_chi\_square\_test
    </p>

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mhkhan27/illuminate")
```

## Functions

### 1\. survey\_frequency\_by\_questions ()

survey\_frequency\_by\_questions() can very usefull to calculate the
number of response/available data (n) for each question asked to
reponsedent. You can use aggregation level/s to the function. The defult
aggregation level is NULL which ideally provides the overall response
count

###### Frequency by questions (with no aggregation level)

``` r
# library(illuminate)
# using air quality
frequency_df_no_aggregation <- illuminate::survey_frequency_by_questions(df =airquality)
```

``` r

frequency_df_no_aggregation %>% knitr::kable() 
```

| Ozone | Solar.R | Wind | Temp | Month | Day |
| ----: | ------: | ---: | ---: | ----: | --: |
|   116 |     146 |  153 |  153 |   153 | 153 |

###### Frequency by questions (with aggregation level)

``` r
# using air quality data from base r

frequency_df_with_aggregation <- illuminate::survey_frequency_by_questions(df =airquality,aggregation_level = "Month" ) 
```

``` r

frequency_df_with_aggregation %>% knitr::kable() 
```

| Month | Ozone | Solar.R | Wind | Temp | Day |
| ----: | ----: | ------: | ---: | ---: | --: |
|     5 |    26 |      27 |   31 |   31 |  31 |
|     6 |     9 |      30 |   30 |   30 |  30 |
|     7 |    26 |      31 |   31 |   31 |  31 |
|     8 |    26 |      28 |   31 |   31 |  31 |
|     9 |    29 |      30 |   30 |   30 |  30 |

### 2\. survey\_frequency\_by\_choices ()

survey\_frequency\_by\_choices() provides the count by each choice.
suppose in the following example we have a dataset of 40 observation
including the reponder gender and upazila.

###### Generating random dataset

``` r

df_for_fre_by_choices <- data.frame(
  gender = rep(c("male","female","male",NA),10),
  upazila =rep(c("teknaf","Ukhiya"),20)
)
 
cols_to_analyze <- names(df_for_fre_by_choices) # column names 
```

###### Frequency by choices (with no aggregation level)

``` r

fre_by_choice_no_aggregation <-illuminate::survey_frequency_by_choices(df = df_for_fre_by_choices,variables_to_analyze = cols_to_analyze)
```

``` r

fre_by_choice_no_aggregation %>% knitr::kable() 
```

| gender.female | gender.male | upazila.teknaf | upazila.Ukhiya |
| ------------: | ----------: | -------------: | -------------: |
|            10 |          20 |             20 |             20 |

###### Frequency by choices (with aggregation level)

``` r

fre_by_choice_with_aggregation <-illuminate::survey_frequency_by_choices(df = df_for_fre_by_choices,variables_to_analyze = cols_to_analyze,aggregation_level = "upazila")
#> Joining, by = "upazila"
```

``` r
fre_by_choice_with_aggregation %>% knitr::kable() 
```

| upazila | gender.female | gender.male |
| :------ | ------------: | ----------: |
| teknaf  |             0 |          20 |
| Ukhiya  |            10 |           0 |

### 3\. merge\_kml

merge\_kml() function is usefull when it comes to merge hundard of
samples points received from different partners

### 4\. kml\_to\_shp

Kml\_to\_shp() function is usefull when it comes to convert hundards of
KML to ESRI Shapefile at once

### 5\. pic\_to\_pdf

pic\_to\_pdf() function is usefull when it comes to convert hundards of
maps (in picture format, i.e jpg,tif,jped,png) to pdf format

### 6\. quick\_survey

The quick\_survey() function is specifally design for flagging the
surveys that has been completed less than a certain time.The function
use media file (audit zip) from kobo sever which should be loaded by
butteR::load\_audit() function. The function butteR can be found
[here](github.com/zackarno/butteR)
