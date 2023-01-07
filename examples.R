# devtools::install_github("mhkhan27/illuminate")

rm(list = ls())

library(srvyr)
library(illuminate)
library(readxl)
library(openxlsx)
library(tidyverse)

read_sheets("C:\\Users\\rakib\\Downloads\\clean_data.xlsx",data_type_fix = T,remove_all_NA_col = T)
variable_to_ana <- clean_data[70:100] %>% names()
overall_analysis <- survey_analysis(df = clean_data,weights = F,vars_to_analyze = variable_to_ana)
