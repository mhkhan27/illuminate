rm(list = ls())

library(srvyr)
library(illuminate)
library(readxl)
library(openxlsx)
library(tidyverse)

read_sheets("data/data.xlsx",data_type_fix = F,remove_all_NA_col = T)

data_up$hh_age_gender4

variable_to_ana <- data_up[40:70] %>% names()

overall_analysis <- survey_analysis(df = data_up,weights = F,vars_to_analyze = variable_to_ana)
