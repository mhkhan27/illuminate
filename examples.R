# devtools::install_github("mhkhan27/illuminate")

rm(list = ls())

library(srvyr)
library(illuminate)
library(readxl)
library(openxlsx)
library(tidyverse)

read_sheets("C:\\Users\\rakib\\Downloads\\clean_data.xlsx",data_type_fix = T,remove_all_NA_col = T)

variable_to_ana <- clean_data[100:200] %>% names()

overall_analysis <- survey_analysis(df = clean_data,weights = F,vars_to_analyze = variable_to_ana)

pop_grp_analysis <- survey_analysis(df = clean_data,weights = F,vars_to_analyze = variable_to_ana,disag = c("pop_group"))

write_list <- list(overall_analysis = overall_analysis,
                   pop_grp_analysis = pop_grp_analysis)

write_formatted_excel(write_list = write_list,output_path = "analysis_for_check.xlsx")
