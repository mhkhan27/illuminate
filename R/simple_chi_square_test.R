#' Simple pair wise chi-square test
#'
#' The count is only based on independent variable, not a cross table of dependent and independent variable
#' All the independent variable must be a binary variable to run the function
#'
#' @param survery_object Survey object from srvyr or survey package
#' @param dap a data frame providing the names of dependent and independent variable
#' @param independent_variable_col_in_dap column name that specify the column of independent variable in the the dap
#' @param dependent_variable_col_in_dap column name that specify the column of dependent variable in the the dap
#' @export



simple_chi_square_test <- function(survery_object,dap,
                                   independent_variable_col_in_dap,
                                   dependent_variable_col_in_dap){


survery_object$variables <- survery_object$variables %>%
  mutate_if(is.character,as.factor)


mod_res<-list()

for(i in 1:nrow(dap)){
  ind_temp<-dap[[independent_variable_col_in_dap]][i]
  dep_temp<-dap[[dependent_variable_col_in_dap]][i]


  #overall chisq
  chisq_res<-svychisq(formula = formula(paste0("~",dep_temp,"+",ind_temp)),design=survery_object)
  # just messing with some tidy models functions
  chisq_res_tidy<-chisq_res%>%
    tidy() %>%
    mutate(ind_var_name=ind_temp,
           dep_var_name=dep_temp) %>%
    janitor::clean_names()

  mean_ci = svyby(formula(paste0("~",dep_temp)), formula(paste0("~",ind_temp)), hhsvy, svymean, vartype="ci", na.rm = T)
  unwt_n = svyby(formula(paste0("~",dep_temp)), formula(paste0("~",ind_temp)), hhsvy, unwtd.count, keep.var=F)

  row.names(mean_ci) <- NULL
  row.names(unwt_n) <- NULL

  # mean_ci %>%  pivot_longer(cols=c("shelter_paid.payment_of_cash", "ci_l", "ci_u"),
  #                         names_to = "name",values_to = "value")

  mean_ci <- mean_ci %>% mutate(ind_var_name=ind_temp,
                                dep_var_name =dep_temp) %>%  rename("ind_var_name_value"=ind_temp,
                                                                    "mean"=dep_temp)

  unwt_n <-unwt_n %>% mutate(ind_var_name=ind_temp,
                             dep_var_name =dep_temp) %>%  rename("ind_var_name_value"=ind_temp,
                                                                 "count_independent_variable"="statistic")
  mean_and_weights <- mean_ci %>% left_join(unwt_n,by = c("ind_var_name_value", "ind_var_name", "dep_var_name"))

  chisq_res_tidy_with_mean_weights <- mean_and_weights %>% left_join(chisq_res_tidy,by = c("ind_var_name", "dep_var_name"))
  mod_res[[i]]<-chisq_res_tidy_with_mean_weights
}

all_res_data <- do.call("bind_rows",mod_res) %>% select(dep_var_name,ind_var_name,ind_var_name_value,count_independent_variable,p_value,mean,ci_l,ci_u,method)

return(all_res_data)
}
