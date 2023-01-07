#'
#' check outliers of the dataset
#'
#' @param df data frame
#' @param kobo_tool_location kobo tool path. Defult is null
#' @param cols_to_report Variables those must be included in the output
#' @param cols_to_remove Variables those must not consider for outlier checks even though they are numeric
#' @param minumum_unique_value_of_variable Default is 10, means for any variable where number of unique value is less than 10, then the variable won't be considered for outlier checking.
#' @return Outliers
#' @export
#'


identify_outliers <- function(df,kobo_tool_location=NULL,cols_to_report,
                          maximum_standard_deviations = 3,
                          minumum_unique_value_of_variable =10,
                          cols_to_remove= NULL){

if(!is.null(cols_to_remove)){
  df <- df %>% select(-all_of(cols_to_remove))
}



if(!is.null(kobo_tool_location)) {

  survey_sheet <- read.xlsx(kobo_tool_location,sheet = "survey")
  choice_sheet <- read.xlsx(kobo_tool_location,sheet = "choices")
  survey_sheet$name <- survey_sheet$name %>% str_replace_all("-",".")


  interger_column_in_kobo <- (survey_sheet %>% filter(type == "integer") %>%
                                filter( !grepl('enumerator|_instance_', name)))$name

  cols_name_exist_in_loop_kobo <- interger_column_in_kobo[interger_column_in_kobo %in% names(df)]

  }

  cols_name_exist_in_loop_numeric <- df %>% select_if(is.numeric) %>% select(-starts_with("X"))%>% names()
  cols_name_exist_in_loop_int <- df %>% select_if(is.integer) %>% select(-starts_with("X"))%>% names()

if(!is.null(kobo_tool_location)) {
  cols_name_exist_in_loop <- c(cols_name_exist_in_loop_kobo,
                               cols_name_exist_in_loop_numeric,
                               cols_name_exist_in_loop_int) %>% unique()

}




if(is.null(kobo_tool_location)) {
  cols_name_exist_in_loop <- c(cols_name_exist_in_loop_numeric,
                               cols_name_exist_in_loop_int) %>% unique()

}

outlier_checks <- list()

  for (x in cols_name_exist_in_loop) {
    print(paste0("checking_",x))

    df[[x]] <- df[[x]] %>% as.numeric()
    variable_value <- df[[x]]

    variable_value <- variable_value[!is.na(variable_value) & !is.null(variable_value) & !is.infinite(variable_value)]

    outliers_tf_nr <- abs(variable_value - mean(variable_value)) > maximum_standard_deviations * sd(variable_value) &
      length(unique(variable_value)) > minumum_unique_value_of_variable


    outliers_value <- variable_value[outliers_tf_nr] %>% unique()



    outlier_checks[[x]]  <-  df %>% mutate(
      issue = case_when(df[[x]] %in% outliers_value ~"outlier (normal distribution)"),
    ) %>% filter(issue == "outlier (normal distribution)") %>% select(cols_to_report,issue,x) %>%
      pivot_longer(cols = paste0(x),names_to ="question",values_to= "old_value")



    #### log checks ####

    df[["log"]] <- df[[x]] %>% log()


    log_variable <- df[["log"]]
    log_variable <- log_variable[!is.na(log_variable) & !is.null(log_variable) & !is.infinite(log_variable)]

    outliers_tf <- abs(log_variable - mean(log_variable)) > maximum_standard_deviations * sd(log_variable) &
        length(unique(log_variable)) > minumum_unique_value_of_variable


    outliers_value_log <- log_variable[outliers_tf] %>% unique()



    outlier_checks[[paste0("log_",x)]] <-  df %>% mutate(
      issue = case_when(df[["log"]] %in%  outliers_value_log ~ "outlier (log distribution)"),
    ) %>% filter(issue == "outlier (log distribution)") %>% select(cols_to_report,issue,x) %>%
      pivot_longer(cols = paste0(x),names_to ="question",values_to= "old_value")


  }

  outliers_cl <- do.call("bind_rows",outlier_checks)

  outliers_cl <- outliers_cl %>% distinct(!!sym(cols_to_report),question,old_value,.keep_all = T)


  return(outliers_cl)
}
