#'
#' check outliers of the dataset
#'
#' @param df data frame
#' @param kobo_tool_location kobo tool path. Defult is null
#' @return Outliers issue
#' @export
#'


outlier_check <- function(df,kobo_tool_location=NULL,cols_to_report = NULL){

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

    outlier_checks[[x]] <-  df %>% mutate(
      issue = case_when(df[[x]] %in% boxplot.stats(aa)$out~"outlier"),
    ) %>% filter(issue == "outlier") %>% select(cols_to_report,issue,x) %>%
      pivot_longer(cols = paste0(x),names_to ="questions",values_to= "old_value")
  }

  outliers_cl <- do.call("bind_rows",outlier_checks)

  return(outliers_cl)
}
