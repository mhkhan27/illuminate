#' Generate dataset for butteR::survey_collaps()
#' @param df The data set
#' @param kobo_survey_sheet The defult (NUll) will create additional table for multiple choice questions
#' @param remove_multiple_choice_concat A logical parameter. Ture will remove the aggregated columns (for multiple choice question) from the dataset
#' @param remove_all_NA_col A logical parameter. Ture will remove the columns where all values are NA
#' @export


fix_data_type_frm_kobo <- function(df,kobo_survey_sheet,remove_multiple_choice_concat = F,remove_all_NA_col=T){

  select_mutiple_from_kobo <- kobo_survey_sheet %>% dplyr::filter(str_detect(type,"select_multiple"))
  integer_from_kobo <- kobo_survey_sheet %>% dplyr::filter(str_detect(type,"integer"))

  select_mutiple_from_kobo_cols <-select_mutiple_from_kobo$name
  interger_from_kobo_cols <-integer_from_kobo$name


  select_int_from_kobo_cols<- interger_from_kobo_cols[interger_from_kobo_cols %in% names(df)]




    df <- df %>% mutate_at(select_int_from_kobo_cols,as.integer)




  select_mutiple_from_kobo_cols<- select_mutiple_from_kobo_cols[select_mutiple_from_kobo_cols %in% names(df)]

  if(remove_multiple_choice_concat == T){

  df <- df %>% dplyr::select(-select_mutiple_from_kobo_cols)
  }


  for(i in select_mutiple_from_kobo_cols ){
    df_cols_to_logical <- df %>% select(starts_with(paste0(i,"."))) %>% names()

    df <- df %>% mutate_at(df_cols_to_logical,as.integer)
    df <- df %>% mutate_at(df_cols_to_logical,as.logical)
  }

if(remove_all_NA_col ==T){
  df <- df %>%
    select_if(function(x) !all(is.na(x))) ### remove all NA columns
}
  return(df)
}






#' @param df The data set
#' @param kobo_survey_sheet The defult (NUll) will create additional table for multiple choice questions
#' @param remove_multiple_choice_concat A logical parameter. Ture will remove the aggregated columns (for multiple choice question) from the dataset
#' @param additional_cols_to_remove Any additional columns that you want to include as qualitative
#' @export


find_qualitative_cols <-function(df,kobo_survey_sheet,additional_cols_to_remove =NULL){

  text_cols <- (kobo_survey_sheet %>% dplyr::filter(type == "text"))$name
  date_cols <- (kobo_survey_sheet %>% dplyr::filter(type == "date"))$name
  gps_cols <- (kobo_survey_sheet %>% dplyr::filter(type == "geopoint"))$name
  note_cols <- (kobo_survey_sheet %>% dplyr::filter(type == "note"))$name

  starts_with_x <- df %>% select(starts_with("X_")) %>% names()

  other_unnecessary <- c( "start", "end", "audit", "today", "deviceid")

  cols_not_to_ana <- c(text_cols,starts_with_x,other_unnecessary,date_cols,gps_cols,note_cols,additional_cols_to_remove) %>% unique()

  cols_not_to_ana <- cols_not_to_ana[cols_not_to_ana %in% names(df)]
  return(cols_not_to_ana)

}









