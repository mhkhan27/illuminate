#' Generate dataset for butteR::survey_collaps()
#' @param df The data set
#' @param kobo_servey_sheet The defult (NUll) will create additional table for multiple choice questions
#' @export


remove_mutiple_choice_cols_and_make_logical <- function(df,kobo_servey_sheet){

  select_mutiple_from_kobo <- kobo_servey_sheet %>% dplyr::filter(str_detect(type,"select_multiple"))
  integer_from_kobo <- kobo_servey_sheet %>% dplyr::filter(str_detect(type,"integer"))

  select_mutiple_from_kobo_cols <-select_mutiple_from_kobo$name
  interger_from_kobo_cols <-integer_from_kobo$name

  select_mutiple_from_kobo_cols<- select_mutiple_from_kobo_cols[select_mutiple_from_kobo_cols %in% names(df)]

  df <- df %>% dplyr::select(-select_mutiple_from_kobo_cols)

  for(i in select_mutiple_from_kobo_cols ){
    df_cols_to_logical <- df %>% select(starts_with(paste0(i,"."))) %>% names()

    df <- df %>% mutate_at(df_cols_to_logical,as.logical)
  }

  df <- df %>%
    select_if(function(x) !all(is.na(x)))

  return(df)
}
