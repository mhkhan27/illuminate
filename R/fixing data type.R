#' @param df The data set
#' @param kobo_survey_sheet The defult (NUll) will create additional table for multiple choice questions
#' @param remove_multiple_choice_concat A logical parameter. Ture will remove the aggregated columns (for multiple choice question) from the dataset
#' @param additional_cols_to_remove Any additional columns that you want to include as qualitative
#' @export


find_qualitative_cols_from_kobo <-function(df,kobo_survey_sheet,additional_cols_to_remove =NULL){

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



numbers_only <- function(x) !grepl("\\D", x)

#' Find all the integer column even if reads as character or factor
#' @param df The data set
#' @export
#'
find_interger_cols <- function(df){
  df1 <- df %>% select_if(function(x) all(numbers_only(trimws(x)),na.rm = T)) %>% names()
  return(df1)
}



#' Fix data type from the dataset
#' @param df The data set
#' @param kobo_survey_sheet The defult (NUll) will create additional table for multiple choice questions
#' @param remove_multiple_choice_concat A logical parameter. Ture will remove the aggregated columns (for multiple choice question) from the dataset
#' @param remove_all_NA_col A logical parameter. Ture will remove the columns where all values are NA
#' @export


fix_data_type <- function(df,remove_all_NA_col=T,na.string=c("","NA"),character_cols=NULL){


## fix naming issue
names(df)  <- names(df) %>% str_replace_all("/",".")



  ### for numeric

df <- type.convert(df, as.is = T,na.string= na.string)



## fix_integer
int_cols_name <- find_interger_cols(df)
df <- df %>% mutate_at(int_cols_name,as.integer)



### select logical column
logical_cols_name <- df %>% select_if(function(x) all(x %in% c(0,1,NA,NA_integer_,NA_real_) |
                                                        x %in% c("0","1",NA_character_),na.rm = T)) %>% names()

df <- df %>% mutate_at(logical_cols_name,as.integer)
df <- df %>% mutate_at(logical_cols_name,as.logical)


if(!is.null(character_cols)){ df <- df %>% mutate_at(character_cols,as.character)}


## remove all NA column
if(remove_all_NA_col ==T){
  df <- df %>%
    select_if(function(x) !all(is.na(x))) ### remove all NA columns
}


  return(df)
}






