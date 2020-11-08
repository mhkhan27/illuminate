#' Generate Frequency Table For Questions
#' @param df The data set
#' @param aggregation_level Aggregation level(s) to analyze data by. The default (NULL) will fully aggregate data to provide one value per variable. Argument also accepts vecors to aggregate upto two variables.
#' @export


survey_frequency_by_questions<- function(df, aggregation_level=NULL){

  if(is.null(aggregation_level)){
    frequency <- df %>% summarise_at(.vars = names(df),.funs = ~ sum(!is.na(.)))
  }

  if(!is.null(aggregation_level)){
    frequency <- df %>% dplyr::group_by(!!!syms(aggregation_level))%>%
      dplyr::summarise_at(names(df %>% select(-aggregation_level)),.funs = ~ sum(!is.na(.)))
  }

  return(frequency)
}
