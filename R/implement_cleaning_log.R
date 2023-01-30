#' implement cleaning log on raw data set. [This function is extracted from butteR]
#' @param df raw data (data.frame)
#' @param df_uuid column in raw data with uuid
#' @param cl cleaning log (data.frame)
#' @param cl_change_type_col column in cleaning log which specifies change type to be made
#' @param change_type_for_change_response values in change type column which should be changed to a new value.
#' @param change_type_for_blank_response values in change type column which should be blank (NA).
#' @param change_type_for_no_change values in change type column which should NOT be changed to a new value.
#' @param change_type_for_deletion values in change type column which should be deleted from the data.
#' @param cl_change_col column in cleaning log which specifies data set column to change
#' @param cl_uuid uuid in cleaning log
#' @param cl_new_val cleaning log column specifying the new correct value
#' @return clean data set
#' @export


implement_cleaning_log <- function(df,
                                   df_uuid,
                                   cl,
                                   cl_change_type_col,
                                   change_type_for_change_response = "change_response",
                                   change_type_for_blank_response = "blank_response",
                                   change_type_for_no_change = "no_action",
                                   change_type_for_deletion = "remove_survey",
                                   cl_change_col,
                                   cl_uuid,cl_new_val){


  all_type <- c(change_type_for_change_response,change_type_for_blank_response,change_type_for_no_change,change_type_for_deletion)

  cl <- cl |> dplyr::mutate(
    change_type_created_f = case_when(!!sym(cl_change_type_col) %in% change_type_for_change_response ~ "change_response",
                                      !!sym(cl_change_type_col) %in% change_type_for_blank_response ~ "blank_response",
                                      !!sym(cl_change_type_col) %in% change_type_for_no_change ~ "no_action",
                                      !!sym(cl_change_type_col) %in% change_type_for_deletion ~ "remove_survey"))

  assertthat::assert_that(all(cl[[cl_change_type_col]] %in% all_type),
                          msg="Error:You have missing change_type option(s)")


  cl[[cl_change_type_col]]<-cl[[cl_change_type_col]] %>% tolower()

  cl[[cl_change_col]]<-cl[[cl_change_col]] %>% trimws()
  cl[[cl_new_val]]<-cl[[cl_new_val]] %>% trimws()
  cl_change_type_options<- c("change_response",  "remove_survey", "blank_response","no_action")
  # cl_change_response<- cl %>% filter(!!sym(cl_change_type_col) %in% c( cl_change_type_options[1]))
  cl_change_response<- cl %>% filter(change_type_created_f %in% c( cl_change_type_options[1],cl_change_type_options[3]))
  cl_change_response<- cl_change_response %>%
    mutate(
      !!cl_new_val:=ifelse(change_type_created_f==cl_change_type_options[3],NA,!!sym(cl_new_val))
    )
  # cl_change_type_options[3]
  cl_remove_survey<- cl %>% filter(change_type_created_f == cl_change_type_options[2])
  # cl_blank_response<- cl %>% filter(!!sym(cl_change_type_col))

  if(all(cl_change_response[[cl_change_col]] %in% colnames(df))==F){
    problem_question_in_cl<-cl_change_response[[cl_change_col]][cl_change_response[[cl_change_col]] %in% colnames(df)==FALSE]
    print(paste0(problem_question_in_cl,": not in data"))
  }

  if(all(cl[[cl_uuid]] %in% c(df[[df_uuid]],"all_data")==F)){
    problem_uuid_in_cl<-cl[[cl_uuid]][cl[[cl_uuid]] %in% c(df[[df_uuid]],"all_data")==FALSE]
    print(problem_uuid_in_cl)
    print("NOT IN DATASET")
  }
  assertthat::assert_that(all(cl_change_response[[cl_change_col]] %in% colnames(df)),
                          msg="Error: Make sure all names in cl_change_col values in the cleaning log are in dataset")
  assertthat::assert_that(all(cl[[cl_uuid]] %in% c(df[[df_uuid]],"all_data")),
                          msg="Error:Make sure all uuids in cleaing log are in data set")

  if(nrow(cl_change_response)>0){
    for(i in 1:nrow(cl_change_response)){
      print(cl_change_response[[cl_change_col]][i])
      cl_uuid_temp<-cl_change_response[[cl_uuid]][i]
      cl_question_temp<-cl_change_response[[cl_change_col]][i]
      cl_new_val_temp<-cl_change_response[[cl_new_val]][i]

      if(cl_uuid_temp!="all_data"){
        df[df[[df_uuid]]==cl_uuid_temp,cl_question_temp]<-cl_new_val_temp}

      if(cl_uuid_temp=="all_data"){
        df[,cl_question_temp]<-cl_new_val_temp}
    }
  }
  else{ print("no change_response in log")}
  if(nrow(cl_remove_survey)>0){
    df<- df %>% filter(!!sym(df_uuid) %in% cl_remove_survey[[cl_uuid]]==FALSE)
  }else{print("no surveys to remove in log")}

  return(df)
}


#' check cleaning log
#' @param df raw data (data.frame)
#' @param df_uuid column in raw data with uuid
#' @param cl cleaning log (data.frame)
#' @param cl_change_type_col column in cleaning log which specifies change type to be made
#' @param change_type_for_change_response values in change type column which should be changed to a new value.
#' @param cl_change_col column in cleaning log which specifies data set column to change
#' @param cl_uuid uuid in cleaning log
#' @param cl_new_val cleaning log column specifying the new correct value
#' @return cleaning log with only problematic entries and note specifying problem
#' @export


check_cleaning_log <- function(df,
                               df_uuid,
                               cl,
                               cl_change_type_col,
                               change_type_for_change_response = "change_response",
                               cl_change_col,
                               cl_uuid,cl_new_val){
  cl[[cl_change_col]]<-cl[[cl_change_col]] %>% trimws()
  cl[[cl_new_val]]<-cl[[cl_new_val]] %>% trimws()

  cl_change_col_prob_df<-cl %>%
    filter(!!sym(cl_change_type_col) %in% change_type_for_change_response) %>%
    mutate(cl_prob="question_does_not_exist") %>%
    filter(!!sym(cl_change_col) %in% colnames(df)==FALSE) %>%
    select(cl_prob,everything())
  cl_uuid_prob_df<-cl %>% filter(!!sym(cl_uuid) %in% c(df[[df_uuid]],"all_data")==FALSE)%>%
    mutate(cl_prob="uuid_does_not_exist") %>%
    filter(!!sym(cl_uuid) %in% df[[df_uuid]]==FALSE) %>%
    select(cl_prob,everything())

  cl_problems_df<-bind_rows(get0("cl_change_col_prob_df"), get0("cl_uuid_prob_df"))
  if(nrow(cl_problems_df)>0){
    print("cleaning log has issues, see output table")
  }
  else{
    cl_problems_df<-"no issues in cleaning log found"

  }
  return(cl_problems_df)

}
