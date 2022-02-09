#' Quick Surveys
#' @param df data set
#' @param consent_column_name column where surevy consent status is stored (default is X_uuid)
#' @param uuid_col_name  column contining unique identifier
#' @param start_question_node start question node from where calculation for time duration will start (audit node is excluded)
#' @param end_question_node end question node to where calculation for time duration will end (audit node is excluded)
#' @param min_allowable_duration Minumum allowable duration
#' @param audit_yes output from butteR::load_audit()
#' @return Quick survey list
#' @export
#'

quick_survey<-function(df,
                       consent_column_name,
                       audit_node,
                       uuid_col_name = "X_uuid",
                       start_question_node,
                       end_question_node,
                       min_allowable_duration,
                       audit_yes){



  retur_list <- list()
  dfl<-list()
  for (i in 1: length(audit_yes)){
    d<-audit_yes[[i]]

    d$node<-gsub("\\[1]","",d$node)
    d$node<-gsub("\\[2]","",d$node)
    d$node<-gsub("\\[3]","",d$node)

    start_question <- d %>% filter(node==paste0(audit_node,start_question_node)& !is.na(event)) %>%
      select(end)
    start_question<-min(start_question$end)
    end_question<-d %>% filter(node==paste0(audit_node,end_question_node)& !is.na(node)) %>%
      select(end)

    end_question<-max(end_question$end)
    duration_ms<-end_question-start_question
    duration_secs<-duration_ms/1000
    duration_minutes<- round(duration_secs/60,1)
    dfl[[i]]<-data.frame(uuid=names(audit_yes)[i],duration_ms=duration_ms,durations_secs=duration_secs,duration_minutes= duration_minutes)
  }

  duration_df2<-do.call("rbind", dfl)
  surveys_with_duration<- duration_df2
  quick_survey_df <- duration_df2 %>% dplyr::filter(duration_minutes < min_allowable_duration)
  quick_survey_df <- quick_survey_df %>% dplyr::select(-c("duration_ms","durations_secs"))

  retur_list[["surveys_with_duration"]] <- surveys_with_duration
  retur_list[["quick_survey_df"]] <- quick_survey_df
  list2env(retur_list,.GlobalEnv)
}
