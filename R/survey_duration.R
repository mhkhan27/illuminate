
#' Quick Surveys
#' @param data Data set
#' @param audit_zip_path Location for audit zip file
#' @param filter Yes, if you want to apply
#' @param filter_column Name of filter column
#' @param filter_choice Filter choice name
#' @param uuid_col_name  Column containing unique identifier
#' @param start_question Start question, where calculation for time duration will start.
#' @param end_question End question node to where calculation for time duration will end.
#' @return Survey duration
#' @export
#'



survey_duration_from_audit<-function(data,
                                     audit_zip_path,
                                     filter =T,
                                     filter_column="informed_consent",
                                     filter_choice= "yes",
                                     uuid_col_name="X_uuid",
                                     start_question,
                                     end_question){

  ################## START::reading audit file #########################################

  list_of_file <- unzip(audit_zip_path, list = T) %>% rename(all_paths = Name)

  all_uuid_df <- list_of_file%>% select(all_paths) %>% dplyr::rowwise() %>% mutate(
    all_uuids = unlist(str_split(all_paths,"/"))[[4]]
  )


  if(filter == T){filtered_uuid <- (data %>% dplyr::filter(!!sym(filter_column) == filter_choice))[[uuid_col_name]]}
  if(filter != T){filtered_uuid <- data[[uuid_col_name]]}


  all_uuid_df_filtered <- all_uuid_df %>% dplyr::filter(all_uuids %in% filtered_uuid)




  audit_yes <- list()

  for(i in unique(all_uuid_df_filtered$all_paths)){
    print(i)
    uuid <- ((all_uuid_df_filtered %>% dplyr::filter(all_paths ==i))$all_uuids)[1]


    audit_yes[[uuid]] <- read.table(unz(audit_zip_path,i),header=T, quote="\"", sep=",")
  }


  # names(audit_yes)<-all_uuid_df_filtered$all_uuids



  ################## END::reading audit file #########################################


  retur_list <- list()
  dfl<-list()
  for (i in 1: length(audit_yes)){

    d<-audit_yes[[i]]

    print(names(audit_yes)[i])


    d$node<-gsub("\\[1]","",d$node)
    d$node<-gsub("\\[2]","",d$node)
    d$node<-gsub("\\[3]","",d$node)


    df_start_subset <- d[grepl(paste0("\\/(?:",start_question,")$"), d$node),]


    start_question_df <- df_start_subset %>% dplyr::select(start)
    start_question_df<-min(start_question_df$start)


    df_end_subset <- d[grepl(paste0("\\/(?:",end_question,")$"), d$node),]



    end_question_df <- df_end_subset %>% dplyr::select(end)
    end_question_df<-max(end_question_df$end)

    duration_ms<-end_question_df-start_question_df
    duration_secs<-duration_ms/1000
    duration_minutes<- round(duration_secs/60,1)
    dfl[[i]]<-data.frame(uuid=names(audit_yes)[i],
                         duration_ms=duration_ms,
                         durations_secs=duration_secs,
                         duration_minutes= duration_minutes)
  }

  duration_df2<-do.call("rbind", dfl)

  return(duration_df2)

}
