#'
#' List of logical issues in the data set
#'
#' @param df Data frame
#' @param uuid UUID column name in the data frame.
#' @param logic_list_df Data frame with a list of checks. This must include a column filled with all logic for the checks.
#' @param logic_col_name Name of the column of logic_list_df data frame where all the logic are listed
#' @param add_description TRUE [default ], if you want to add a description for each logic.
#' @param col_name_issue Name of the column of logic_list_df data frame where the description for each logic is listed
#' @return A data frame where all the issues are listed as a cleaning log format
#' @export
#'

logical_check <- function(df,uuid,logic_list_df,logic_col_name,add_description =T,col_name_issue) {


  issue <- list()

  for(i in 1:length(logic_list_df[[logic_col_name]])) {

    # Find out the column names that listed in logic

    to_replace = logic_list_df[[i,paste0(logic_col_name)]]
    word_list <- to_replace %>% strsplit( " ")  %>% dput()
    word_list_df <- word_list  %>% as.data.frame()
    word_in_the_logic <- word_list_df[[1]] %>% dput()


    cols_name_in_the_logic <- word_in_the_logic[word_in_the_logic %in% names(df)]

    print(cols_name_in_the_logic)
    column_to_report <- c(uuid,cols_name_in_the_logic)

    ########

    df_filtered<- df %>% filter(!! rlang::parse_expr(logic_list_df[[i,paste0(logic_col_name)]])) %>% select(column_to_report)
    df_filtered <- df_filtered %>% mutate_all(.,as.character)

    df_filterd_pl <- df_filtered %>% pivot_longer(cols = cols_name_in_the_logic,names_to = "question",values_to = "old_value")

    if(add_description ==T){
      issue[[paste0(i)]]<- df_filterd_pl %>% mutate( issue = logic_list_df[[i,paste0(col_name_issue)]])
    }
    if(add_description ==F){
      issue[[paste0(i)]]<- df_filterd_pl
    }


  }

  return(do.call("bind_rows",issue))



}








