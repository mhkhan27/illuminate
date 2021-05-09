#'
#' read all the sheets from a Excel file
#'
#'
#' @param dataset_path location of the dataset
#' @param kobo_tool_path location of the kobo tool
#' @return All of the sheet as individual dataframe
#' @export
#'

read_all_sheet_as_csv_format <- function(dataset_path){
  sheet_name <- excel_sheets(dataset_path)
  df_all <- list()
  for (i in sheet_name) {
    assign(i,read_xlsx(dataset_path,sheet = i))
    df <- get(i)
    colnames(df) <- colnames(df) %>% str_replace_all("/",".")
    df_st_with <- df %>% select(starts_with("_")) %>% names()
    df <- df %>% rename_at(df_st_with,~paste0("X",.))
    df_all[[i]] <- df
    assign(i,df)
  }
  list2env(df_all,.GlobalEnv)
}







read_data_and_make_interger<- function(dataset_path,kobo_tool_path){

  dataset_name <- excel_sheets(dataset_path)
  read_all_sheet_as_csv_format(dataset_path)

  read_all_sheet_as_csv_format(kobo_tool_path)


  for(d in dataset_name){

    #### select_mutiple
    select_multiple_cols <- (survey %>% filter(grepl("select_multiple",type)))$name
    data_set_multiple_col <- select_multiple_cols[select_multiple_cols %in% names(get(d))]

    for (i in data_set_multiple_col) {
      ind_cols <-get(d) %>%  select(starts_with(paste0(i,"."))) %>% names()

      df <- get(d) %>% mutate_at(ind_cols,as.numeric)
      assign(d,df)

    }

    #select interger

    select_int_cols <- (survey %>% filter(grepl("integer",type)))$name
    data_se_int_col <- select_int_cols[select_int_cols %in% names(get(d))]

    for (i in data_se_int_col) {

      df <- get(d) %>% mutate_at(i,as.numeric)
      assign(d,df)
    }
  }
}




