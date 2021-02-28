#'
#' read all the sheets from a Excel file
#'
#'
#' @param xlsx_file_path location of the excel file
#' @return All of the sheet as individual dataframe
#' @export
#'

read_all_sheet_as_csv_format <- function(xlsx_file_path){
  sheet_name <- excel_sheets(xlsx_file_path)
  df_all <- list()
  for (i in sheet_name) {
    assign(i,read_xlsx(xlsx_file_path,sheet = i))
    df <- get(i)
    colnames(df) <- colnames(df) %>% str_replace_all("/",".")
    df_st_with <- df %>% select(starts_with("_")) %>% names()
    df <- df %>% rename_at(df_st_with,~paste0("X",.))
    df_all[[i]] <- df
    assign(i,df)
  }
  list2env(df_all,.GlobalEnv)
}
