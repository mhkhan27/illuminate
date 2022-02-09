#' This function recreates the concerted columns for select multiple questions
#' @param df data frame
#' @param uuid Unique ID column(Default is X_uuid).
#' @export

recalculate_concerted_col_for_select_multiple <- function(df, uuid = "X_uuid"){

  names(df) <- sub("(\\..*?)\\.", "\\1_", names(df))
  names(df) <- sub("(\\..*?)\\.", "\\1_", names(df))
  cols_order <- df %>% names()

  select_multiple <- auto_sm_parent_child(df)

  select_multiple_list <- list()
  for (i in select_multiple$sm_parent) {
    select_multi_single <- select_multiple %>% filter(sm_parent == i)
    concat_col <- select_multi_single$sm_parent %>% unique()
    choice_cols <- select_multi_single$sm_child %>% unique()

    df_only_cols <- df %>% select(choice_cols,uuid)

    pivot_long <-  df_only_cols

    final_df <- pivot_long %>% pivot_longer(cols = !uuid,names_to = "cols" ,values_to = "value") %>%
      filter(value == 1 | value == TRUE | value == "1" | value == "TRUE") %>% group_by(!!sym(uuid)) %>% summarise(
        !!sym(concat_col):= paste0(cols,collapse = ",")
      )

    final_df[[concat_col]] <- final_df[[concat_col]] %>% str_replace_all(paste0(concat_col,"."),"")

    select_multiple_list[[concat_col]] <- final_df
  }

  final_df_for_export<- purrr::reduce(select_multiple_list, dplyr::full_join, by = uuid)
  concat_col_names_from_fina_export <-  final_df_for_export %>% select(-uuid) %>% names()

  data_with_fix_concat <- df %>% select(-concat_col_names_from_fina_export) %>%
    left_join(final_df_for_export) %>% select(cols_order)

  return(data_with_fix_concat)
}
