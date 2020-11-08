#' Generate Frequency Table For Each Choices
#' @param df The data set
#' @param aggregation_level Aggregation level(s) to analyze data by. The default (NULL) will fully aggregate data to provide one value per variable. Argument also accepts vecors to aggregate upto two variables.
#' @param variables_to_analyze Vector containing all variable names to analyze and include in summary table.
#' @param kobo_servey_sheet Output folder in where converted KMLs will be saved.The defult (NUll) will create additional table for multiple choice questions
#' @export

survey_frequency_by_choices <-function(df,aggregation_level=NULL,variables_to_analyze,kobo_servey_sheet=NULL){

all_na <- function(x) any(!is.na(x))
df_interger_cols <- df %>% select_if(~is.integer(.)) %>% names()
df_numeric_cols <- df %>% select_if(~is.numeric(.))  %>% names()

df_interger_cols <- c(df_interger_cols, df_numeric_cols )
df_interger_cols <- df_interger_cols[df_interger_cols %in% variables_to_analyze ]

if(!is.null(kobo_servey_sheet)){

select_mutiple_from_kobo <- kobo_servey_sheet %>% dplyr::filter(str_detect(type,"select_multiple"))
integer_from_kobo <- kobo_servey_sheet %>% dplyr::filter(str_detect(type,"integer"))

select_mutiple_from_kobo_cols <-select_mutiple_from_kobo$name
interger_from_kobo_cols <-integer_from_kobo$name


df <- df %>% dplyr::select(-select_mutiple_from_kobo_cols)

}

df_count <- list()
df_int_count <- list()
df_count2 <- list()
df_int_count2 <- list()


if (length(aggregation_level)==1) {

for(x in unique(df[[aggregation_level]])) {

df2 <- df %>% dplyr::filter(get(aggregation_level) == x)
df2 <- df2 %>% dplyr::select_if(all_na)

colname_in_filter_df <- df2 %>% names()

cols_to_analyze <- variables_to_analyze[variables_to_analyze %in% colname_in_filter_df]
cols_to_analyze <- cols_to_analyze[!cols_to_analyze %in% df_interger_cols]

for(i in cols_to_analyze) {
  df_count[[i]] <- df2 %>% dplyr::select(i) %>% AMR::freq(i) %>% as.data.frame() %>%
    dplyr::select(item,count) %>% pivot_wider(id_cols = c("item","count"),
    names_from = "item",names_prefix = paste0(i,"."), values_from = "count")
}

df_count2[[x]]<- do.call("bind_cols",df_count) %>% dplyr::mutate(
  !!aggregation_level := paste0(x))

for(j in df_interger_cols) {
df_int_count[[j]] <- length(df2[[j]][!is.na(df2[[j]])])
}

df_int_count2[[x]] <- do.call("bind_cols",df_int_count) %>% dplyr::mutate(
  !!aggregation_level := paste0(x))
}

join_df_character <- do.call("bind_rows",df_count2) %>% dplyr::select(paste0(aggregation_level),everything())
join_df_interger <- do.call("bind_rows",df_int_count2) %>% dplyr::select(paste0(aggregation_level),everything())
join_df <- join_df_character %>% left_join(join_df_interger)
}


if (length(aggregation_level)>1) {

df$aggre_colum <- paste0(df[[aggregation_level[1]]],".",df[[aggregation_level[2]]])

  for(x in unique(df$aggre_colum)) {

    df2 <- df %>% dplyr::filter(aggre_colum == x)
    df2 <- df2 %>% dplyr::select_if(all_na)
    colname_in_filter_df <- df2 %>% names()

    cols_to_analyze <- variables_to_analyze[variables_to_analyze %in% colname_in_filter_df]
    cols_to_analyze <- cols_to_analyze[!cols_to_analyze %in% df_interger_cols]

for(i in cols_to_analyze) {
      df_count[[i]] <- df2 %>% dplyr::select(i) %>% AMR::freq(i) %>% as.data.frame() %>%
        dplyr::select(item,count) %>% pivot_wider(id_cols = c("item","count"),names_from = "item",names_prefix = paste0(i,"."), values_from = "count")
  }

    df_count2[[x]]<- do.call("bind_cols",df_count) %>% dplyr::mutate(
      aggre_colum =paste0(x))

for(j in df_interger_cols) {
      df_int_count[[j]] <- length(df2[[j]][!is.na(df2[[j]])])
  }

    df_int_count2[[x]] <- do.call("bind_cols",df_int_count) %>% dplyr::mutate(
      aggre_colum = paste0(x))
}

join_df_character <- do.call("bind_rows",df_count2) %>% dplyr::select(aggre_colum,everything()) %>%
    separate(aggre_colum, c(aggregation_level[1], aggregation_level[2]),extra = "merge")
join_df_interger <- do.call("bind_rows",df_int_count2) %>% dplyr::select(aggre_colum,everything()) %>%
  separate(aggre_colum, c(aggregation_level[1], aggregation_level[2]),extra = "merge")
join_df <- join_df_character %>% left_join(join_df_interger)

}

if (is.null(aggregation_level)) {

  df2 <- df %>% dplyr::select_if(all_na)
  colname_in_filter_df <- df2 %>% names()

  cols_to_analyze <- variables_to_analyze[variables_to_analyze %in% colname_in_filter_df]
  cols_to_analyze <- cols_to_analyze[!cols_to_analyze %in% df_interger_cols]

  for(i in cols_to_analyze) {
    df_count[[i]] <- df2 %>% dplyr::select(i) %>% AMR::freq(i) %>% as.data.frame() %>%
      dplyr::select(item,count) %>% pivot_wider(id_cols = c("item","count"),names_from = "item",names_prefix = paste0(i,"."), values_from = "count")
  }

if(length(df_interger_cols)!=0){
  for(j in df_interger_cols) {
    df_int_count[[j]] <- length(df2[[j]][!is.na(df2[[j]])])
  }
  join_df_character <- do.call("bind_cols",df_count)
  join_df_interger <- do.call("bind_cols",df_int_count)
  join_df <- bind_cols(join_df_character,join_df_interger)
}
  if(length(df_interger_cols)==0){
  join_df <- do.call("bind_cols",df_count)}

}


names(join_df) <- if_else(endsWith(names(join_df),"1.1"),str_replace_all(names(join_df),"1.1","1.yes"),names(join_df))
names(join_df) <- if_else(endsWith(names(join_df),"0.0"),str_replace_all(names(join_df),"0.0","0.no"),names(join_df))

names(join_df) <- if_else(endsWith(names(join_df),".1"),str_replace_all(names(join_df),".1",".yes"),names(join_df))
names(join_df) <- if_else(endsWith(names(join_df),".0"),str_replace_all(names(join_df),".0",".no"),names(join_df))

join_df <- join_df %>% dplyr::select(-starts_with(paste0(aggregation_level,".")))
return(join_df)
}





