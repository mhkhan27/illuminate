#' Credit goes to Zack Arno
#' Get NA percentages and frequency for each column in the data set
#' @param data The dataset (data.frame).
#' @return frequency and proportion table of NA rates.
#' @export

get_na_response_rates<-function(data){
  na_count_per_question<-sapply(data, function(y) sum(length(which(is.na(y)))))
  na_percent_per_question <-sapply(data, function(y) ((sum(length(which(is.na(y)))))/nrow(data))*100)
  non_response_df<-data.frame(num_non_response=na_count_per_question,perc_non_response= na_percent_per_question)
  non_response_df1<-non_response_df %>%
    mutate(question=rownames(.)) %>%
    dplyr::select(question, everything()) %>%
    arrange(num_non_response, question)
}






#' @name auto_detect_sm_parents
#' @rdname auto_detect_sm_parents
#' @title Detect select multiple parent columns
#'
#' @description `auto_detect_sm_parents` is mean to detect select multiple parent columns in a way that does
#' not rely on the XLSForm as the input
#'
#' @param df a survey object or dataframe
#' @param sm_sep select multiple parent child separator. This is specific for XLSForm data (default = /).
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a list of select multiple parent columns in data set.
#'
#'
#' @export
auto_detect_sm_parents<- function(df, sm_sep="."){
  sm_parents<-sub(glue::glue('.[^\\{sm_sep}]*$'), '', colnames(df))
  sm_parents<-data.frame(col_names=sm_parents[sm_parents!=""])
  select_multiple_detected<-sm_parents %>%
    group_by(col_names) %>%
    summarise(n=n()) %>%
    filter(n>1) %>%
    select(col_names)
  return(as.character(select_multiple_detected$col_names))

}
#' @name auto_sm_parent_child
#' @rdname auto_sm_parent_child
#' @title detect and group together select multiple parent and children columns
#' @description `auto_sm_parent_child` is mean to detect select multiple parent columns & children columns in a way that does
#' not rely on the XLSForm as the input
#' @param df a survey object or dataframe
#' @param sm_sep select multiple parent child separator. This is specific for XLSForm data (default = /).
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a data frame containing the the child select multiple columns alongside there parents
#' @export


auto_sm_parent_child<- function(df, sm_sep="."){
  sm_parents<-auto_detect_sm_parents(df, sm_sep)
  sm_child<- df %>%
    select(starts_with(glue::glue("{sm_parents}{sm_sep}"))) %>%
    colnames()
  tibble(
    sm_parent=sub(glue::glue('.[^\\{sm_sep}]*$'),'',sm_child),
    sm_child
  )

}


#' @name survey_collapse_binary_long
#' @rdname survey_collapse_binary_long
#' @title Collapse logical binary columns into tidy long format
#'
#' @description `survey_collapse_binary_long()` uses the srvyr [srvyr::survey_mean] & survey package [survey::svymean]   methods
#' to collapse/or aggregate binary logical data. This function can be used on its own, but was build mainly to for its use in [butteR::survey_collapse]
#' which is meant to help batch analyze data
#'
#' @param dfsvy a survey or preferably srvyr object
#' @param x columns to collapse
#' @param disag the columns to collapse/ subset by(analagous to [[dplyr::group_by]] to [[dplyr::summarise]]) flow
#' @param na_val if you want NA replaced by value. By default NA values will be removed prior to aggregation. It is recommended
#' that you do not adjust this value and deal with na values as a separate step
#' @param sm_sep select multiple parent child separator. This is specific for XLSForm data (default = /).

#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a long format data frame containing the collapsed data.
#'
#'
#' @export



survey_collapse_binary_long<- function(df,
                                       x,
                                       disag=NULL,
                                       na_val=NA_real_,
                                       sm_sep="/" ) {
  if(is.na(na_val) & !all(!is.na(df$variables[[x]]))){
    df<-df%>%
      filter(!is.na(!!sym(x)))
  }
  if(!is.na(na_val)){
    df<-df %>%
      mutate(
        !!x:=ifelse(is.na(x), na_val,x)
      )
  }
  if(!is.null(disag)){
    disag_syms<-syms(disag)
    df<-df %>%
      group_by(!!!disag_syms)
    df_n<-df

    if(is.logical(df$variables[[x]])) {
      df_n<-df %>%
        group_by(!!!disag_syms,!!x:=factor(!!sym(x)),.drop=FALSE)
      vec_n<-df_n %>%
        summarise(n_unweighted= unweighted(n())) %>%
        filter(!!sym(x)==T) %>%
        pull(n_unweighted)
    }
    if(!is.logical(df$variables[[x]])) {
      df_n<-df %>%
        group_by(!!!disag_syms,.drop=FALSE)
      vec_n<-df_n %>%
        summarise(n_unweighted= unweighted(n())) %>%
        pull(n_unweighted)


    }
  }
  if(is.null(disag)){
    if(is.logical(df$variables[[x]])) {
      df_n<-df %>%
        group_by(!!sym(x),.drop=F)

      vec_n<-df_n %>%
        summarise(n_unweighted= unweighted(n())) %>%
        filter(!!sym(x)==T) %>%
        pull(n_unweighted)}
    if(!is.logical(df$variables[[x]])){
      vec_n<-df %>%
        mutate(!!x := !is.na(!!sym(x))) %>%
        summarise(n_unweighted= unweighted(n())) %>%
        pull(n_unweighted)
    }
    subset_names<- "dummy"
    subset_vals<- "dummy"
  }

  if(length(vec_n)==0){
    vec_n<-0
  }


  res<-df %>%
    summarise(
      `mean/pct`=survey_mean(!!sym(x),na.rm=TRUE,vartype="ci"),
    ) %>%
    mutate(variable_val=x) %>% # mean for intger
    cbind(n_unweighted=vec_n)


  if(!is.null(disag)){
    class(disag)
    subset_names<- glue::glue("subset_{1:length(disag)}_name")
    subset_vals<- glue::glue("subset_{1:length(disag)}_val")
    # res<-
    res<-  res %>%
      rename_at(.vars = disag,
                .funs = function(x) glue::glue("subset_{1:length(x)}_val")) %>%
      mutate_key_pair(names =subset_names,values = disag ) %>%
      mutate_at(
        .vars = subset_vals,.funs = function(x)as.character(x)
      )
    # res<-res %>%
    #   pivot_longer(disag,
    #                names_to="subset_name",
    #                values_to= "subset_value") %>%
    #   mutate(subset_value=as.character(subset_value))


  }
  res %>%
    mutate(variable=sub(glue::glue('.[^\\{sm_sep}]*$'), '',
                        variable_val)) %>%
    select(any_of(c ("variable",
                     "variable_val",
                     as.character(subset_names),
                     as.character(subset_vals))),
           everything())
  # dplyr::select(any_of(
  #   c("variable","variable_value","subset_name", "subset_value")
  # ),
  # everything())


}




#' @name survey_collapse_categorical_long
#' @rdname survey_collapse_categorical_long
#' @title Collapse categorical data into tidy long format
#'
#' @description `survey_collapse_categorical)long()` uses the srvyr [srvyr::survey_mean] & survey package [survey::svyciprop]   methods
#' to collapse/or aggregate cateogrical data. This function can be used on its own, but was build mainly to for its use in [butteR::survey_collapse]
#' which is meant to help batch analyze data
#'
#' @param dfsvy a survey or preferably srvyr object
#' @param x columns to collapse
#' @param disag the columns to collapse/ subset by(analagous to [[dplyr::group_by]] to [[dplyr::summarise]]) flow
#' #' @param na_val if you want NA replaced by value. By default NA values will be removed prior to aggregation. It is recommended
#' that you do not adjust this value and deal with na values as a separate step
#' @param sm_sep select multiple parent child separator. This is specific for XLSForm data (default = /).
#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a long format data frame containing the collapsed data.
#'
#'
#' @export

survey_collapse_categorical_long<- function(df, x,disag=NULL,na_val=NA_character_) {
  if(is.na(na_val)){
    df<- df %>%
      filter(!is.na(!!sym(x)))
  }
  if(!is.na(na_val)){
    df %>%
      mutate(
        !!x:=ifelse(is.na(x), na_val,x)
      )
  }

  if(!is.null(disag)){
    group_by_vars<-syms(c(disag,x))
  }else{
    group_by_vars<-syms(c(x))
  }

  df<-df %>%
    group_by(!!!group_by_vars,.drop=F)
  res<-df %>%
    summarise(
      `mean/pct`=survey_mean(na.rm=TRUE,vartype="ci"),
      n_unweighted= unweighted(n())
    ) %>%
    mutate(variable=x) %>%
    rename(variable_val=x)



  if(!is.null(disag)){
    subset_names<- glue::glue("subset_{1:length(disag)}_name")
    subset_vals<- glue::glue("subset_{1:length(disag)}_val")
    res<- res %>%
      rename_at(.vars = disag,
                .funs = function(x) glue::glue("subset_{1:length(x)}_val")) %>%
      mutate_key_pair(names =subset_names,values = disag ) %>%
      mutate_at(
        .vars = subset_vals,.funs = function(x)as.character(x)
      )


  }
  res %>%
    select(any_of(c ("variable",
                     "variable_val",
                     "subset_names", "subset_vals")),
           everything())
}

#' @name survey_analysis
#' @rdname survey_analysis
#' @title Batch Collapse Survey Data into tidy long format
#'
#' @description `survey_analysis` uses the srvyr [srvyr::survey_mean] & survey package [survey::svymean]   methods
#' to collapse/or aggregate survey data. This function uses `survey_collapse_categorical_long` and `survey_collapse_binary_long`
#' to perform the batch analysis. This function is extracted from butteR
#'
#' @param df a survey or preferably srvyr object
#' @param vars_to_analyze columns to collapse
#' @param disag the columns to collapse/ subset by(analagous to [[dplyr::group_by]] to [[dplyr::summarise]]) flow
#' @param na_val if you want NA replaced by value. By default NA values will be removed prior to aggregation. It is recommended
#' that you do not adjust this value and deal with na values as a separate step
#' @param sm_sep select multiple parent child separator. This is specific for XLSForm data (default = /).
#' @param kobo_path kobo tool path
#' @param question_lable A logical variable. Select TRUE if label from kobo is necessary in the analysis.

#'  If using read_csv to read in data the separator will most likely be '/' where as if using read.csv it will likely be '.'
#' @return a long format data frame containing the collapsed data.
#'
#'
#' @export


survey_analysis<-function(df,
                           weights = F,
                           weight_column =NULL,
                           strata ,
                           vars_to_analyze=NULL,
                           disag=NULL,
                           na_val,
                           sm_sep="/",
                           question_lable = F,
                           kobo_path = NULL){



  if(!is.null(vars_to_analyze)) {vars_to_analyze <- vars_to_analyze[vars_to_analyze %in% names(df)]}
  if(is.null(vars_to_analyze)) {vars_to_analyze <- names(df)}


  if(!is.null(weight_column)) {vars_to_analyze <- vars_to_analyze[!vars_to_analyze %in% weight_column]}


  if(weights == T){
    df <- as_survey(df,strata = strata, weight = weight_column)
  }

  if(weights == F){
    df <- as_survey(df)
  }

  sm_parent_child_all<-auto_sm_parent_child(df$variables)
  sm_parent_child_vars<- sm_parent_child_all %>%
    filter(sm_parent %in% vars_to_analyze)
  not_sm<-vars_to_analyze[!vars_to_analyze %in% sm_parent_child_vars$sm_parent]
  vars_to_analyze<- c(not_sm, sm_parent_child_vars$sm_child)



  ######### check calculation type #############################################

  calculation_type <- lapply(df$variables,class) %>% as.data.frame()
  calculation_type <- calculation_type %>%
    pivot_longer(cols = names(calculation_type),
                 names_to = "main_variable",values_to = "type") %>% mutate(
                   type = case_when(main_variable %in% sm_parent_child_all$sm_child ~ "logical", T~ type)
                 ) %>% mutate(analysis_type = case_when(type %in% c("numeric","integer") ~ "mean",
                                                        type == "logical" ~ "prop_select_multiple",
                                                        T~ "prop_select_one")) %>% select(-type)


  #########33


  ############################# NA RESPONSE #######################################################################################

  na_response_rate <-get_na_response_rates(df$variables)


  response_rate <- na_response_rate %>% mutate(
    response_count = nrow(df$variables)- num_non_response
  )

  response_rate <- dplyr::filter(response_rate ,!grepl('\\.',question)) %>% tibble() %>% dplyr::select(question,response_count)

  ##############################################################################################################



  if(!is.null(disag)){
    vars_to_analyze<-setdiff(vars_to_analyze,disag )
  }

  res_list<-list()
  for(i in vars_to_analyze){
    print(i)
    if(is.character(df$variables[[i]])|is.factor(df$variables[[i]])){
      res_list[[i]]  <-survey_collapse_categorical_long(df = df,
                                                        x = i,
                                                        disag = disag,
                                                        na_val = NA_character_
      )
    }
    if(is.logical(df$variables[[i]])|is.numeric(df$variables[[i]])){
      res_list[[i]]  <-survey_collapse_binary_long(df = df,
                                                   x = i,
                                                   disag = disag,
                                                   na_val = NA_real_,
                                                   sm_sep = sm_sep
      )
    }

  }
  output_result<- bind_rows(res_list) %>%  tidyr::separate(variable_val,
                                                           c("question", "options"),sep = "\\.",
                                                           extra='merge') %>% mutate(
                                                             main_variable = case_when(is.na(variable)| variable == ""  ~question, T ~ variable),
                                                             choice = case_when(!is.na(options)|options!= ""~ options,T~question),
                                                             choice = case_when(main_variable == choice ~ NA_character_, T~ choice)
                                                           ) %>% select(main_variable,choice,everything()) %>% select(-variable,-question,-options)


  if(question_lable == T) {
    read_sheets(kobo_path)
    survey <- survey %>% select(name,starts_with("label::"))
    choices <- choices  %>% select(name,starts_with("label::"))%>% distinct(name,.keep_all = T)
    names(choices) <- paste0("choice_", names(choices))
    output_result<- output_result %>% left_join(survey,by = c("main_variable"= "name")) %>%
      left_join(choices,by= c("choice"="choice_name")) %>% select(main_variable,starts_with("label::"),choice,starts_with("Choice_label"),everything())

    output_result <- output_result %>%  left_join(response_rate,by =c("main_variable"="question"))
  }

  if(question_lable == F) {
    output_result <- output_result %>%  left_join(response_rate,by =c("main_variable"="question"))
  }


  if(!is.null(disag)){

    main_variable_list <- output_result$main_variable %>% unique()


    count_by_location  <- list()

    for(i in main_variable_list){

      df_2 <-  df$variables %>% dplyr::select(i,disag) %>% dplyr::filter(!is.na(df$variables[i]))

      count_by_location[[i]] <- df_2 %>% dplyr::group_by(!!!syms(disag)) %>% dplyr::summarise(
        count_by_subset = n()) %>% mutate(
          main_variable = i
        )

    }

    count_by_location_df <- do.call("bind_rows",count_by_location)


    for(i in 1:length(disag)){
      new_col_name <- paste0("subset_",i,"_val")
      count_by_location_df <- count_by_location_df %>% rename(
        !!new_col_name:= disag[[i]]
      )


    }

    count_by_location_df <- count_by_location_df %>% select(main_variable,count_by_subset,starts_with("subset_"))

    output_result <- output_result %>% left_join(count_by_location_df) %>% distinct()

    output_result <- output_result  %>% left_join(calculation_type) %>% mutate(
      analysis_type = case_when(is.na(choice) ~ "mean",
                                main_variable %in% sm_parent_child_all$sm_parent ~ "prop_select_multiple",
                                T~ analysis_type)
    )

    if(length(disag) == 1){output_result <-  output_result %>% mutate(key_index = paste0(analysis_type," @/@ ", main_variable," ~/~ ",choice, " @/@ ",
                         subset_1_name, " ~/~ " , subset_1_val))}

    if(length(disag) == 2){output_result <-  output_result %>% mutate(key_index = paste0(analysis_type," @/@ ", main_variable," ~/~ ",choice, " @/@ ",
                                                                                         subset_1_name, " ~/~ " , subset_1_val, " ~/~ ",subset_2_name, " ~/~ " , subset_2_val))}


    if(length(disag) == 3){output_result <-  output_result %>% mutate(key_index = paste0(analysis_type," @/@ ", main_variable," ~/~ ",choice, " @/@ ",
                                                                                         subset_1_name, " ~/~ " , subset_1_val, " ~/~ ",subset_2_name, " ~/~ " , subset_2_val,
                                                                                        " ~/~ ",subset_3_name, " ~/~ " , subset_3_val))}


    if(length(disag) == 4){output_result <-  output_result %>% mutate(key_index = paste0(analysis_type," @/@ ", main_variable," ~/~ ",choice, " @/@ ",
                                                                                         subset_1_name, " ~/~ " , subset_1_val, " ~/~ ",subset_2_name, " ~/~ " , subset_2_val,
                                                                                         " ~/~ ",subset_3_name, " ~/~ " , subset_3_val,
                                                                                         " ~/~ ",subset_4_name, " ~/~ " , subset_4_val))}

    if(length(disag) == 5){output_result <-  output_result %>% mutate(key_index = paste0(analysis_type," @/@ ", main_variable," ~/~ ",choice, " @/@ ",
                                                                                         subset_1_name, " ~/~ " , subset_1_val,
                                                                                         " ~/~ ",subset_2_name, " ~/~ " , subset_2_val,
                                                                                         " ~/~ ",subset_3_name, " ~/~ " , subset_3_val,
                                                                                         " ~/~ ",subset_4_name, " ~/~ " , subset_4_val,
                                                                                         " ~/~ ",subset_5_name, " ~/~ " , subset_5_val))}


    output_result <- output_result %>% relocate(analysis_type,.after = last_col()) %>% relocate(key_index,.after = last_col())

  }


  if(is.null(disag)){
    output_result <- output_result  %>% left_join(calculation_type) %>% mutate(
      analysis_type = case_when(is.na(choice) ~ "mean",
                                main_variable %in% unique(sm_parent_child_all$sm_parent) ~ "prop_select_multiple",
                                T~ analysis_type)
    ) %>%  relocate(response_count, .after = last_col())

    output_result <-  output_result %>% mutate(
      key_index = paste0(analysis_type," @/@ ", main_variable," ~/~ ",choice, " @/@ ",
                         "NA ~/~ NA")
    ) %>% relocate(analysis_type,.after = last_col()) %>% relocate(key_index,.after = last_col())
    }

  output_result
}



#' @name mutate_key_pair
#' @rdname mutate_key_pair
#' @title Mutate columns on based on a list of names and values
#'
#' @description conditionally mutate on columns based
#' on a list of column names and values. This is mostly useful for conditional
#' mutate commands and can currently only mutate uniform columns.
#' It is used inside the survey collapse functions
#'
#' @param df dataframe
#' @param names names of columns to mutate
#' @param values uniform values to mutate


mutate_key_pair<- function(df, names, values){
  df %>%
    tibble::add_column(!!!set_names(as.list(values),nm=names))
}
