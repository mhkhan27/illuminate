
#'
#' Weighted pearsons test
#'
#'
#' @param data dataset
#' @param dep_var must be numeric and only one value can be use.
#' @param ind_var must be numeric, multiple values are allowed
#' @param strata Column name in the dataset for strata
#' @param survey_weights Column name in the dataset for survey weights
#' @return outcome of left join based on the primary key
#' @export
#'



weighted_pearson_test<- function(data,dep_var,ind_var,strata,survey_weights){

  data <- data %>% select(dep_var,ind_var,strata,survey_weights)

  data <- data %>% filter(!is.na(data[[dep_var]]))
  data <- data %>% filter(! is.infinite(data[[dep_var]]))


  out_liers_dep <- boxplot.stats(data[[dep_var]])$out
  data <- data %>% filter(!data[[dep_var]] %in% out_liers_dep)


  result_list <- list()

  for (x in  ind_var) {

    out_liers <- boxplot.stats(data[[x]])$out

    df <- data %>% filter(!data[[x]] %in% out_liers)
    df <- df %>% filter(! is.infinite(df[[x]]))

    svy <- as_survey(df,strata = strata,weights = survey_weights)

    text = formula(paste0("~",x, " + " , dep_var))

    result <- jtools::svycor(text,svy,na.rm=T, sig.stats = T,digits = 6)


    correlation_value_df<- result$cors %>% as.data.frame()
    cor_val <- correlation_value_df %>% tidyr::pivot_longer(cols = names(correlation_value_df),names_to = "variable",values_to = "value") %>% mutate(
      value = round(value,6)
    )%>% filter(value != 1)

    correlation_value <- cor_val$value %>% unique()



    #### p value



    p_value_df<- result$p.values %>% as.data.frame()
    p_val <- p_value_df %>% tidyr::pivot_longer(cols = names(p_value_df),names_to = "variable",values_to = "value") %>% mutate(
      value = round(value,10)
    )%>% filter(value != 0)

    if(nrow(p_val) == 0){p_value_t <- .00000001}

    if(nrow(p_val) != 0){p_value_t <- p_val$value %>% unique()}



    ### T value



    t_value_df<- result$t.values %>% as.data.frame()
    t_val <- t_value_df %>% tidyr::pivot_longer(cols = names(t_value_df),names_to = "variable",values_to = "value") %>% mutate(
      value = round(value,6)
    )%>% filter(value != Inf)

    t_value_t <- t_val$value %>% unique()



    result_list[[x]] <- data.frame(
      dep_variable = dep_var,
      ind_variable = x,
      correlation_value = correlation_value,
      p_value = p_value_t,
      t_value = t_value_t
    ) %>%

      mutate(
        H0 = paste0("There is no correlation between ",dep_variable, " and ", x),

        relationship  = case_when(p_value < .05 ~ paste0("The test rejects H0 hence there is significant relationship between ",dep_var," and ",x),
                                  T~ paste0("The test does not reject H0 hence there is NO significant relationship between ",dep_var," and ",x)),

        correlation_type = case_when(correlation_value < 0 ~ "negative",T~"positive"),

        correlation_class = case_when(abs(correlation_value) > .9 ~ "Very high",
                                      abs(correlation_value) > .7 ~ "high",
                                      abs(correlation_value) > .5 ~ "moderate",
                                      abs(correlation_value) > .3 ~ "low",
                                      T~ "negligible ")) %>% mutate(
                                        Interpretation = case_when( p_value > .05 ~ paste0("The test does not reject H0 hence there is NO significant relationship between ",dep_var," and ",x),

                                                                    T~ paste0(relationship, " and there is a ", correlation_class, "(",correlation_type,") relation btween them. " )

                                        )) %>% select(-correlation_type,-correlation_class,-relationship)



  }

  final_result <- do.call("bind_rows",result_list)
  return(final_result)

}




#'
#' Weighted T test
#'
#'
#' @param data dataset
#' @param binary_variable must be binary variable and only one value can be use.
#' @param non_binary_variable must be numeric, multiple values are allowed.
#' @param strata Column name in the dataset for strata
#' @param survey_weights Column name in the dataset for survey weights
#' @return outcome of left join based on the primary key
#' @export
#'

weighted_t_test <- function(data,binary_variable,non_binary_variable,strata,survey_weights){


  data <- data %>% filter(!is.na(data[[binary_variable]]))
  data <- data %>% filter(! is.infinite(data[[binary_variable]]))


  test_result <- list()

  for(i in non_binary_variable){

     out_liers_independent_variable <- boxplot.stats(data[[i]])$out

     if(length(out_liers_independent_variable) > 0){
     df <- data %>% filter(!data[[i]] %in% out_liers_independent_variable)
     }

     if(length(out_liers_independent_variable) == 0){
       df <- data
     }

     df <- df %>% filter(!is.na(df[[i]]))
     df <- df %>% filter(! is.infinite(df[[i]]))

    dfsvy <- as_survey(df,strata = strata,weights = survey_weights)


    f_mula <- formula(paste0(i, "~",binary_variable ))
    test <- survey::svyttest(f_mula,design = dfsvy)



    test_result[[i]] <- data.frame(
      binary_variable = binary_variable,
      non_binary_variable = i,
      t_value = test$statistic %>% as.numeric(),
      p_value = test$p.value %>% as.numeric(),
      df = test$parameter %>% as.numeric(),
      difference_in_mean = test$estimate %>% as.numeric(),
      method = test$method
    )


  }

  final_result <- do.call("bind_rows",test_result)

  return(final_result)


}
