#'
#' Function for one way ANOVA/T/Chi-square test
#'
#'
#' @param data Data
#' @param kobo_survey_sheet Questions/survey sheet from the kobo tool. It should be a data frame.
#' @param variable_1 Mostly independent variable (It might be the dis/aggregation level). It should be a character vector.
#' @param target_Variables Mostly dependent variables (On which variables you want to apply the statistical test)
#' @param ci Confidence interval. The default is .95
#' @param simulate.p.value A logical vector indicating whether to compute p-values by Monte Carlo simulation. This parameter only applied to Chi square test. TRUE is default. FLASE is preferable when you have large data set.
#' @details
#' Matrix for Statistical test (when to use what test)
#'\tabular{rrrrr}{
#'   \strong{} \tab \strong{Categorical} \tab \strong{Numerical}  \cr
#'   \strong{Categorical} \tab Chi Square Test \tab T test: When there is only 2 groups/ ANOVA: When there is more than 2 groups \cr
#'   \strong{Numerical} \tab Regression (Not included in this function) \tab Perason's Correlation test
#' }
#'
#' @export

statistical_test<- function(data,kobo_survey_sheet,variable_1,target_Variables,ci = .95,simulate.p.value = T){
  sl <- (1-ci)

  ######################################### start::define class ###################################################
  survey_select_one <- survey %>% select(type,name) %>% mutate(
    only_type = case_when(grepl("select_one",type)~"select_one",
                          T~type),
    class = case_when(only_type == "integer" ~ "numerical",
                      T~"categorical")
  ) %>% filter(only_type %in% c("select_one","integer"))


  survey_select_multiple <- auto_sm_parent_child(df = data,sm_sep = ".") %>% rename(
    name = sm_child) %>% mutate(
      class = "categorical",
      only_type = "select_multiple"
    )

  survey_only_needed_Variable <- survey_select_one %>% bind_rows(survey_select_multiple) %>% select(name,type,class)
  ##########################################End::define class######################################################



  all_test_result <- list()
  for (i in target_Variables) {

    check_class <-(survey_only_needed_Variable %>% filter(name ==i))$class

    ##################### Start::Chi square test#######################################

    if(check_class == "categorical" & length(unique(data[[i]])[!is.na(unique(data[[i]]))]) >1 &
       length(unique(data[[variable_1]])[!is.na(unique(data[[variable_1]]))])  >1 ){
      #Chi square TEST
      df_for_test <- data %>% select(variable_1,i)
      df_for_test[[i]] <- factor(df_for_test[[i]])
      test_result <- chisq.test(df_for_test[[variable_1]],df_for_test[[i]],correct = F,simulate.p.value	=simulate.p.value)

      all_test_result[[i]] <- data.frame(
        independent_variable = variable_1,
        dependent_variable = i,
        statistic = test_result$statistic,
        degree_of_freedom = test_result$parameter,
        p_value = round(test_result$p.value,3),
        methods = test_result$method) %>% mutate(
          summray = case_when(p_value < sl ~ "[H1] Independent and dependent variables are related to each other",
                              p_value > sl ~ "[H0] Independent and dependent variables are NOT related to each other (The two variables are independent.)")
        )
    }

    ##################### Start::Z/T and ANOVA test#######################################
    if(check_class == "numerical"& length(unique(data[[i]])[!is.na(unique(data[[i]]))]) >1 &
       length(unique(data[[variable_1]])[!is.na(unique(data[[variable_1]]))])  >1 ){



      # Z/T test ----------------------------------------------------------------

      if(length(unique(data[[variable_1]])) <3){


        # T test ------------------------------------------------------------------

        # t test
        # A t-test is a type of inferential statistic used to determine if there is a significant difference between the means of two groups, which may be related in certain features.

        t_test_re <-t.test(data[[i]]~ data[[variable_1]],conf.level = ci)

        all_test_result[[i]] <- data.frame(
          independent_variable = variable_1,
          dependent_variable = i,
          statistic = t_test_re$statistic,
          degree_of_freedom = t_test_re$parameter,
          p_value = round(t_test_re$p.value,3),
          lower=  t_test_re$conf.int[1],
          upper =  t_test_re$conf.int[2],
          methods = t_test_re$method) %>% mutate(
            summray = case_when(p_value < sl ~ "[H1] There is a significant diffence between two variables",
                                p_value > sl ~ "[H0] There is NO significant diffence between two variables")
          ) %>% select(-lower,-upper)


      } ### end uniques(data[[variable_1]]) <3

      if(length(unique(data[[variable_1]])) >2){

        #Anova test
        ## hypothesis

        # H0 = There is no difference between the groups and equality between means
        # H1 = There is a difference between the means and groups

        aov_test <-aov(data[[i]] ~ factor(data[[variable_1]]))
        pairwise_anova <- TukeyHSD(aov_test,conf.level = ci)
        pairwise_anova_df <- pairwise_anova[1] %>% as.data.frame()

        pairwise_anova_final <- tibble::rownames_to_column(pairwise_anova_df, "relation_variable")
        names(pairwise_anova_final) <- names(pairwise_anova_final) %>%  str_sub(-5,-1)
        all_test_result[[i]] <- pairwise_anova_final %>% rename(
          p_value = p.adj,
          relation_variable=iable,
          degree_of_freedom=.diff,
          lower = ..lwr,
          upper = ..upr
        ) %>% mutate(
          independent_variable = variable_1,
          dependent_variable = i,
          methods = "One way ANOVA",
          summray = case_when(p_value < sl ~ "[H1] There is a difference between the means and groups",
                              p_value > sl ~ "[H0] There is no difference between the groups and equality between means")
        ) %>% select(independent_variable,dependent_variable,relation_variable,degree_of_freedom,p_value,summray,lower,upper,everything()) %>% select(-lower,-upper)

      }### end uniques(data[[variable_1]]) >2


    } ### end check_class == "numerical"
  }### end for loop



  result <- do.call("bind_rows",all_test_result)
  rownames(result) <-NULL
  return(result )

}
