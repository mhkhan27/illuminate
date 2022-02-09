#' Bind tables containing cleaning issues
#' @param ... Data sets which should be added in the cleaning log
#' @param spotted_by Name of the person/unit who identified the issue. Default vale is "BGD_Data_Unit"
#' @export

create_cleaning_log <- function(spotted_by ="BGD_Data_Unit",...){
  issue_dfs_all <- list(...)
  return(bind_rows(issue_dfs_all) %>% mutate(
    spotted_by =spotted_by,
    new_value = NA_real_,
    change_type = NA_real_
  ))
}
