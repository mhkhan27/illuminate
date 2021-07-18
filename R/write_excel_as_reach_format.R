write_excel_as_reach_format <- function(write_list,output_path){


  headerStyle <- createStyle(fontSize = 12,
                             fontColour = "#FFFFFF",
                             halign = "center",
                             valign = "center",
                             fontName = "Arial Narrow",
                             textDecoration = "bold",
                             fgFill = "#ee5859",
                             border = "TopBottomLeftRight ",
                             borderColour = "#fafafa",
                             wrapText = T)

  bodyStyle <- createStyle(fontSize = 11,
                           fontName = "Arial Narrow",
                           border = "TopBottomLeftRight ",
                           borderColour = "#4F81BD",
                           valign = "center",
                           halign = "left")




  wb <- createWorkbook()


  number_of_sheet <- length(write_list)

  for(i in 1:number_of_sheet ){

    dataset_name <- names(write_list[i])
    dataset <-  get(dataset_name)


    addWorksheet(wb,dataset_name)
    writeData(wb, sheet = i,dataset, rowNames = F)
    addFilter(wb,sheet =  i, row = 1, cols = 1:ncol(dataset))
    freezePane(wb, sheet = i, firstCol = TRUE, firstRow = T)
    addStyle(wb, sheet = i, headerStyle, rows = 1, cols = 1:ncol(dataset), gridExpand = TRUE)
    addStyle(wb, sheet = i, bodyStyle, rows = 1:nrow(dataset)+1, cols = 1:ncol(dataset), gridExpand = TRUE)
    setColWidths(wb, i, cols = 1:ncol(dataset), widths = 25)
    setRowHeights(wb, i, 1, 20)



  }

  saveWorkbook(wb, file = output_path, overwrite = TRUE)

}









calculateDifferences <- function(data, tool.survey){
  # 1) convert all columns to character
  data <- mutate_all(data, as.character)
  # 2) remove columns that are naturally different in each survey
  cols <- data.frame(column=colnames(data)) %>%
    left_join(dplyr::select(tool.survey, name, type), by=c("column"="name")) %>%
    filter(!(type %in% c("date", "start", "end",
                         "audit", "note", "calculate", "geopoint")) &
             !str_starts(column, "_") &
             !str_detect(column, "/"))
  data <- data[, all_of(cols$column)]
  # 3) convert NA to "NA" and all columns to factor
  data[is.na(data)] <- "NA"
  data <- data %>% mutate_if(is.character, factor)
  # 4) calculate gower distance
  gower_dist <- daisy(data, metric="gower", warnBin=F, warnAsym=F, warnConst=F)
  gower_mat <- as.matrix(gower_dist)
  # 5) convert distance to number of differences and determine closest matching survey
  r <- c()
  for (i in 1:nrow(data)) r <- c(r, sort(gower_mat[i,]*ncol(data))[2])
  # 6) store results
  data[["n.col.not.NA"]] <- rowSums(data!="NA")
  data[["survey.id"]] <- 1:dim(data)[1]
  data[["most.similar.survey"]] <- names(r)
  data[["number.different.columns"]] <- as.numeric(r)
  data <- data %>% arrange(number.different.columns, survey.id)
  return(data)
}










