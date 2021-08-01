#'
#' This function provide excel file as REACH format
#'
#' @param write_list a list file (can be compile with single or multiple dataframe)
#' @param output_path Path for output file
#' @param cols_for_color Column name in the dataframe which should be use for colorizing the cell. The default is null.
#' @return Nicely formatted excel file
#' @export
#'



write_excel_as_reach_format <- function(write_list,output_path,cols_for_color = NULL){


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

    if(!is.null(cols_for_color)){
      u = unique(dataset[[cols_for_color]])

      for(x in u){
        y = which(dataset[[cols_for_color]] == x)

        random.color <- randomColor(1, luminosity = "light")

        style <- createStyle(fgFill=random.color,
                             fontSize = 11,
                             fontName = "Arial Narrow",
                             border = "TopBottomLeftRight ",
                             borderColour = "#4F81BD",
                             valign = "center",
                             halign = "left")


        addStyle(wb, sheet = i, style, rows = y+1, cols = 1:ncol(dataset), gridExpand = TRUE)



      }


    }

  }


  saveWorkbook(wb, file = output_path, overwrite = TRUE)

}

