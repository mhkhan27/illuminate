#' Use to merge KMLs
#' @param input_folder Input folder from where KMLs will be merged.
#' @export


merge_kml <- function(input_folder){

fl <- list.files(input_folder,full.names = T)
kml <- list()
for (i in  1:length(fl)){
  kml[[i]] <- st_read(fl[i])}
a<- do.call("rbind",kml)
return(a)
}


