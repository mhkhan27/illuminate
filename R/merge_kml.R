#' Use to merge KMLs
#' @param input_folder Input folder from where KMLs will be merged.
#' @param output_folder Output folder in where merged KML will be saved.
#' @export


merge_kml <- function(input_folder,output_folder){

fl <- list.files(input_folder,full.names = T)
kml <- list()
for (i in  1:length(fl)){
  kml[[i]] <- st_read(fl[i])}
a<- do.call("rbind",kml)
plotKML(sf::as_Spatial(a), paste0(output_folder,"merged_all"))}


