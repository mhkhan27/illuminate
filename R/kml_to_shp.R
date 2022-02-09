#' Use to convert KMLs to shapefiles
#' @param input Input folder from where KMLs will be converted to shapefile.
#' @param output Output folder in where converted KMLs will be saved.
#' @export

kml_to_shp <- function(input, output){

fl = list.files(input, full.names = TRUE, pattern = '.kml')
file_names<-list.files(input, full.names = FALSE, pattern = '.kml')
new_file_naes<- stringr::str_replace_all(file_names,c('.kml'=""))
new_file_naes<- paste0(new_file_naes,".shp")


for (i in  1: length(fl)){
kml <- st_read(fl[i])
kml2 <- as(kml, 'Spatial')
file_name_temp<-new_file_naes[i]
writeOGR(kml2,paste0(output,file_name_temp),driver = "ESRI Shapefile",layer = ".shp")
}}

