#' Use to convert KMLs to shapefiles
#' @param input Input folder from where KMLs will be converted to shapefile.
#' @param output Output folder in where converted KMLs will be saved.
#' @export

kml_to_shp <- function(input, output){

#list all files in the input directory
fl = list.files(input, full.names = TRUE, pattern = '.kml')
#list all files in the input directory, but only the file names
file_names<-list.files(input, full.names = FALSE, pattern = '.kml')
#remove the .kml extension from the file names
new_file_naes<- stringr::str_replace_all(file_names,c('.kml'=""))
#add the .shp extension to the file names
new_file_naes<- paste0(new_file_naes,".shp")



#loop through the list of files
for (i in  1: length(fl)){
#read the file
kml <- st_read(fl[i])
#convert the file to a spatial object
kml2 <- as(kml, 'Spatial')
#create a new file name
file_name_temp<-new_file_naes[i]
#write the file to a new location
writeOGR(kml2,paste0(output,file_name_temp),driver = "ESRI Shapefile",layer = ".shp")
}}



