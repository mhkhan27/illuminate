#' Use to convert images (.jpeg,.jpg,.png) to pdfs
#' @param input Input folder from where picture will be converted to pdfs.
#' @param output Output folder in where converted pdfs will be saved.
#' @export

pic_to_pdf <- function(input, output){

#list all files in the input directory
fl = list.files(input, full.names = TRUE)
#list all file names in the input directory
file_names<-list.files(input, full.names = FALSE)
#remove the file extension from the file names
new_file_naes<- stringr::str_replace_all(file_names,c('.jpeg'="",'.jpg'="",'.png'=""))
#add the .pdf extension to the file names
new_file_naes<- paste0(new_file_naes,".pdf")


for ( i in  1: length(fl)){
  img = image_read(fl[i])
  file_name_temp<-new_file_naes[i]
  image_write(img, format="pdf", file.path(output, file_name_temp))

}}
