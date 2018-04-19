#' lineaus function
#'splits apart a semi-comma separated taxonomy list into a table of 
#' @param tax.vector a vector of semicolon separated taxonomy strings
#'
#' @return returns taxonomy as a data frame with k/p/o/c/f/g/s as separate columns.
#' @export
#'
#' @examples
linaeus<-function(tax.vector){
  list<- strsplit(tax.vector,split=";")
  matrix<-stringi::stri_list2matrix(list,byrow=T)
  colnames(matrix) <- c("kingdom","phylum","class","order","family","genus","species")
  table<-as.data.frame(matrix)
  return(table)
}