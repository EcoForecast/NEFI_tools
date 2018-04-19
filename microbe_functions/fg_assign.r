#' fg_assign.r assigns functional groups to a taxonomy table based on the FUNGuild database.
#' This function was built by Colin Averill, however the FUNGuild database was developed by Nguyen et al.
#' PLEASE CITE: Nguyen NH, Song Z, Bates ST, Branco S, Tedersoo L, Menke J, Schilling JS, Kennedy PG. 2016. FUNGuild: An open annotation tool for parsing fungal community datasets by ecological guild. Fungal Ecology, 20: 241-248. doi:10.1016/j.funeco.2015.06.006
#' depends on R packages rvest, jsonlite.
#'
#' @param tax_table a data frame with kingdom/phylum/class/order/family/genus/species as separate columns with those names in lower case.
#' @param url path to FUNGuild database. defaults to http://www.stbates.org/funguild_db.php. 
#'
#' @return a taxonomy table with FUNGuild assignments appended.
#' @export
#'
#' @examples
#' generate some artificial data to assign guild to.
#' tax_table <- structure(list(kingdom = "Fungi", phylum = "Ascomycota", class = "Pezizomycetes", order = "Pezizales", family = "Tuberaceae", genus = "Tuber", species = "Tuber melosporum"), 
#'                            .Names = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), 
#'                           row.names = 4L, class = "data.frame")
#' test <- fg_assign(tax_table)
#' 
fg_assign <- function(tax_table, url = "http://www.stbates.org/funguild_db.php"){
  #check if dependencies are installed. If not, stop. 
  if (!require('rvest'   ,character.only = TRUE)){
    stop("please install the rvest package.")
  }
  if (!require('jsonlite',character.only = TRUE)){
    stop("please install the jsonlite package.")
  }
  
  #check that the input is formatted right. If not, stop, throw an error.
  if (!is.data.frame(tax_table)){
    stop('Your taxonomy table needs to be a data.frame. Try again.')
  }
  
  #download FUNGuild database, convert it to something R interpretable.
  fg <- url %>% 
    xml2::read_html() %>%
    rvest::html_text() 
  fg <- jsonlite::fromJSON(gsub("funguild_db", "", fg))
  
  #There are 9 unique levels of taxonomic resolution actually in FUNGuild (though 24 potential levels)
  #0-keyword, 3-Phylum, 7-Order, 9-Family, 13-genus, 20-Species, 21-Subspecies, 24-Form
  #This function requires data on k/c/p/o/f/g/s, so only deals with levels 3,7,9,13,20
  #What follows is a series of if statements to assign function.
  #start with highest level of taxonomy and go down.
  #This is written with for loops. Could be faster with lapply.
  
  #add columns to tax table for fg output.
  out <- data.frame(matrix(,nrow=nrow(tax_table),ncol = 7))
  colnames(out) <- colnames(fg)[4:10]
  tax_table <- cbind(tax_table,out)
  
  #phylum level match.
  for(i in 1:nrow(tax_table)){
    if(tax_table$phylum[i] %in% fg$taxon){
      tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table$phylum[i],fg$taxon),4:10]
    }
  }
  #class level match.
  for(i in 1:nrow(tax_table)){
    if(tax_table$class[i] %in% fg$taxon){
      tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table$class[i],fg$taxon),4:10]
    }
  }
  #order level match.
  for(i in 1:nrow(tax_table)){
    if(tax_table$order[i] %in% fg$taxon){
      tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table$order[i],fg$taxon),4:10]
    }
  }
  #family level match.
  for(i in 1:nrow(tax_table)){
    if(tax_table$family[i] %in% fg$taxon){
      tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table$family[i],fg$taxon),4:10]
    }
  }
  #genus level match.
  for(i in 1:nrow(tax_table)){
    if(tax_table$genus[i] %in% fg$taxon){
      tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table$genus[i],fg$taxon),4:10]
    }
  }
  #species level match.
  for(i in 1:nrow(tax_table)){
    if(tax_table$species[i] %in% fg$taxon){
      tax_table[i,(ncol(tax_table) - 6):ncol(tax_table)] <- fg[match(tax_table$species[i],fg$taxon),4:10]
    }
  }
  
  #report and return output.
  cat(sum(!is.na(tax_table$guild))/(nrow(tax_table))*100,'% of taxa assigned a functional guild.', sep = '')
  return(tax_table)
}
