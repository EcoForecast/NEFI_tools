#' Extract PRISM climate data for a particular site by year_month combination.
#' This function takes a data frame that contains columns of:
#' year_month in the form: "2014-01"
#' NEON site as "BART" or similar
#' latitude and longitude
#' depends on raster, and sp packages.
#'
#' @param data data frame with site, year_month, latitude and longitude columns.
#' @param prism.dir path to directory that contains prism files. Currently defaults to directory in Colin's data folder.
#'
#' @return returns of a data frame of 30-year mat and map normals as well as location-month specific tmean, tmin, tmax, and ppt.
#' @export
#'
#' @examples
prism_query <- function(data,prism.dir = '/fs/data3/caverill/PRISM/'){
  #grab longitude and latitude from data frame.
  points <- cbind(data$longitude, data$latitude)
  
  #get unique year-month-site combinations
  data$year_month_site <- paste(data$year_month,data$site, sep = '_')
  dates <- data[!duplicated(data$year_month_site),c('year_month_site','year_month','site','latitude','longitude')]
  dates$year  <- substr(dates$year_month,1,4)
  dates$month <- substr(dates$year_month,6,7)
  
  #extract 30-year products, create a data frame.
  map30 <- extract(raster(paste0(prism.dir,'PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil')),points)
  mat30 <- extract(raster(paste0(prism.dir,'PRISM_tmean_30yr_normal_800mM2_annual_bil/PRISM_tmean_30yr_normal_800mM2_annual_bil.bil')),points)
  
  to_return <- data.frame(data$year_month_site, map30, mat30)
  colnames(to_return)[1] <- 'year_month_site'
  
  #create output list for looping monthly data extraction.
  out.list <- list()
  
  #loop through each unique site by date combination.
  for(i in 1:nrow(dates)){
    points <- cbind(dates$longitude[i],dates$latitude[i])
    yr <- dates$year[i]
    mo <- dates$month[i]
    
    #extract for each variable of interest.
    tmean <- extract(raster(paste0(prism.dir,'PRISM_tmean_stable_4kmM2_',yr,mo,'_bil/PRISM_tmean_stable_4kmM2_',yr,mo,'_bil.bil')),points)
    tmin  <- extract(raster(paste0(prism.dir,'PRISM_tmin_stable_4kmM2_',yr,mo,'_bil/PRISM_tmin_stable_4kmM2_',yr,mo,'_bil.bil')),points)
    tmax  <- extract(raster(paste0(prism.dir,'PRISM_tmax_stable_4kmM2_',yr,mo,'_bil/PRISM_tmax_stable_4kmM2_',yr,mo,'_bil.bil')),points)
    ppt   <- extract(raster(paste0(prism.dir,'PRISM_ppt_stable_4kmM3_',yr,mo,'_bil/PRISM_ppt_stable_4kmM3_',yr,mo,'_bil.bil')),points)
    
    #group it up, put it in output list
    all <- c(tmean,tmin,tmax,ppt)
    out.list[[i]] <- all
  }
  
  #collapse output list
  out <- data.frame(do.call('rbind',out.list))
  colnames(out) <- c('tmean','tmin','tmax','ppt')
  out$year_month_site <- dates$year_month_site
  
  #merge 30 year and monthly data
  to_return <- merge(to_return,out, all.x = T)
  to_return <- to_return[,-1]
  
  #return a dataframe with the climate variables
  return(to_return)
}
