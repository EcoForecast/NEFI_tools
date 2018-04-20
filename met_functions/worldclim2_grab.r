#' Extract worldclim2 temperature, precipitation and uncertainties at 30s resolution for a given latitude/longtiude.
#' The precipitation uncertainty scales with elevation. If the user does not supply elevation then we default to 500m (mean in worldclim2 calibration data).
#' Would be nice to add a topography product to this when elevation is not supplied.
#' Only precipitation uncertainty scales with elevation.
#' Depends on output from JAGS models Colin fit. Not totally sure where to store these.
#' 
#'
#' @param latitude   vector of latitude in decimal degrees.
#' @param longitude  vector of longitude in decimal degrees.
#' @param elev       vector of elevations. If none supplied defaults to 500m.
#' @param n.sim      number of simulations to base uncertainty estimate on. Default to 1000.
#'
#' @return           returns a datafrme with mat, map, mat_sd and map_sd
#' @export
#'
#' @examples
#' points <- structure(c(-148.274862, -148.274862, -148.2747566, -148.2747566, 
#'                      -148.2746513, -148.2746513, -148.2744406, -148.2744406, -148.2740191, 
#'                      -148.2740191, 64.766184, 64.766184, 64.766184, 64.766184, 64.766184, 
#'                      64.766184, 64.766184, 64.766184, 64.766184, 64.766184), .Dim = c(10L, 
#'                                                                                      2L))
#' test.out <- worldclim2_grab(points[,2], points[,1])

worldclim2_grab <- function(latitude,longitude,elev = 500, n.sim = 1000){
  
  #make points an object
  points <- cbind(longitude, latitude)
 
  #load mean annual temperature and precipitation rasters from worldclim2
  prec <- raster::raster('/fs/data3/caverill/WorldClim2/wc2.0_bio_30s_12.tif')
  temp <- raster::raster('/fs/data3/caverill/WorldClim2/wc2.0_bio_30s_01.tif')
  
  #load runjags summaries of precipitation and temperature fitted vs. observed.
  prec.jags <- readRDS('/fs/data3/caverill/NEFI_microbial/worldclim2_uncertainty/precipitation_JAGS_model.rds')
  temp.jags <- readRDS('/fs/data3/caverill/NEFI_microbial/worldclim2_uncertainty/temperature_JAGS_model.rds')
  
  #extract worldclim2 predicted climate data.
  prec.obs <- raster::extract(prec, points)
  temp.obs <- raster::extract(temp, points)
  
  #temperature uncertainty workup. Draw from paramters n.sim times, calcualte predicted sd.
  temp.list <- list()
  for(i in 1:n.sim){
      intercept <- rnorm(1,temp.jags[1,4], temp.jags[1,5])
          slope <- rnorm(1,temp.jags[2,4], temp.jags[2,5])
             sd <- rnorm(1,temp.jags[3,4], temp.jags[3,5])
       pred.obs <- intercept + slope*temp.obs       
       pred.out <- rnorm(length(pred.obs),pred.obs,sd)
       temp.list[[i]] <- pred.out
  }
  temp.list <- do.call('cbind',temp.list)
    temp.sd <- apply(temp.list,1, sd, na.rm = TRUE)
    
  #precipitation uncertainty workup. Draw from parameters n.sim times, calcualte predicted sd.
    prec.list <- list()
    for(i in 1:n.sim){
          intercept <- rnorm(1,prec.jags[1,4], prec.jags[1,5])
              slope <- rnorm(1,prec.jags[2,4], prec.jags[2,5])
             sd.int <- rnorm(1,prec.jags[3,4], prec.jags[3,5])
      sd.elev.slope <- rnorm(1,prec.jags[4,4], prec.jags[4,5])
           pred.obs <- intercept + prec.obs*slope
            pred.sd <- sd.int + elev*sd.elev.slope
           pred.out <- rnorm(length(pred.obs),pred.obs,pred.sd)
     prec.list[[i]] <- pred.out
    }
    prec.list <- do.call('cbind',prec.list)
      prec.sd <- apply(prec.list, 1, sd, na.rm = T)
      
  #wrap up output and return.
  to_return <- data.frame(cbind(prec.obs,prec.sd,temp.obs,temp.sd))
  colnames(to_return) <- c('map','map_sd','mat','mat_sd')
  return(to_return)
  
} #end function.