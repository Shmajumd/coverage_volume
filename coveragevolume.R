#Function for doing lots of trig.  Volume of theoretical beam coverage with ground elevation taken into account.  
<<<<<<< HEAD
#This should show up on GitHub now
#Here is another change after that stupid error
=======
#Test commit
>>>>>>> 022555d8d180f0037996f43a6fe1ad9e0997cb92
coveragevolume <- function(){
  #get prerequisites
  #if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
  require(raster)
  require(geosphere)
  require (sp)
  require (maps)
  require(bit)
  require (bit64)
  require (rgdal)
  require (data.table)
  #Set radar location
  radar.lat <- 47.451973
  parameters.data.frame <- as.data.frame(x = radar.lat, xy = true)
  radar.lon <- -122.315776
  parameters.data.frame$radar.lon <- radar.lon
  radar.z <- 117       # m ASL
  parameters.data.frame$radar.z <- radar.z
  #Set radar specs
  radar.range <- 7000      # meters
  parameters.data.frame$radar.range <- radar.range
  radar.angle.top <- 11    # degrees
  radar.angle.top <- radar.angle.top * pi / 180
  parameters.data.frame$radar.angle.top <- radar.angle.top
  radar.angle.bottom <- 0  # degrees
  radar.angle.bottom <- radar.angle.bottom * pi / 180
  parameters.data.frame$radar.angle.bottom <- radar.angle.bottom
  #maybe make this more generic later, choose from list of radar locations, something like that.
  #Read in DEM data.  
  #get some rasters, dataframe them, give them column names, add distance to radar, remove rows outside radar range
  #bah, wrong files
    #n48w122.raster <- raster(file.path("~/Documents/SeaTac/Olympic Volume/SEA 13 ArcSecond DEM/USGS_NED_13_n48w122_ArcGrid/grdn48w122_13","w001001.adf"))
    #n48w122.data.frame <- as.data.frame(x = n48w122.raster, xy = TRUE)
    #colnames(n48w122.data.frame) <- c("lon", "lat", "elev")
    #n48w122.data.frame$disttoradar <- distGeo(p1 = cbind(n48w122.data.frame$lon,n48w122.data.frame$lat),p2 = c(radar.lon,radar.lat))
    #n48w122.data.frame <- subset(n48w122.data.frame, disttoradar < radar.range)
    
    #n48w124.raster <- raster(file.path("~/Documents/SeaTac/Olympic Volume/SEA 13 ArcSecond DEM/USGS_NED_13_n48w124_ArcGrid/grdn48w124_13","w001001.adf"))
    #n48w124.data.frame <- as.data.frame(x = n48w124.raster, xy = TRUE)
    #colnames(n48w124.data.frame) <- c("lon", "lat", "elev")
    #n48w124.data.frame$disttoradar <- distGeo(p1 = cbind(n48w124.data.frame$lon,n48w124.data.frame$lat),p2 = c(radar.lon,radar.lat))
    #n48w124.data.frame <- subset(n48w124.data.frame, disttoradar < radar.range)
    
    #n49w122.raster <- raster(file.path("~/Documents/SeaTac/Olympic Volume/SEA 13 ArcSecond DEM/USGS_NED_13_n49w122_ArcGrid/grdn49w122_13","w001001.adf"))
    #n49w122.data.frame <- as.data.frame(x = n49w122.raster, xy = TRUE)
    #colnames(n49w122.data.frame) <- c("lon", "lat", "elev")
    #n49w122.data.frame$disttoradar <- distGeo(p1 = cbind(n49w122.data.frame$lon,n49w122.data.frame$lat),p2 = c(radar.lon,radar.lat))
    #n49w122.data.frame <- subset(n49w122.data.frame, disttoradar < radar.range)
    
    #n49w123.raster <- raster(file.path("~/Documents/SeaTac/Olympic Volume/SEA 13 ArcSecond DEM/USGS_NED_13_n49w124_ArcGrid/grdn49w124_13","w001001.adf"))
    #n49w123.data.frame <- as.data.frame(x = n49w123.raster, xy = TRUE)
    #colnames(n49w123.data.frame) <- c("lon", "lat", "elev")
    #n49w123.data.frame$disttoradar <- distGeo(p1 = cbind(n49w123.data.frame$lon,n49w123.data.frame$lat),p2 = c(radar.lon,radar.lat))
    #n49w123.data.frame <- subset(n49w123.data.frame, disttoradar < radar.range)
    
    #n49w124.raster <- raster(file.path("~/Documents/SeaTac/Olympic Volume/SEA 13 ArcSecond DEM/USGS_NED_13_n49w124_ArcGrid/grdn49w124_13","w001001.adf"))
    #n49w124.data.frame <- as.data.frame(x = n49w124.raster, xy = TRUE)
    #colnames(n49w124.data.frame) <- c("lon", "lat", "elev")
    #n49w124.data.frame$disttoradar <- distGeo(p1 = cbind(n49w124.data.frame$lon,n49w124.data.frame$lat),p2 = c(radar.lon,radar.lat))
    #n49w124.data.frame <- subset(n49w124.data.frame, disttoradar < radar.range)
  
    n48w123.raster <- raster(file.path("~/Documents/SeaTac/Olympic Volume/Radar_Coverage/SEA 13 ArcSecond DEM/n48w123/grdn48w123_13","w001001.adf"))
    n48w123.data.frame <- as.data.frame(x = n48w123.raster, xy = TRUE)
    colnames(n48w123.data.frame) <- c("lon", "lat", "elev")
    n48w123.data.frame$disttoradar <- distGeo(p1 = cbind(n48w123.data.frame$lon,n48w123.data.frame$lat),p2 = c(radar.lon,radar.lat))
    n48w123.data.frame <- subset(n48w123.data.frame, disttoradar < radar.range)
    
  #combine data frames
    #DEM.data.frame <- rbind(n48w122.data.frame,n48w124.data.frame,n49w122.data.frame,n49w123.data.frame,n49w124.data.frame)
    DEM.data.frame <- n48w123.data.frame
    #Determine DEM resolution
    dem.res <- distGeo(p1 = c(DEM.data.frame$lon[1],DEM.data.frame$lat[1]),p2 = c(DEM.data.frame$lon[2],DEM.data.frame$lat[1]))
    
    #For every element in dem.data.filter, determine height of column, multiply by dem.res squared to get volume of column
    DEM.data.frame$radtop <- tan(radar.angle.top) * DEM.data.frame$disttoradar + radar.z
    DEM.data.frame$radbot <- tan(radar.angle.bottom) * DEM.data.frame$disttoradar + radar.z
    DEM.data.frame$height <- ifelse(DEM.data.frame$elev > DEM.data.frame$radbot, DEM.data.frame$radtop - DEM.data.frame$elev, DEM.data.frame$radtop - DEM.data.frame$radbot) # m
        #what if elev > radtop? 
    DEM.data.frame$height <- ifelse(DEM.data.frame$height < 0, 0, DEM.data.frame$height)
        #calculate volumes
    DEM.data.frame$vol <- DEM.data.frame$height * dem.res * dem.res # sq m
    #Sum element volumes in dem.data.filter?
    totalvol <- sum(DEM.data.frame$vol)
    parameters.data.frame$totalvol <- totalvol
    #coverage.vol <- sum(#dem.data.filter(vol))
    #Output interesting things
    #write.csv(coverage.vol, all parameters used)
    write.csv(DEM.data.frame, "output.csv", row.names=FALSE)
    write.csv(parameters.data.frame, "parameters.csv", row.names=FALSE)
    #volume of a particular area?  filter dem.data.filter again and repeat
    print("all done")
}
