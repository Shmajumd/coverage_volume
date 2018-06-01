#Function for doing lots of trig.  Volume of theoretical beam coverage with ground elevation taken into account.  

#Variable Info
  #radar.lat: latitude decimal degrees
  #radar.lon: longitude decimal degrees
  #radar.z: meters ASL
  #radar.range: meters from radar location to filter DEM by (radius). 
  #radar.angle.top: angle from radar center to top of beam
  #radar.angle.bottom: angle from radar center to bottom of beam
#DEM Info
  #https://viewer.nationalmap.gov/basic/

coveragevolume <- function(radar.lat = 47.451973, radar.lon = -122.315776, radar.z = 117,radar.range = 25, radar.angle.top = 11, radar.angle.bottom = 0){
  #Set working directory
    working.directory<-getwd()
  #Get Prerequisites
    if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
    if("bit64" %in% rownames(installed.packages()) == FALSE) {install.packages("bit64")}
    if("geosphere" %in% rownames(installed.packages()) == FALSE) {install.packages("geosphere")}
    if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")}
  #Required Packages
    require(data.table)
    require(bit64)
    require(geosphere)  
    require(raster)
  #Store radar location as a data.frame
    parameters.data.frame <- data.frame(radar.lat,radar.lon,radar.z)
    #maybe make this more generic later, choose from list of radar locations, something like that.
    
  #Read in DEM data.  
  #get some rasters, dataframe them, give them column names, add distance to radar, remove rows outside radar range
    n48w123.raster <- raster(file.path(working.directory,"SEA 13 ArcSecond DEM/n48w123/grdn48w123_13","w001001.adf"))
    n48w123.data.frame <- as.data.frame(x = n48w123.raster, xy = TRUE)
    colnames(n48w123.data.frame) <- c("lon", "lat", "elev")
    n48w123.data.frame$disttoradar <- distGeo(p1 = cbind(n48w123.data.frame$lon,n48w123.data.frame$lat),p2 = c(radar.lon,radar.lat))
    n48w123.data.frame <- subset(n48w123.data.frame, disttoradar <= radar.range)
    
  #Combine data.frames
    #DEM.data.frame <- rbind(n48w122.data.frame,n48w124.data.frame,n49w122.data.frame,n49w123.data.frame,n49w124.data.frame) ##If more than one is needed.
    DEM.data.frame <- n48w123.data.frame
  #Determine DEM resolution
    dem.lon.res <- distGeo(p1 = c(DEM.data.frame$lon[1],DEM.data.frame$lat[1]),p2 = c(DEM.data.frame$lon[2],DEM.data.frame$lat[1]))
    dem.lat.res <- distGeo(p1 = c(DEM.data.frame$lon[1],DEM.data.frame$lat[1]),p2 = c(DEM.data.frame$lon[1],DEM.data.frame$lat[2]))
    parameters.data.frame$latres <- dem.lat.res
    parameters.data.frame$lonres <- dem.lon.res
  #For every element in dem.data.filter, determine height of column, multiply by dem.lat.res and dem.lon.res
    DEM.data.frame$radtop <- tan(radar.angle.top * pi/180) * DEM.data.frame$disttoradar + radar.z
    DEM.data.frame$radbot <- tan(radar.angle.bottom * pi/180) * DEM.data.frame$disttoradar + radar.z
    DEM.data.frame$height <- ifelse(DEM.data.frame$elev > DEM.data.frame$radbot, DEM.data.frame$radtop - DEM.data.frame$elev, DEM.data.frame$radtop - DEM.data.frame$radbot) # m
  #what if elev > radtop? 
    DEM.data.frame$height <- ifelse(DEM.data.frame$height < 0, 0, DEM.data.frame$height)
  #calculate volumes
    DEM.data.frame$vol <- DEM.data.frame$height * dem.lat.res * dem.lon.res # sq m
  #Sum element volumes in dem.data.filter?
    totalvol <- sum(DEM.data.frame$vol)
    parameters.data.frame$totalvol <- totalvol
    #coverage.vol <- sum(#dem.data.filter(vol))
  #Output interesting things
    #write.csv(coverage.vol, all parameters used)
    currentDate <- Sys.Date()
    currentTime <-Sys.time()
    DEMfilename <- paste("DEMoutput_",currentDate,"_",currentTime,".csv",sep="")
    PARfilename <- paste("Parameters_",currentDate,"_",currentTime,".csv",sep="")
      write.csv(DEM.data.frame, file=DEMfilename, row.names=FALSE)
      write.csv(parameters.data.frame, PARfilename, row.names=FALSE)
    #volume of a particular area?  filter dem.data.filter again and repeat
    print("all done")
}
