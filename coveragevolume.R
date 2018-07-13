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

coveragevolume <- function(radar.lat = 47.451973, radar.lon = -122.315776, radar.z = 117, radar.range = 5000, radar.angle.top = 11, radar.angle.bottom = -11){
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
    require(MASS)
    require(parallel)
  #Store radar location as a data.frame
    parameters.data.frame <- data.frame(radar.lat,radar.lon,radar.z, radar.range, radar.angle.bottom, radar.angle.top)
    #maybe make this more generic later, choose from list of radar locations, something like that.
    
  #Read in DEM data.  
  #get some rasters, dataframe them, give them column names, add distance to radar
    n48w123.raster <- raster(file.path(working.directory,"SEA 13 ArcSecond DEM/n48w123/grdn48w123_13","w001001.adf"))
    n48w123.data.frame <- as.data.frame(x = n48w123.raster, xy = TRUE)
    colnames(n48w123.data.frame) <- c("lon", "lat", "elev")
    n48w123.data.frame$disttoradar <- distGeo(p1 = cbind(n48w123.data.frame$lon,n48w123.data.frame$lat),p2 = c(radar.lon,radar.lat))
    #n48w123.data.frame <- subset(n48w123.data.frame, disttoradar <= radar.range)
    
  #Combine data.frames
    #DEM.data.frame <- rbind(n48w122.data.frame,n48w124.data.frame,n49w122.data.frame,n49w123.data.frame,n49w124.data.frame) ##If more than one is needed.
    DEM.data.frame <- n48w123.data.frame
  #Find info about DEM
    lat <- unique(DEM.data.frame$lat)
    lon <- unique(DEM.data.frame$lon)
    n <- length(lat)
    o <- length(lon)  
    min.lat <-min(lat)
    min.lon <- max(lon)
    min.2.lat <- -sort(-lat,partial=n-1)[n-1]
    min.2.lon <- sort(lon,partial=n-1)[n-1]
  #Determine DEM resolution
    dem.lon.res <- distGeo(p1 = c(min.lon, min.lat),p2 = c(min.2.lon,min.lat))
    dem.lat.res <- distGeo(p1 = c(min.lon, min.lat),p2 = c(min.lon,min.2.lat))
    parameters.data.frame$latres <- dem.lat.res
    parameters.data.frame$lonres <- dem.lon.res
  #Remove data outside of radar range  
    DEM.data.frame <- subset(DEM.data.frame, disttoradar <= radar.range)
  
  #Load radar data and calculate track densities
    setwd(file.path(working.directory, "Radar_Data"))
    filelist<-list.files(pattern="\\.csv$")
    filelist<-sample(x = filelist,size = 5,replace = F) ###For testing, only use a smallish sample of the data. Comment out when doing a full run.
    numCores<- detectCores()-1  
    datalist<-mclapply(filelist,fread,mc.cores = numCores) #faster if loading multiple CSV files
    #datalist<-lapply(filelist,fread)
    data<-rbindlist(datalist)
    
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
