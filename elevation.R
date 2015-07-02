###############################################################
# Strava elevation analysis
#
# 1. Load one or more gpx files of a workout
# 2. The file will be displayed on a map
# 3. For the coordinates in the gpx file, google maps elevation is queried
# 4. The plot and analysis will compare the elevation of the gpx records
#
#
###############################################################

source('global-options.R')

# Read and parse gpx files

library(XML)

## create a list of all files in the target folder
files <- paste("files/",list.files("files/.",pattern = "*.gpx"),sep='')

## prepare the data, so coordinates and elevation can be extracted
for(i in 1:length(files)){
  # Parse the GPX file
  pfile <- htmlTreeParse(files[i],error = function (...) {}, useInternalNodes = T)
  
  # Get all elevations, times and coordinates via the respective xpath
  elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
  
  #times <- xpathSApply(get(gpx.parsed[i]), path = "//trkpt/time", xmlValue)
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  
  # Extract latitude and longitude from the coordinates
  lon <- as.numeric(coords["lon",])
  lat <- as.numeric(coords["lat",])
  
  # Put everything in a dataframe and get rid of old variables
  geodf <- data.frame(lon = lon,lat = lat, ele = elevations)
  #geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times) # times works only for garmin
  rm(list=c("elevations", "lat", "lon", "coords")) # , "pfile""times", needs to be added if used
  
  # Calculate distance between trackpoints
  distance <- distCosine(geodf[,1:2],geodf[-1,1:2])
  total_distance <- cumsum(distance)
  geodf <- data.frame(cbind(geodf,distance,total_distance))
  
  # Calculate total elevation gain
  elevation.temp <- c(NA,head(geodf$ele,-1))
  elevation.diff <- geodf$ele - elevation.temp
  rm(elevation.temp)
  geodf <- data.frame(cbind(geodf,elevation.diff))
  geodf$id <- i
  colnames(geodf)[colnames(geodf)=='ele'] <- paste('ele.',i,sep="")
  assign(paste0("gpx.data.", i), geodf)
}

gpx <- ls(pattern='gpx.data.*')


## Calculate total elevation gain
#total_elevation <- sum(get(gpx[1])$elevation.diff[get(gpx[1])$elevation.diff>0],na.rm=T)

######################
# Create Map
######################

library(ggmap)

## choose the file for which one the map should be drawn
file_to_use <- get(gpx[1]) # choose which gpx track to show

## create the map with the gps track

par(mfrow=c(1,2)) # to add the later elevation profile
map <- get_map(location = c(lon = median(file_to_use$lon), lat = median(file_to_use$lat)),
               zoom = 11,maptype = c("terrain"))
p <- ggmap(map,extent = 'panel')
p <- p+ geom_point(aes(x = lon,y = lat),data = file_to_use,colour = "red",size = 1,pch = 20)
p

######################
# Google Maps Elevation (needs some work, as API has limits - split the data!)
######################

library(rgbif)
library(audio)

file_gmaps <- get(gpx[2])
file_gmaps <- file_gmaps[,1:2]

apikey <- getOption("g_elevation_api")
multi.fun <- function(x) {
  c(elevation(latitude=x$lat,longitude=x$lon,key = apikey),wait(1))
}

elevation <- multi.fun(file_gmaps)[3]
google <- cbind(file_gmaps,elevation)
google$ele.2 <- google$elevation
google$elevation <- NULL
colnames(google)[colnames(google)=='ele.2'] <- 'ele.3'
gpx.data.3 <- google

######################
# Plot elevation profiles
######################

library("ggplot2")
library("reshape")

## melt every data frame. this step is required, as the elevation columns have varying names
for(i in 1:length(gpx)){
  dat <- melt(get(gpx[i])[,c(3,5)],id.vars ='total_distance')
  assign(paste0("gpx.melt.", i), dat)
}

## melt the data
melts <- ls(pattern='gpx.melt.*')
melts <- do.call(rbind,mget(melts))

## Create a plot for all elevation profiles in the data frame
p <- ggplot(data=melts,aes(x=total_distance, y=value,group=variable,colour=variable))#, group=id,colour=id))
p <- p + geom_line(size=0.5)
p <- p + geom_path(alpha = 0.1)
p


