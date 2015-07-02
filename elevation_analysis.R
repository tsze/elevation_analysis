###############################################################
# Strava elevation analysis
#
# 1. Load a gpx file of a workout
# 2. The file will be displayed on a map
# 3. For the coordinates in the gpx file, google maps elevation is queried
# 4. The plot and analysis will compare the elevation of the gpx records
#
#
###############################################################

# Read and parse gpx files
library(XML)

file <- file.choose() # will prompt you for an input gpx file

# Parse the GPX file
pfile <- htmlTreeParse(file,error = function (...) {}, useInternalNodes = T)

# Get all elevations, times and coordinates via the respective xpath
elevation <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
#times <- xpathSApply(get(gpx.parsed[i]), path = "//trkpt/time", xmlValue) # disabled, as only work for garmin gpx
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)

# Extract latitude and longitude from the coordinates
lon <- as.numeric(coords["lon",])
lat <- as.numeric(coords["lat",])

# Put everything in a dataframe and get rid of old variables
geodf <- data.frame(lon = lon,lat = lat, elevation = elevation)
#geodf <- data.frame(lat = lats, lon = lons, elevation = elevations, time = times) # times works only for garmin
rm(list=c("elevation", "lat", "lon", "coords")) # , "pfile""times", needs to be added if used

# Calculate distance between trackpoints and total distance
distance <- distCosine(geodf[,1:2],geodf[-1,1:2])
total_distance <- cumsum(distance)
geodf <- data.frame(cbind(geodf,distance,total_distance))

# Calculate total elevation gain
elevation.temp <- c(NA,head(geodf$elevation,-1))
elevation.diff <- geodf$elevation - elevation.temp
rm(elevation.temp)
geodf <- data.frame(cbind(geodf,elevation.diff))

#geodf$id <- 'input'
#colnames(geodf)[colnames(geodf)=='elevation'] <- paste('elevation.',i,sep="")


######################
# Create Map
######################

library(ggmap)

## create the map with the gps track
map <- get_map(location = c(lon = median(geodf$lon), lat = median(geodf$lat)),
               zoom = 11,maptype = c("terrain"))
p <- ggmap(map,extent = 'panel')
p <- p+ geom_point(aes(x = lon,y = lat),data = geodf,colour = "red",size = 1,pch = 20)
plot(p)
dev.copy(png,"map.png",width=8,height=6,units="in",res=300)
dev.off()


######################
# Google Maps Elevation 
# note: this works only with a google elevation API subscription
# url: https://developers.google.com/maps/documentation/elevation/
######################

library(rgbif)
library(audio)

apikey <- getOption("g_elevation_api")
multi.fun <- function(x) {
  c(elevation(latitude=x$lat,longitude=x$lon,key = apikey),wait(1))
}

#elevation.gmaps <- data.frame(multi.fun(geodf[1:100,])[3])

elevation.gmaps <- read.csv('garmin_google_elevation.csv',header=TRUE) #if locally available (I don't have a Google API account, so used a workaround by splitting up the data)
#elevation.gmaps <- read.csv('strava_google_elevation.csv',header=TRUE) #if locally available (I don't have a Google API account, so used a workaround by splitting up the data)

colnames(elevation.gmaps)[colnames(elevation.gmaps)=='elevation'] <- 'elevation.gmaps'
geodf <- cbind(geodf,elevation.gmaps)

# Calculate total elevation gain for gmaps results
elevation.temp <- c(NA,head(geodf$elevation.gmaps,-1))
elevation.diff.gmaps <- geodf$elevation.gmaps - elevation.temp
rm(elevation.temp)
geodf <- data.frame(cbind(geodf,elevation.diff.gmaps))




######################
# Plot elevation profiles
######################

library("ggplot2")
library("reshape")

## melt data frame for ggplot2
dat <- melt(geodf[,c(3,5,7)],id.vars ='total_distance')

## Create a plot for all elevation profiles in the data frame
p <- ggplot(data=dat,aes(x=total_distance, y=value,group=variable,colour=variable))#, group=id,colour=id))
p <- p + geom_line(size=0.5)
p <- p + geom_path(alpha = 0.1)
plot(p)
dev.copy(png,"profile.png",width=8,height=6,units="in",res=300)
dev.off()

######################
# elevation analysis
######################

library(scales)

correlation <- cor(geodf$elevation,geodf$elevation.gmaps) #provides the correlation coefficient between measured and gmaps elevation
variance <- var(geodf$elevation,geodf$elevation.gmaps) #provides the correlation coefficient between measured and gmaps elevation

elevation.gain.gpx <- sum(ifelse(geodf$elevation.diff>0,geodf$elevation.diff,0),na.rm=T)
elevation.gain.gmaps <- sum(ifelse(geodf$elevation.diff.gmaps>0,geodf$elevation.diff.gmaps,0),na.rm=T)
print(paste('gpx overfitted by ',elevation.gain.gpx/elevation.gain.gmaps-1,sep=''))




