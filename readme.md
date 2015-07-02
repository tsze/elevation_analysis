* Elevation Analysis of GPS workout tracks (gpx)
** Intro
Using a Garmin device and Strava, I noticed there are sometimes huge differences in elevation gain. I wrote this script be able to compare recorded tracks (gpx files) with google maps elevation data, which is used as a reference for every gpx source file. 

** Analysis
The script prompts for a file, which you can choose locally and load into the R environment. This file needs to be in the gpx format with the ending '*.gpx'. 
The script then parsed the xml structured file for elevation data and coordinates. It will produce and export a map with the track as PNG file, as well as the elevation profile with the google maps elevation profile as a comparison.
In order to get the google elevation data, R is using the Google API. In order to run this script successfully, you need a Google API developer account, or transform the script to split the data into smaller pieces (500 per batch), which you can query for free.
Finally, the script will calculate total elevation gain for both, the GPS recorded data as well as the Google maps elevation data. 