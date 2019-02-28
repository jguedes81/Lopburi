# CODE IN SUPPORT OF D'ALPOIM GUEDES, HANSON ET AL. 2019. ARCHAEOLOGICAL AND ANTHROPOLOGICAL SCIENCES 
#https://doi.org/10.1007/s12520-019-00794-8

#load packages for spatial processing
library(raster)
library(maps)
library(mapdata)
library(FedData)

FedData::pkg_test("sf")
FedData::pkg_test("rgdal")
FedData::pkg_test("ncdf4")
FedData::pkg_test("raster")
FedData::pkg_test("geomapdata")
FedData::pkg_test("maptools")

# Plotting packages
FedData::pkg_test("RColorBrewer")
FedData::pkg_test("plotly")
FedData::pkg_test("htmlwidgets")
require(stats)
require(ggplot2)
library(gdata)
library(rgdal)
library(xlsx)

par(family="mono")

#Define the study region
ASIA_poly <- 
  extent(60,160,0,40) %>% ## Use a bounding box you would like for your project.
  FedData::polygon_from_extent("+proj=longlat +ellps=GRS80")

#Download data from Natural Earth (www.naturalearthdata.com)and point the code to the directories you have created to house this data

#country data
countries <- readOGR("/DATA/NATURAL_EARTH/ne_10m_admin_0_countries_lakes/","ne_10m_admin_0_countries_lakes")
countries <- as(countries,"SpatialLines")
countries <- raster::crop(countries,ASIA_poly)
provinces <- raster::crop(readOGR("/DATA/NATURAL_EARTH/ne_10m_admin_1_states_provinces_lakes/","ne_10m_admin_1_states_provinces_lakes"),ASIA_poly)
provinces <- provinces[provinces$admin=="Thailand",]
provinces.lines <- raster::crop(readOGR("/DATA/NATURAL_EARTH/ne_10m_admin_1_states_provinces_lines/","ne_10m_admin_1_states_provinces_lines"),ASIA_poly)
provinces.lines <- provinces.lines[provinces.lines$adm0_name=="Thailand",]
rivers <- raster::crop(readOGR("/DATA/NATURAL_EARTH/ne_10m_rivers_lake_centerlines/","ne_10m_rivers_lake_centerlines"),ASIA_poly)
cities <- raster::crop(readOGR("/DATA/NATURAL_EARTH/ne_10m_populated_places/","ne_10m_populated_places"),ASIA_poly)
cities <- cities[cities$NAME %in% c("Bangkok"),]

#DEM Data
#Download the ETOPO1 raster from https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/grid_registered/georeferenced_tiff/
#extract the zip file and place in your directory

ETOPO1.rast <- raster("/DATA/ETOPO1/ETOPO1_Bed_g_geotiff.tif")
ETOPO1.rast <- raster::crop(ETOPO1.rast,ASIA_poly, snap="out")
projection(ETOPO1.rast) <- projection(ASIA_poly)

#thresholding out areas below 0 masl and above 9000 masl to reduce file size
ASIA <- (ETOPO1.rast>0 & ETOPO1.rast<=9000)
ASIA <- calc(ASIA,function(x){x[x==0] <- NA; return(x)})
ETOPO1.rast <- raster::mask(ETOPO1.rast,ASIA)


# Calculate a hillshade of the raster, for plotting
slope <- terrain(ETOPO1.rast, opt='slope')
aspect <- terrain(ETOPO1.rast, opt='aspect')
ETOPO1.hill <- hillShade(slope, aspect, 40, 230)

#Creating elevation classes for DEM

ASIA1 <- (ETOPO1.rast>0 & ETOPO1.rast<=100)
ASIA1 <- calc(ASIA1,function(x){x[x==0] <- NA; return(x)})
ASIA2 <- (ETOPO1.rast>100 & ETOPO1.rast<=250)
ASIA2 <- calc(ASIA2,function(x){x[x==0] <- NA; return(x)})
ASIA3 <- (ETOPO1.rast>250 & ETOPO1.rast<=500)
ASIA3 <- calc(ASIA3,function(x){x[x==0] <- NA; return(x)})
ASIA4 <- (ETOPO1.rast>500 & ETOPO1.rast<=1000)
ASIA4 <- calc(ASIA4,function(x){x[x==0] <- NA; return(x)})
ASIA5 <- (ETOPO1.rast>1000 & ETOPO1.rast<=9000)
ASIA5 <- calc(ASIA5,function(x){x[x==0] <- NA; return(x)})

#linking site data. Read in your site data  

sites <- read.xlsx(".../Central_Thai_sites.xlsx", stringsAsFactors=F, sheetName = "Sheet1")

points(sites$Long, sites$Lat,pch=19, col="black",cex=1)


#PLOTTING MASTER MAP

par(family="mono")
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ASIA_poly),xmax(ASIA_poly)), ylim=c(ymin(ASIA_poly),ymax(ASIA_poly)), xaxs="i", yaxs="i", axes=FALSE, main='')

plot(ETOPO1.hill, maxpixels=ncell(ETOPO1.hill), col=grey(60:100/100), useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)

plot(countries, add=T, lwd=1, col="black")
plot(rivers, add=T, lwd=0.2, col="blue")
plot(provinces.lines, add=T,lwd=0.5, lty=2, col="black")
plot(cities, add=T, pch=21)

#PLOTTING ELEVATION RASTER

par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ASIA_poly),xmax(ASIA_poly)), ylim=c(ymin(ASIA_poly),ymax(ASIA_poly)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(ETOPO1.hill, maxpixels=ncell(ETOPO1.hill), col=grey(60:100/100), useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)

plot(ASIA1, maxpixels=ncell(ASIA1), col="#2ca25f80", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(ASIA2, maxpixels=ncell(ASIA2), col="#238443", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(ASIA3, maxpixels=ncell(ASIA3), col="#41ab5d", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(ASIA4, maxpixels=ncell(ASIA4), col="#78c679", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(ASIA5, maxpixels=ncell(ASIA5), col="#addd8e", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)

plot(countries, add=T, lwd=1, col="black")
plot(rivers, add=T, lwd=0.5, col="blue")
plot(provinces.lines, add=T,lwd=0.5, lty=2, col="black")
plot(cities, add=T, pch=21)
points(sites$Long, sites$Lat,pch=19, col="black",cex=1)


legend("bottomleft", pch = c(15, 15, 15, 15,15), 
       col = c("#2ca25f80", "#238443", "#41ab5d", "#78c679", "#addd8e"), 
       legend = c("0-100", "100-250", "250-500", "500-1000", "1000-9000"))



#BIOCLIM DATA TO CREATE SPATIAL MAP OF ANNUAL PRECIPITATION (FIGURE 4A)

#download data from www.worldclim.org and point to this directory
bio<-getData('worldclim',download = TRUE, res=2.5,path='/Worldclim',var='bio')

bioclim_names<-c("Annual Mean Temp", "Mean Diurnal Range", "Isothermality", "Temp Seasonality", "Max Temp Warmest Month", "Min Temp Coldest Month", "Temp Annual Range", "Mean Temp Wettest Quarter","Mean Temp Driest Quarter", "Mean Temp Warmest Quarter", "Mean Temp Coldest Quarter", "Annual Precip", "Precip Wettest Month", "Precip Driest Month", "Precip Seasonality", "Precip Wettest Quarter", "Precip Driest Quarter", "Precip Warmest Quarter", "Precip Coldest Quarter")

#view all the names of the bio variables
names(bio)<-bioclim_names

#plotting all the different rasters
plot(bio)

plot(bio[[1]], main=names(bio[[1]]))

#cropping to a given extent

ASIA_poly <- 
  extent(97.5,105.5,13,16.5)

envcrop <- crop(bio,ASIA_poly)

#dividing the temperature by 10

ten_div<-c(1,2,5,6,7,8,9,10,11)  #the layers we want to divide by ten

for (layer in ten_div){
  
  envcrop[[layer]]<-envcrop[[layer]]/10
}

#creating breaks for precipitation
PRECIP1 <- (envcrop[[12]]>0 & envcrop[[12]]<=1000)
PRECIP1 <- calc(PRECIP1,function(x){x[x==0] <- NA; return(x)})
PRECIP2 <- (envcrop[[12]]>1000 & envcrop[[12]]<=1100)
PRECIP2 <- calc(PRECIP2,function(x){x[x==0] <- NA; return(x)})
PRECIP3 <- (envcrop[[12]]>1100 & envcrop[[12]]<=1200)
PRECIP3 <- calc(PRECIP3,function(x){x[x==0] <- NA; return(x)})
PRECIP4 <- (envcrop[[12]]>1200 & envcrop[[12]]<=1300)
PRECIP4 <- calc(PRECIP4,function(x){x[x==0] <- NA; return(x)})
PRECIP5 <- (envcrop[[12]]>1300 & envcrop[[12]]<=1400)
PRECIP5 <- calc(PRECIP5,function(x){x[x==0] <- NA; return(x)})
PRECIP6 <- (envcrop[[12]]>1400 & envcrop[[12]]<=1500)
PRECIP6 <- calc(PRECIP6,function(x){x[x==0] <- NA; return(x)})
PRECIP7 <- (envcrop[[12]]>1500 & envcrop[[12]]<=1600)
PRECIP7 <- calc(PRECIP7,function(x){x[x==0] <- NA; return(x)})
PRECIP8 <- (envcrop[[12]]>1600 & envcrop[[12]]<=9000)
PRECIP8 <- calc(PRECIP8,function(x){x[x==0] <- NA; return(x)})

terrain.colors(8, alpha=1)

#creating breaks for temperature
TEMP1 <- (envcrop[[1]]>-20 & envcrop[[1]]<=-10)
TEMP1 <- calc(TEMP1,function(x){x[x==0] <- NA; return(x)})
TEMP2 <- (envcrop[[1]]>-10 & envcrop[[1]]<=-5)
TEMP2 <- calc(TEMP2,function(x){x[x==0] <- NA; return(x)})
TEMP3 <- (envcrop[[1]]>-5 & envcrop[[1]]<=0)
TEMP3 <- calc(TEMP3,function(x){x[x==0] <- NA; return(x)})
TEMP4 <- (envcrop[[1]]>0 & envcrop[[1]]<=5)
TEMP4 <- calc(TEMP4,function(x){x[x==0] <- NA; return(x)})
TEMP5 <- (envcrop[[1]]>5 & envcrop[[1]]<=10)
TEMP5 <- calc(TEMP5,function(x){x[x==0] <- NA; return(x)})
TEMP6 <- (envcrop[[1]]>10 & envcrop[[1]]<=20)
TEMP6 <- calc(TEMP6,function(x){x[x==0] <- NA; return(x)})
TEMP7 <- (envcrop[[1]]>20 & envcrop[[1]]<=30)
TEMP7 <- calc(TEMP7,function(x){x[x==0] <- NA; return(x)})
TEMP8 <- (envcrop[[1]]>30 & envcrop[[1]]<=40)
TEMP8 <- calc(TEMP8,function(x){x[x==0] <- NA; return(x)})


#plotting precipitation

dev.off()
par(family="mono")
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ASIA_poly),xmax(ASIA_poly)), ylim=c(ymin(ASIA_poly),ymax(ASIA_poly)), xaxs="i", yaxs="i", axes=FALSE, main='')


plot(PRECIP1, maxpixels=ncell(PRECIP1), col="#deebf7", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(PRECIP2, maxpixels=ncell(PRECIP2), col="#9ecae1", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(PRECIP3, maxpixels=ncell(PRECIP3), col="#FFFF40FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(PRECIP4, maxpixels=ncell(PRECIP4), col="#FFCC00FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(PRECIP5, maxpixels=ncell(PRECIP5), col="#FF9900FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(PRECIP6, maxpixels=ncell(PRECIP6), col="#FF6600FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(PRECIP7, maxpixels=ncell(PRECIP7), col="#FF3300FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(PRECIP8, maxpixels=ncell(PRECIP8), col="#FF0000FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)

plot(countries, add=T, lwd=1)
plot(provinces.lines, add=T,lwd=0.5)

legend("bottomleft", pch = c(15, 15, 15, 15,15, 15, 15, 15,15), 
       col = c("lightskyblue", "lightskyblue", "#FFFF40FF", "#FFCC00FF", "#FF9900FF", "#FF6600FF", "#FF3300FF", "#FF0000FF"), 
       legend = c("0-1000", "1000-1100", "1100-1200", "1200-1300", "1300-1400", "1400-1500", "1500-1600", "1600-9000"))

sites <- read.xlsx("/Users/jadedalpoimguedes/Desktop/TAP\ /Central_Thai_sites.xlsx", stringsAsFactors=F, sheetName = "Sheet1")

points(sites$Long, sites$Lat,pch=19, col="black",cex=1)

#plotting
dev.off()
par(family="mono")
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ASIA_poly),xmax(ASIA_poly)), ylim=c(ymin(ASIA_poly),ymax(ASIA_poly)), xaxs="i", yaxs="i", axes=FALSE, main='')

#plotting annual mean temperature 
plot(envcrop[[1]])

par(family="mono")
par(mai=c(0,0,0,0))
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(ASIA_poly),xmax(ASIA_poly)), ylim=c(ymin(ASIA_poly),ymax(ASIA_poly)), xaxs="i", yaxs="i", axes=FALSE, main='')


plot(TEMP1, maxpixels=ncell(TEMP1), col="#F2F2F2FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(TEMP2, maxpixels=ncell(TEMP2), col="#EFC2B3FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(TEMP3, maxpixels=ncell(TEMP3), col="#ECB176FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(TEMP4, maxpixels=ncell(TEMP4), col="#E9BD3AFF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(TEMP5, maxpixels=ncell(TEMP5), col="#E6E600FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(TEMP6, maxpixels=ncell(TEMP6), col="#8BD000FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(TEMP7, maxpixels=ncell(TEMP7), col="#3EBB00FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)
plot(TEMP8, maxpixels=ncell(TEMP8), col="#00A600FF", useRaster=T, legend=FALSE,  xlab="", ylab="", axes=FALSE, main='', add=T)

plot(countries, add=T, lwd=1)
plot(provinces.lines, add=T,lwd=0.5)

legend("bottomleft", pch = c(15, 15, 15, 15,15, 15, 15, 15,15), 
       col = c("#F2F2F2FF", "#EFC2B3FF", "#ECB176FF", "#E9BD3AFF", "#E6E600FF", "#8BD000FF", "#3EBB00FF", "#00A600FF"), 
       legend = c("-20/-10", "-10/-5", "-5/0", "0-5", "5-10", "10-15", "15-20", "20-40"))

sites <- read.xlsx("/Users/jadedalpoimguedes/Desktop/Central_Thai_sites.xlsx", stringsAsFactors=F, sheetName = "Sheet1")

plot(sites, add=T)


##SOILS MAP FOR AREA SURROUNDING THE SITE. FIGURE 1B. 


install.packages(c("RCurl", "XML", "rgdal", "raster", "sp", "aqp", "mda", "gstat", "plotKML", "dismo", "rJava"))
install.packages("GSIF", repos=c("http://R-Forge.R-project.org"), type = "source")

#zoom in around site
ASIA_poly <- 
  extent(100.5,100.75,14.85,15.15)

#read in soils data
#download raw soils data from www.soilsgrid.com and point to .tiff file 
soils<-raster("/TAXNWRB_250m.tiff") 

soils <-crop(soils, ASIA_poly)

#looking at the range of values in a raster
cellStats(soils, range)

#creating a histogram to look at the values of soil types
par(family="mono")
par(mai=c(1,1,1,1))


hist(soils, main="Distribution of soils values", 
     col= "blue", breaks=20, xlim=c(0,20),
     maxpixels=22000000)

data(soil.legends)

col<-list(soil.legends$TAXNWRB$COLOR)

number<-list(soil.legends$TAXNWRB)

#LIST OF SOIL TYPES IN AREA AND CORRESPONDING COLOR
#1 Haplic.Acrisols #FE813E
#11 Haplic.Alisols #F5EBCC
#25 Endogleyic.Cambisols #FDCE70
#27 Haplic.Cambisols #FDE260
#48 Haplic.Fluvisols #10A9E9
#52 Haplic.Fluvisols..Eutric. #0F8EE9
#54 Haplic Gleysols #A087BF
#56 Haplic Gleysols (Eutric) #9E83BF
#79 Haplic Luvisols   #F99491
#115 Calcic Vertisols  #A87188
#116 Haplic Vertisols #A88E99
#117 Haplic Vertisols (Eutric)#A851A2

# Read in sites
sites <- read.xlsx("../Central_Thai_sites.xlsx", stringsAsFactors=F, sheetName = "Sheet1")

#Plotting

par(mai=c(0,0,0,0))

plot(1, type='n', xlab="", ylab="", xlim=c(xmin(soils),xmax(soils)), ylim=c(ymin(soils),ymax(soils)), xaxs="i", yaxs="i", axes=FALSE, main='')
plot(soils, col="#FE813E", zlim=c(1,1), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#F5EBCC", zlim=c(11,11), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#FDCE70", zlim=c(25,25), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#FDE260", zlim=c(27,27), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#10A9E9", zlim=c(48,48), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#0F8EE9", zlim=c(52,52), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#9E83BF", zlim=c(56,56), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#F99491", zlim=c(79,79), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#A87188", zlim=c(115,115), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#A88E99", zlim=c(116,116), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)
plot(soils, col="#A851A2", zlim=c(117,117), maxpixels=ncell(soils), useRaster=T,legend=FALSE, xlab="", ylab="", axes=FALSE, main='', add=T)

points(sites$Long, sites$Lat,pch=19, col="black",cex=1)

legend("bottomleft", pch = c(15, 15, 15, 15,15, 15, 15, 15,15, 15, 15), 
       col = c("#FE813E","#F5EBCC", "#FDCE70","#FDE260","#10A9E9","#0F8EE9","#9E83BF", "#F99491", "#A87188", "#A88E99", "#A851A2"), 
       legend = c("Haplic.Acrisols", "Haplic.Alisols", "Endogleyic.Cambisols", "Haplic.Cambisols", "Haplic.Fluvisols", "Haplic Fluvisols Eutric", "Haplic Gleysols", "Haplic Gleysols (Eutric)", "Haplic Luvisols", "Calcic Vertisols", "Haplic Vertisols", "Haplic Vertisols (Eutric)"))


#CODE TO PREPARE PRECIPITATION PLOTS (FIGURE 4B)
## This code is similar to the code employed in d'Alpoim Guedes, Manning, and Bocinsky 2016 "PREPARE_GHCN.R"
## This script downloads and cleans daily climate records from the Global Historical Climate Database.
# All functions associated with this code should be loaded from d'Alpoim Guedes, Manning, and Bocinsky 2016 "GUEDES_ET_AL_2015_SOURCE"

require(FedData)
require(spatstat)
require(zoo)
require(raster)
require(rasterVis)
require(Hmisc)
require(abind)
require(mgcv)
require(plotrix)

#read in all functions

all.functions <- lapply(list.files(".../asian_niche-0.9.0/src",full.names=T),source)

# Set the calibration period
calibration.years <- 1961:1990

# Create a polygon for GHCN extraction
GHCN.poly <- polygon_from_extent(extent(97.5,105.5,13,16.5), proj4string="+proj=longlat")

## GHCN DATA ##
# These are the GHCN stations that will be used to calibrate the interpolation model.
# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
GHCN.data <- get_ghcn_daily(template=GHCN.poly, label="Asia_GHCN", elements=c("PRCP"), raw.dir="/Volumes/DATA/GHCN", extraction.dir = "/Users/jadedalpoimguedes/Desktop/EXTRACTIONS/GHCN/", standardize=T)
saveRDS(GHCN.data,file='/OUTPUT/ghcn_data_all.Rds')
GHCN.data <- readRDS('/OUTPUT/ghcn_data_all.Rds')

GHCN.stations <- GHCN.data[[1]] # The spatial station data
GHCN.data <- GHCN.data[[2]] # The actual temperature data
GHCN.stations <- GHCN.stations[!duplicated(GHCN.stations$ID),] # Remove duplicated station location data
GHCN.data <- GHCN.data[as.character(GHCN.stations$ID)] # Ensure that the order of the two datasets are the same

# Some stations have the same "location", but not the same data. Remove these points.
nonduplicates <- !duplicated(coordinates(GHCN.stations)) * !duplicated(coordinates(GHCN.stations),fromLast=T)
GHCN.stations <- GHCN.stations[nonduplicates,]
GHCN.data <- GHCN.data[nonduplicates]

# Get run length encoding of missing data
all.rles <- do.call(c,lapply(GHCN.data,function(test){
  tryCatch(getMissingRLE(test),error=function(e) NULL)
}))

# Calculate the cutoff length of data gaps.
# Years with gaps longer than "cutoff" will be dropped
cutoff <- calcGapCutoff(rleVector=all.rles, pLevel=0.95)

## Clean the GHCN data
# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
GHCN.data.clean <- lapply(GHCN.data,function(station.data){ghcnCleaner(data.list=station.data, min.years=10, year.range=calibration.years, na.cutoff=0)})
names(GHCN.data.clean) <- names(GHCN.data)
GHCN.data.clean <- GHCN.data.clean[!sapply(GHCN.data.clean, is.null)]
saveRDS(GHCN.data.clean,"../OUTPUT/GHCN.data.clean.Rds")
GHCN.data.clean <- readRDS("../OUTPUT/GHCN.data.clean.Rds")

# THIS SECTION WAS RUN, CACHED, THEN COMMENTED OUT FOR SPEED.
# UNCOMMENT TO RUN AGAIN.
# Keep only the clean stations
GHCN.stations <- GHCN.stations[GHCN.stations$ID %in% names(GHCN.data.clean),]
# 
# # Get the station elevations
# ## YOU MUST POINT THIS AT A DIRECTORY WITH SRTM TILES
GHCN.stations <- srtmExtractor(locations=GHCN.stations, SRTM.dir="/Volumes/DATA/SRTM/")
# 
# # Get all stations averages over the calibration period
GHCN.data.averages <- lapply(GHCN.data.clean, function(station){
  return(lapply(station,calcDailyMeanSD))
})
# 
# # Create a final dataset for use anywhere in Asia!
GHCN.data.final <- list(GHCN.stations,GHCN.data.averages)
saveRDS(GHCN.data.final,file="/Users/jadedalpoimguedes/Desktop/OUTPUT/ghcn_data_final.Rds")
#GHCN.data.final <- readRDS('../OUTPUT/ghcn_data_final.Rds')



# # Weather data from Lop Buri, Thailand:TH000048425
lopburi.elev <- GHCN.stations[GHCN.stations$ID=="TH000048426","elevation"]
lopburi.PRCP.raw <- data.frame(DOY=as.numeric(strftime(as.POSIXlt(paste(rep(GHCN.data.clean[['TH000048426']][['PRCP']]$YEAR,each=31),rep(GHCN.data.clean[['TH000048426']][['PRCP']]$MONTH,each=31),rep(1:31,times=nrow(GHCN.data.clean[['TH000048426']][['PRCP']])), sep='.'), format="%Y.%m.%d"), format = "%j")), DATA=as.numeric(t(GHCN.data.clean[['TH000048426']][['PRCP']][,-1:-2])) )
lopburi.PRCP.raw <- lopburi.PRCP.raw[!is.na(lopburi.PRCP.raw$DATA) & !is.na(lopburi.PRCP.raw$DOY),]
lopburi.PRCP.smooth <- calcDailyMeanSD(GHCN.data.clean[['TH000048426']][['PRCP']])

lopburi.PRCP.raw[,2] <- lopburi.PRCP.raw[,2]/10
lopburi.PRCP.smooth[,2:3] <- lopburi.PRCP.smooth[,2:3]/10



#  Setting plotting parameters
fig.width <- 7.25
between <- 0.25
nrow <- 3
ncol <- 2
year.height <- 0.3
plot.width <- (fig.width - ((ncol-1)*between) - (year.height*2))/ncol
plot.height <- plot.width
fig.height <- (plot.height * nrow) + year.height*2

#Setting plotting parameters
par(family="mono")
par(mai=c(0.5,0.5,0.5,0.5), lend='round', ljoin='round', xpd=T)
plot(1, type='n', xlab="", ylab="",xaxs='i',yaxs='i', xlim=c(0,1), ylim=c(0,1), axes=FALSE, main='')

#Plotting Precipitation
polar.plot(lengths=lopburi.PRCP.raw$DATA, polar.pos=(lopburi.PRCP.raw$DOY)*360/365, label.prop=1.2, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"), label.pos=c(0,32,60,92,122,153,184,214,245,275,306,336)*360/365, rp.type='s', clockwise=TRUE, start=1, radial.lim=c(0,20), cex=0.1, point.symbols=19, mar=par('mar'))
polar.plot(lengths=as.numeric(lopburi.PRCP.smooth[,'MEAN']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,20), lwd=3, line.col='dodgerblue', add=TRUE)
polar.plot(lengths=as.numeric(lopburi.PRCP.smooth[,'MEAN'])+as.numeric(lopburi.PRCP.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,20), lwd=2, line.col='red', add=TRUE)
polar.plot(lengths=as.numeric(lopburi.PRCP.smooth[,'MEAN'])-as.numeric(lopburi.PRCP.smooth[,'SD']), polar.pos=(1:366)*360/366, rp.type='p', clockwise=TRUE, start=0, radial.lim=c(0,20), lwd=2, line.col='red', add=TRUE)



# A SCRIPT TO PLOT A BARPLOT OF MEAN MONTHLY RAINFALL VALUES. FIGURE 4B
#calculating monthly rainfall values
mean_rainfall<-as.data.frame(lopburi.PRCP.smooth)


mean <-c("MEAN")

mean_rainfall_new <-mean_rainfall[mean]

jan <- sum(mean_rainfall_new[c(1:31), c(1)]/10)
feb <- sum(mean_rainfall_new[c(32:59), c(1)]/10)
march <-sum(mean_rainfall_new[c(60:90), c(1)]/10)
april<-sum(mean_rainfall_new[c(91:120), c(1)]/10)
may<-sum(mean_rainfall_new[c(121:151), c(1)]/10)
june<-sum(mean_rainfall_new[c(152:181), c(1)]/10)
july <-sum(mean_rainfall_new[c(182:212), c(1)]/10)
aug<-sum(mean_rainfall_new[c(213:243), c(1)]/10)
sep<-sum(mean_rainfall_new[c(244:273), c(1)]/10)
oct<-sum(mean_rainfall_new[c(274:304), c(1)]/10)
nov <- sum(mean_rainfall_new[c(305:334), c(1)]/10)
dec <-sum(mean_rainfall_new[c(335:365), c(1)]/10)

MONTH <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

rain <- c(jan, feb, march, april, may, june, july, aug, sep, oct, nov, dec)

monthly_mean_rainfall <-data.frame(MONTH, rain)

#creating a barplot

par(mai=c(0.5,0.5,0.5,0.5), las=2)
barplot(monthly_mean_rainfall$rain, main = "Mean Rainfall Lopburi: 1960-1990", ylim=c(0,300),
        xlab = "mm",
        ylab = "month",
        names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        col = "blue")

##PLOTTING ANNUAL VARIABILITY IN RAINFALL: FIGURE 5 

library(dplyr)
library(tidyverse)

GHCN.data <- get_ghcn_daily(template=ASIA_poly, label="Lopburi_GHCN", elements=c("PRCP"), raw.dir=".../EXTRACTIONS/GHCN", extraction.dir = ".../EXTRACTIONS/GHCN/", standardize=T)

#selecting data from the Lopburi station
ghcn_dat<-GHCN.data$tabular$TH000048426$PRCP

#prcp<-lapply(ghcn_dat, "[[", "PRCP")

prcp<-do.call("rbind", ghcn_dat)

#prcp$station<-substring(row.names(prcp),1,11)
#row.names(prcp)<-1:nrow(prcp)

#I'm going to convert to "long" format for easier calculations.
# The gather function is from the tidyr package
#prcp_long<-gather(prcp, Day, D1:D31)

#prcp_long<-gather(prcp, Day, D1:D31)

#Using the Gather function
prcp_long<-ghcn_dat %>% gather (Day, PRECIP, D1:D31)

#summarizing data by year

prcp_long %>% group_by(YEAR) %>% 
  summarise(total_rain=sum(PRECIP,na.rm=TRUE)/10) -> rain_years_str

##plotting x y line plot of annual data

x<-rain_years_str$YEAR
y<-rain_years_str$total_rain
xrange <-range(rain_years_str$YEAR)
yrange <-range(rain_years_str$total_rain)

#plotting set up
par(family="mono")
par(mai=c(1,1,1,1))
plot(xrange, yrange, type="n", xlab="Year", ylab="mm of rainfall")

#add lines

lines(x,y, type="b", pch=20)
abline(h=1000)

##END## 