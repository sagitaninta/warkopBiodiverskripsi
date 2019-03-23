#=================================================
# Analisis Spasial dari Data yang Sudah dirapikan
#=================================================

rm(list=ls())

spatialPackages<-c("dplyr", "sp", "raster", "rgdal", "ggmap", "PBSmapping", "rvest")
for(lib in spatialPackages[!spatialPackages %in% installed.packages()]) 
  {install.packages(lib,dependencies=TRUE)}
sapply(spatialPackages,require,character=TRUE)

# loading spatial packages
library(sp)
library(raster)   
library(rgdal)
library(PBSmapping)
library(ggmap)
library(rvest)

## Reading cleaned data
sulbat<-read.csv("SulawesiBat_clean.csv")

## Making spatial data
sulbat_mat<-cbind(sulbat$decimallongitude,sulbat$decimallatitude) # extracting coordinates
sulbat_mat<-sulbat_mat[complete.cases(sulbat_mat),] # removing rows with NA
row.names(sulbat_mat)<-1:nrow(sulbat_mat) # giving numeric rownames to each points

glimpse(sulbat_mat)
head(sulbat_mat)
nrow(sulbat_mat)

llCRS <- CRS("+proj=longlat +ellps=WGS84") # getting projection
sulbat_sp<-SpatialPoints(sulbat_mat, proj4string = llCRS) # assigning projection for the coordinates
glimpse(sulbat_sp)
bbox(sulbat_sp) # returns the bounding box of the coordinates

# Getting Indonesia Shapefile
indonesia<-getData('GADM', country='IDN', level=1)  ##Get the Province Shapefile for Indonesia (internet needed)
glimpse(indonesia)
indonesia$NAME_1

# To get only Sulawesi, then...
sulprovs<-c("Sulawesi Barat","Sulawesi Selatan","Sulawesi Tengah","Sulawesi Tenggara","Sulawesi Utara","Gorontalo")
sulawesi <- subset(indonesia, NAME_1 %in% sulprovs)
glimpse(sulawesi)
writeOGR(sulawesi, ".", "Sulawesi2", driver="ESRI Shapefile")
slws = importShapefile("Sulawesi2.shp")

# Making a plot of Sulawesi map with bat occurrence data
plotPolys(slws, projection=TRUE)
plot(sulbat_sp, axes=TRUE, add=TRUE)

# Zooming in Sulawesi Selatan, then...
sulsel<-indonesia[indonesia$NAME_1=="Sulawesi Selatan",]
writeOGR(sulsel, ".", "Sulawesi Selatan", driver="ESRI Shapefile")
slsl = importShapefile("Sulawesi Selatan.shp")
plotPolys(slsl, projection=TRUE)
plot(sulbat_sp, axes= TRUE, add=TRUE)
