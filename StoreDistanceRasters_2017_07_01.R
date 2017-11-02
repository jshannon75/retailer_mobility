library(sf)
library(rgdal)
library(tidyverse)
library(nabor)


#Read in a list of census blocks
blocks<-readOGR(".","atl_blocks")
blocks<-spTransform(blocks,CRS("+init=epsg:32616 +proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Read in a list of shapefiles for retailer locations in each chain in each year
storeFiles_names <- list.files(path='dist_raster/storepoints/',pattern='.shp')
storeFiles_names<-substr(storeFiles_names,1,nchar(storeFiles_names)-4)

#Create a function to measure distance to a given set of store points from all census blocks
storeDist<-function(filename){
  storepoints<-readOGR("dist_raster/storepoints",filename)
  storepoints<-spTransform(storepoints,CRS("+init=epsg:32616 +proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  knn<-gDistance(blocks,storepoints,byid=TRUE)
  knn_short<-apply(knn, 2, min)
  knn_short
}

#Apply the function to the stores read in on line 14 and write the results
testfiles<-lapply(storeFiles_names,storeDist)

blocks_sf<-st_as_sf(blocks)
blocks_sf$neardist<-(knn_short)
st_write(blocks_sf,"dist_raster/blocks_test2.shp")


