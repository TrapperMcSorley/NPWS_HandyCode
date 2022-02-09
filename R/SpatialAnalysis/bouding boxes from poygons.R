library(sf)
library(fs)
library(dplyr)
library(ggplot2)

#change
outfile <- "full file name and path"

setwd("c:/Users/mcsorleya/OneDrive - DPIE")

#chage
polygon_name <- "your shape name"

polygons <- st_read(dsn = path("EcoHealth.gdb" , polygon_name))


boundingBoxes <- data.frame()
for (i in 1:length(polygons)){
  bbox <- st_bbox(polygons[[i]])
  minlon <- bbox[1]
  minlat <- bbox[2]
  maxlon <- bbox[3]
  maxlat <- bbox[4]
  
  #change this to be the field name
  Area <- polygons$
  #--------------------------------------
  
  boundingBoxes <- rbind(boundingBoxes , data.frame(Area , minlat, maxlat, minlon, maxlon) )                   
}                    

write.csv(boundingBoxes , outfile)