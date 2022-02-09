
##retrieve EcoHealth estate polygons from file geo DB


# libraries ---------------------------------------------------------------
library(arcgisbinding)
library(sf)
library(fs)
library(dplyr)
library(ggplot2)

arc.check_product()

#source get bionet florasurvey by bbox
source("c:/Users/mcsorleya/OneDrive - DPIE/Camera trap data standards/R/get_bionet_floraSurvey_byBbox.R")


# load data from P:drive --------------------------------------------------

#load flight paths
setwd("c:/Users/mcsorleya/OneDrive - DPIE")

estate_polygons_name <- st_layers(dsn = "EcoHealth.gdb")
estate_polygons_name  <- estate_polygons_name$name[grepl("*estate$" ,estate_polygons_name$name )]
estate_polygons_name <- estate_polygons_name[!estate_polygons_name == "EcoHealth_estate"]


#import each layer
estate_polygons <- vector('list' , length(estate_polygons_name))
for (i in seq_along(estate_polygons)){
  estate_polygons[[i]] <- st_read(dsn = "EcoHealth.gdb" , layer = estate_polygons_name[i] )
  names(estate_polygons)[i] <- estate_polygons_name[i]
}

#get bounding box for each layer
for (i in 1:length(estate_polygons)){
  bbox <- st_bbox(estate_polygons[[i]])
  minlon <- bbox[1]
  minlat <- bbox[2]
  maxlon <- bbox[3]
  maxlat <- bbox[4]
  
  outdir <- paste0("EcoHealth_files/" , estate_polygons_name[i])
  dir_create(outdir)
  
  get_bionet(maxlat = maxlat , minlat = minlat , maxlon = maxlon , minlon = minlon,
             outdir =  outdir)
}


# open the files and match to polygons ------------------------------------

# read in data ------------------------------------------------------------
Project_dir <- "c:/Users/mcsorleya/OneDrive - DPIE/EcoHealth_files/"
gdbpath <- "c:/Users/mcsorleya/OneDrive - DPIE/EcoHealth.gdb/"
#read in each floraurvey data set
#list the files that include bionet_

for (i in 1:length(estate_polygons)){
  #find the bionet data and read it in 
  Site_dir <- path(Project_dir , estate_polygons_name[i])
  bionet_flora_paths <- dir(path = Site_dir ,  recursive = T, pattern = "bionet_" , full.names = T )
  bionet_flora <- read.csv(bionet_flora_paths)
  
  #convert to sf class
  binot_flora_sf <- st_as_sf(bionet_flora , coords = c("decimalLongitude","decimalLatitude"))                     
  st_crs(binot_flora_sf) <- st_crs(estate_polygons[[i]])
  
  #clip to reserve polygon
  binot_flora_clipped <-  st_filter(binot_flora_sf ,estate_polygons[[i]] )
  
  #get the dataset path for saving
  dataset <- sub("_.*$" , "" ,estate_polygons_name[i] )
  dataset_path <- path(gdbpath ,  paste0(dataset, "_florasurveys"))
  
  #save the data
  arc.write(path =dataset_path,
            data =  binot_flora_clipped ) 

}


# summaries the fauna surveys by reserve ----------------------------------

#read the data for db
flors_plot_xy_path <- st_layers(dsn = "EcoHealth.gdb")
flors_plot_xy_path  <- flors_plot_xy_path$name[grepl("*florasurveys$" ,flors_plot_xy_path$name )]

flors_plot_xy <- vector('list' , length(flors_plot_xy))
for (i in seq_along(flors_plot_xy)){
  flors_plot_xy[[i]] <- st_read(dsn = "EcoHealth.gdb" , layer = flors_plot_xy_path[i] )

  names(flors_plot_xy)[i] <- sub("_.*$" , "" ,flors_plot_xy_path[i] )
}


flora_plot_dt <- data.table::rbindlist(flors_plot_xy, fill = TRUE , idcol = "EcoHealthSite")

flora_plot_dt$posix <- as.POSIXct(flora_plot_dt$visitDate)

flora_after1990 <- flora_plot_dt %>% filter(posix>= as.POSIXct("1990-01-01"))

ggplot(flora_after1990 , aes(x=posix))+
  geom_histogram()+
  facet_wrap(~EcoHealthSite , scale="free_y")+
  ggtitle("Bionet flora survey" , subtitle = "Histogram of last visit: 1990+")

ggsave("EcoHealth_files/histogram flora survey dates.png")

flora_plot_dt$visitNo <- as.integer(flora_plot_dt$visitNo)

ggplot(flora_plot_dt %>% filter(visitNo >1) , aes(x=visitNo))+
  geom_histogram()+
  facet_wrap(~EcoHealthSite , scale="free_y")+
  scale_x_continuous(breaks = 1:10)+
  ggtitle("Bionet flora survey" , subtitle = "Histogram of visistNo: plots with more than 1")

ggsave("EcoHealth_files/histogram flora survey no of site plot visists.png")

