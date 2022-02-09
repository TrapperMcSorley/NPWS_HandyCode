##geoJSON from feature class




# libraries ---------------------------------------------------------------
library(arcgisbinding)
library(sf)
library(tidyr)
library(fs)
library(ggplot2)
library(tidyverse)


arc.check_product()

wd_path <- "c:/Users/mcsorleya/OneDrive - DPIE"
out_path <- "c:/Users/mcsorleya/OneDrive - DPIE/EcoHealth_files/GeoJSON/"

feature_name <-  "EcoHealth_estate_internal_boundaries"

reseve_lookup_path <- "c:/Users/mcsorleya/OneDrive - DPIE/EcoHealth_files/Reserve look up table.csv"
reseve_lookup <- read.csv(reseve_lookup_path , stringsAsFactors = F)

#import the feature class
EcoHealth_estate <- st_read(dsn = path(wd_path , "EcoHealth.gdb") , layer = feature_name)  

#join with look up
EcoHealth_estate <- EcoHealth_estate %>%
  rename(NPWS_Estate = NAME_SHORT) %>%
  left_join(reseve_lookup , by ="NPWS_Estate")

#check the geometires are right
ggplot()+
  geom_sf(data= EcoHealth_estate%>% filter(Eco_Health %in% "Sturt Narriearra") , aes(fill = Eco_Health))+
  theme(legend.position = "none") 

#remove internal polygon boudaries and merge to ecohealth site
EcoHealth_estate_union <- EcoHealth_estate %>%
  group_by(Eco_Health , Eco_Health_short) %>%
  summarise()

#merge to NPWS_Estate
EcoHealth_estate <- EcoHealth_estate %>%
  group_by(NPWS_Estate , Eco_Health , Eco_Health_short) %>% 
  summarise()


ggplot()+
  geom_sf(data= EcoHealth_estate_union  , aes(fill = Eco_Health))+
  geom_sf(data= EcoHealth_estate  , aes(fill = NPWS_Estate) , alpha = 0.4)+
  theme(legend.position = "none")


# branch and area geoJSON -------------------------------------------------
# PDrive <- "P:/Corporate/Themes"
# Branch <- st_read(dsn = path(PDrive, "Admin/Corporate/Corporate.gdb") , layer = "NPWS_Branches")  
# Area <- st_read(dsn = path(PDrive, "Admin/Corporate/Corporate.gdb") , layer = "NPWS_Areas")  
# 
# 
# #save to geoJson
# st_write(EcoHealth_estate_union, dsn = path(out_path, paste0(feature_name, "_removed.json")),  driver = "GeoJSON" )
# st_write(EcoHealth_estate, dsn = path(out_path, "EcoHealth_estate.json"),  driver = "GeoJSON" )
# st_write(Branch, dsn = path(out_path, "Branch.json"),  driver = "GeoJSON" )
# st_write(Area, dsn = path(out_path, "Area.json"),  driver = "GeoJSON" )
# st_write(EcoHealth_estate_union, dsn = path(out_path, paste0(feature_name, "_removed_topo.json")),  driver = "TopoJSON" )

#save to gdb
#save the data
path_to_GDB <- path(wd_path , "EcoHealth.gdb/EcoHealth_merged")
poly <- EcoHealth_estate_union
savetoGDB <- function(path_to_GDB, poly){
arc.write(path =path_to_GDB,
          data =  poly ) 
}

savetoGDB(path = path_to_GDB , poly)
