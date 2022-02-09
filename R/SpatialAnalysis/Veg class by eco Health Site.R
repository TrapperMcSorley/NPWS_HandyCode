#veg class data
library(sf)
library(fs)
library(dplyr)
library(ggplot2)
library(gdal)
library(RColorBrewer)

# load data from P:drive --------------------------------------------------

#load flight paths
EcoHealth_GDB <- "c:/Users/mcsorleya/DPIE/Ecological Health Unit - Documents/Data/EcoHealthGeoDataBase"

EcoHealth_sites <- st_read(dsn = path(EcoHealth_GDB,"EcoHealth.gdb") , layer = "EcoHealth_sites")

#load all file with PCT
layers <- st_layers(path(EcoHealth_GDB,"EcoHealth.gdb"))
PCT_layers <- layers$name[grepl("*_PCT$" , layers$name)]

#import each layer
PCT <- vector('list' , length(PCT_layers))
for (i in seq_along(PCT_layers)){
  PCT[[i]] <- st_read(dsn = path(EcoHealth_GDB,"EcoHealth.gdb") , layer = PCT_layers[i] )
  names(PCT)[i] <- PCT_layers[i]
}

VegClass <- vector('list' , length(PCT_layers))
for (i in seq_along(PCT)){
  if ("vegetationClass" %in% colnames(PCT[[i]])){
    VegClass[[i]] <-  PCT[[i]] %>%
      st_cast("MULTIPOLYGON") %>% 
      group_by(vegetationClass) %>%
      summarise()
    
    VegClass[[i]]$Eco_Health_short <- sub("_PCT","",names(PCT)[i])
    names(VegClass)[i] <- names(PCT)[i]
  }
  if ("Class" %in% colnames(PCT[[i]])){
    VegClass[[i]] <-  PCT[[i]] %>%
      st_cast("MULTIPOLYGON") %>% 
      group_by(Class) %>%
      summarise()
    
    VegClass[[i]]$Eco_Health_short <- sub("_PCT","",names(PCT)[i])
    names(VegClass)[i] <- names(PCT)[i]
  }
  
  }



#load other veg Class layers
#load all file with _class_clipped in teir name

veg_Class_layers <- layers$name[grepl("*_class_clipped$" , layers$name)]

#import each layer
veg_Class <- vector('list' , length(veg_Class_layers))
for (i in seq_along(veg_Class)){
  veg_Class[[i]] <- st_read(dsn = path(EcoHealth_GDB,"EcoHealth.gdb") , layer = veg_Class_layers[i] )
  names(veg_Class)[i] <- sub("*_veg_class_clipped$" , "", veg_Class_layers[i])
}


VegClass_2 <- vector('list' , length(veg_Class))
for (i in seq_along(veg_Class)){
  
  VegClass_2[[i]] <-  veg_Class[[i]] %>%
      st_cast("MULTIPOLYGON") %>% 
      group_by(vegClass) %>%
      summarise()
    
  VegClass_2[[i]]$Eco_Health_short <- names(veg_Class)[i]
  names(VegClass_2)[i] <- names(veg_Class)[i]
}  


#combine the two datasets
names(VegClass)
names(VegClass_2)

for (i in 3:length(VegClass)){
  print(colnames(VegClass[[i]]) )
 colnames(VegClass[[i]])[1] <- "vegClass"
 VegClass[[i]]$vegClass <- as.character(VegClass[[i]]$vegClass)
}

for (i in 1:length(VegClass_2)){
  print(colnames(VegClass_2[[i]]))
  VegClass_2[[i]]$vegClass <- as.character(VegClass_2[[i]]$vegClass)
}

VegClass_3 <- VegClass[3:5] %>% bind_rows()
VegClass_4 <- VegClass_2 %>% bind_rows()
VegClass_5 <- bind_rows(VegClass_3 , VegClass_4)

#plot veg Class

out_dir <- "c:/Users/mcsorleya/DPIE/Ecological Health Unit - Documents/Data/Summaries of existing data/Maps/VegeationClass/"
dir_create(out_dir)

library(colorRamps)

levels(EcoHealth_sites$Eco_Health_short)[7] = "RoyalGarawarra"

for (i in 1:length(unique(VegClass_5$Eco_Health_short))){

  site <- EcoHealth_sites %>% filter(Eco_Health_short %in% unique(VegClass_5$Eco_Health_short)[i])
  Veg <- VegClass_5 %>% filter(Eco_Health_short %in% site$Eco_Health_short)
  
  n_col <- length(unique(Veg$vegClass))
  getPalette <- colorRampPalette(brewer.pal(12, "Paired"))(n_col)
  
ggplot()+
  geom_sf(data = site)+
  geom_sf(data = Veg, aes(fill= vegClass), col=NA)+
  scale_fill_manual(values = getPalette)+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(color = "light grey"),
        legend.text=element_text(size=6),
        legend.key.size = unit(0.2, 'cm'),
        legend.position = "bottom", legend.title = element_blank(),
        axis.text = element_blank())

ggsave(path(out_dir , paste0(site$Eco_Health,"_VegClass.png" )) ,height = 10 , width = 18 , units = "cm" )
}

