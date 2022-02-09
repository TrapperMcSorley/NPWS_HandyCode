#veg formation data
library(sf)
library(fs)
library(dplyr)
library(ggplot2)
library(gdal)

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

VegForm <- vector('list' , length(PCT_layers))
for (i in seq_along(PCT)){
  if ("vegetationFormation" %in% colnames(PCT[[i]])){
    VegForm[[i]] <-  PCT[[i]] %>%
      st_cast("MULTIPOLYGON") %>% 
      group_by(vegetationFormation) %>%
      summarise()
    
    VegForm[[i]]$Eco_Health_short <- sub("_PCT","",names(PCT)[i])
    names(VegForm)[i] <- names(PCT)[i]
  }
  if ("Formation" %in% colnames(PCT[[i]])){
    VegForm[[i]] <-  PCT[[i]] %>%
      st_cast("MULTIPOLYGON") %>% 
      group_by(Formation) %>%
      summarise()
    
    VegForm[[i]]$Eco_Health_short <- sub("_PCT","",names(PCT)[i])
    names(VegForm)[i] <- names(PCT)[i]
  }
  
  }
}


#load other veg form layers
#load all file with PCT

veg_form_layers <- layers$name[grepl("*_form_clipped$" , layers$name)]

#import each layer
veg_form <- vector('list' , length(veg_form_layers))
for (i in seq_along(veg_form)){
  veg_form[[i]] <- st_read(dsn = path(EcoHealth_GDB,"EcoHealth.gdb") , layer = veg_form_layers[i] )
  names(veg_form)[i] <- sub("*_veg_form_clipped$" , "", veg_form_layers[i])
}


VegForm_2 <- vector('list' , length(veg_form))
for (i in seq_along(veg_form)){
  
  VegForm_2[[i]] <-  veg_form[[i]] %>%
      st_cast("MULTIPOLYGON") %>% 
      group_by(vegForm) %>%
      summarise()
    
  VegForm_2[[i]]$Eco_Health_short <- names(veg_form)[i]
  names(VegForm_2)[i] <- names(veg_form)[i]
}  


#combine the two datasets
names(VegForm)
names(VegForm_2)

for (i in 3:length(VegForm)){
  print(colnames(VegForm[[i]]) )
 colnames(VegForm[[i]])[1] <- "vegForm"
 VegForm[[i]]$vegForm <- as.character(VegForm[[i]]$vegForm)
}

for (i in 1:length(VegForm_2)){
  print(colnames(VegForm_2[[i]]))
  VegForm_2[[i]]$vegForm <- as.character(VegForm_2[[i]]$vegForm)
}

VegForm_3 <- VegForm[3:5] %>% bind_rows()
VegForm_4 <- VegForm_2 %>% bind_rows()
VegForm_5 <- bind_rows(VegForm_3 , VegForm_4)

#plot veg form

out_dir <- "c:/Users/mcsorleya/DPIE/Ecological Health Unit - Documents/Data/Summaries of existing data/Maps/VegeationFormation/"

for (i in 1:length(unique(VegForm_5$Eco_Health_short))){

  site <- EcoHealth_sites %>% filter(Eco_Health_short %in% unique(VegForm_5$Eco_Health_short)[i])
  Veg <- VegForm_5 %>% filter(Eco_Health_short %in% site$Eco_Health_short)
  
ggplot()+
  geom_sf(data = site)+
  geom_sf(data = Veg, aes(fill= vegForm), col=NA)+
  scale_fill_brewer(palette = "RdYlBu")+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(color = "light grey"))

ggsave(path(out_dir , paste0(site$Eco_Health,"_VegForm.png" )) ,height = 10 , width = 18 , units = "cm" )
}
