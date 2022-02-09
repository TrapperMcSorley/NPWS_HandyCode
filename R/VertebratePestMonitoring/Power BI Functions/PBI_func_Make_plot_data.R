dataset = site_day_operating
detections = site_day_species

library(dplyr)

species = unique(detections$Species)
results <- data.frame()
for (i in 1:length(species)){
  print(species[i])
  species_detections <- detections  %>%  filter(Species == species[i])%>% select(-Night, -Species)
  print(length(species_detections[,1]))
  temp <- data.frame(dataset %>% left_join(species_detections))
  temp$Species <- species[i]
  results <- rbind(results,temp)
  
}
rm(temp)
results
