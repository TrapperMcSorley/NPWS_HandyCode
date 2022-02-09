cd_wildcount <- read.csv("C:/Users/mcsorleya/OneDrive - DPIE/AI for Camera Traps/Species to train/images_with_detections_wildcount.csv")
cd_vpm <- read.csv("C:/Users/mcsorleya/OneDrive - DPIE/AI for Camera Traps/Species to train/images_with_detections.csv")
library(lubridate) 
library(dplyr)


my_tableYears = function(cd){
cd$CreateDate <- as.character(cd$CreateDate)
cd$Date <- sapply(strsplit(cd$CreateDate," "), `[`, 1)
cd$posix <- parse_date_time(cd$Date  , c("dmY", "ymd") )
cd$year <- year(cd$posix)
images_per_year <- cd %>% group_by(year) %>% summarise(no_images= n())
return(images_per_year)
}

image_per_VPM = my_tableYears(cd_vpm)
image_per_VPM$pro = "VPM"


image_per_WC = my_tableYears(cd_wildcount)
image_per_WC$pro = "WildCount"

image_per_year <- image_per_WC %>% bind_rows(image_per_VPM)


write.csv(image_per_year , "C:/Users/mcsorleya/OneDrive - DPIE/AI for Camera Traps/Species to train/images_per_year.csv")

library(ggplot2)
ggplot(image_per_year %>% filter(!year==2021) , aes(x=year , y=no_images , fill = pro))+
  geom_bar(stat= "identity" , position = position_dodge())


ggsave("C:/Users/mcsorleya/OneDrive - DPIE/AI for Camera Traps/Species to train/Images_per_year.png")  
