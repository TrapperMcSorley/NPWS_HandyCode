#testing new data schema for camera traps

library(dplyr)
library(tidyr)
library(fs)

# Set file paths and variables---------------------------------------------------
#always use Brisbane Australia to avoid daylight savings issues
Time_zone <-  "Australia/Brisbane"

#the start date is when the first camera was deployed at Big Yengo.
start_date <- as.POSIXct("2016-08-16" , tz = Time_zone)

Site_dates_path <- "c:/Users/mahonp/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/3.2 Formated logs/start_end_dates.csv"
fails_path <- "c:/Users/mahonp/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/3.2 Formated logs/fail_dates.csv"
dt_path <- "C:/Users/mahonp/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/3.1 Detection tables/Stations_detections.csv"

#read in the data
Site_dates <- read.csv(Site_dates_path)
fails <- read.csv(fails_path)
dt <- read.csv(dt_path)

#create a subsites variable for iteration
Subsites <- unique(Site_dates$Subsite)

#change date formats-----------------------------------------------

Site_dates$start <- as.POSIXct(Site_dates$start , tz = Time_zone)
Site_dates$finish <- as.POSIXct(Site_dates$finish , tz = Time_zone)
Site_dates$DateClosed <- as.POSIXct(Site_dates$DateClosed , tz = Time_zone)
fails$S.NotFunctioning <- as.POSIXct(fails$S.NotFunctioning , tz = Time_zone)
fails$E.NotFunctioning<- as.POSIXct(fails$E.NotFunctioning , tz = Time_zone)

#some how duplicate fails crept into the dataset. this removes them
fails <- unique(fails)

# create detection table by site ------------------------------------------

##site x day x species table

#add subsite into detections df
dt <- dt %>% left_join(Site_dates %>% select(stationID, Subsite))

site_day_species <- dt %>%
  group_by(Subsite, Species , night) %>%
  summarise(
    detections = sum(detection)
  )

site_day_species$Date <- as.Date(start_date + site_day_species$night *60*60*24)


# create station day operating table -----------------------------------------------------------------------

#build a data frame of cameras operating at each station on each day
#station x day cameras functioning
station_day_operating <- data.frame()

#iterate through each subsite
for (i in 1:length(Subsites)){
  subsite = Subsites[i]
  
  #collect the data for each subsite from the two tables
  Site_dates_subsite <- Site_dates %>% filter(Subsite %in% subsite)
  fails_subsite =  fails %>% filter(stationID %in% Site_dates_subsite$stationID)
  
  #extract the start and end date for the subsite.
  #The data will be limited to with this date range
  finish_date = max(Site_dates_subsite$finish)
  begin_date = min(Site_dates_subsite$start)
  
  #get date for any closed stations
  closed_stations <- Site_dates_subsite %>% filter(!is.na(DateClosed))
  
  #create a df of all days for all stations of the subsite with the date range
  #this is initially populated with a 1 for each day
  #but will be changed to 0 when the station isn't functioning
  station_day <- expand.grid(stationID = Site_dates_subsite$stationID,
                       Date = seq.POSIXt(from =begin_date, to = finish_date, by = 'day' ) ,
                       operating = 1L)
  
  # set when camera fails to 0
  for(l in 1:length(fails_subsite$stationID)){
    fails_station <- fails_subsite[l,]
    s_notfunct <- fails_station$S.NotFunctioning
    e_notfunct <- fails_station$E.NotFunctioning
    
    temp <- station_day %>%
      filter(stationID %in% fails_subsite$stationID[l]) %>%
      filter(Date >= s_notfunct & Date <= e_notfunct) %>%
      mutate(operating = 0L)
    
    station_day <- rows_update(station_day , temp , by = c("stationID" , "Date"))
  }
  
  #When stations closed to 0
  if (length(closed_stations[,1]) >0){
    #camera fails
    for(l in 1:length(closed_stations$stationID)){
      closed_station <- closed_stations[l,]
      close_date <- closed_station$DateClosed
      
      temp <- station_day %>%
        filter(stationID %in% closed_station$stationID[l]) %>%
        filter(Date >= close_date) %>%
        mutate(operating = 0L)
      
      station_day <- rows_update(station_day , temp , by = c("stationID" , "Date"))
    }
  }
  
  #add to running list of subsites
  station_day$night <- as.integer(difftime(station_day$Date,
                                          start_date,
                                          units = "days"))
  
  station_day_operating <- rbind(station_day_operating, station_day)
  
}


# Summarise the station x day operating to subsite level -----------------------------------------------------------------------

####Summarise station day operating to site day operating

site_day_operating <- station_day_operating %>%
  left_join(Site_dates) %>%
  group_by(Subsite, Date) %>%
  summarise(
    cameras = n(), 
    no.operating = as.integer(sum(operating))
  )



# Create a matrix of detections for each species --------------------------
species  <-  unique(dt$Species)
matricies <- vector("list" ,length = length(species))
names(matricies) <- species

for (i in 1:length(species)){
  
  #get the detections for the species, we'll add nights back in later
  species_detections <- dt  %>%  filter(Species == species[i]) %>% select(-Species)
  
  #join the detection to the data frame of stations operating
  temp <- station_day_operating %>%
    left_join(species_detections , by = c("night" , "stationID")) %>% 
    mutate(detection = if_else(operating ==1 & is.na(detection), 0L , detection)) %>% 
    select(stationID, night, detection)
  
  #create a matrix from the dataframe
  matricies[[species[i]]] <- temp %>% pivot_wider(values_from = detection , names_from = night , names_sort = T) 
  
}

rm(temp,species_detections)


# archive and save the data-----------------------------------------------------------------------

#archive
paths_to_archive <- dir_ls(path_dir(dt_path) , glob = "*.csv")

#dont archive the detection df
paths_to_archive <- paths_to_archive[!grepl("Stations_detections.csv" , paths_to_archive)]

datestamp <- strftime(Sys.time() , format = "%Y%m%d%H%M")
destination_dir <- path_join(c(path_dir(dt_path) , "archive", datestamp))
dest_path <- gsub(path_dir(dt_path) , destination_dir , paths_to_archive)
dir_create(destination_dir)

file_copy(paths_to_archive,dest_path)
if (all(file_exists(dest_path))) file_delete(paths_to_archive)


#save
for (i in 1:length(matricies)){
  matix_path <- path_join(c(path_dir(dt_path) , paste0("Matrix_", gsub(" ","",names(matricies)[i]),".csv")))
  write.csv(matricies[[i]], matix_path)
}

#site x day x species
write.csv(site_day_species , "C:/Users/mahonp/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/3.1 Detection tables/Site_detections.csv")

#station x day operation
write.csv(station_day_operating , "C:/Users/mahonp/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/3.1 Detection tables/Station_operating.csv")

#site x day operating
write.csv(site_day_operating ,"C:/Users/mahonp/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/3.1 Detection tables/Site_operating.csv")

