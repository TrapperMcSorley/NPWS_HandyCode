####Full table of sites and image tagged and proccessed
#libaries
library(stringr)
library(plyr)
library(fs)
Path <- "D:/Data/Plains wanderer image data/Plains wanderer_ImageData20210906230955.csv"
#
SBB <- read.csv(Path , stringsAsFactors = FALSE)


#remove erroneoulst renamed files

SBB$posix <- as.POSIXct(SBB$CreateDate , tz = "Australia/Brisbane")

tagcolumns <- which(substr(colnames(SBB), 1,3) == "Tag") 

#funtion to find bits in the keywords
myresults <- data.frame(array(dim = c(length(SBB[,1]) , length(tagcolumns))))
  
FindSBB <- function(Tag , column){
    return(as.integer(grepl(pattern = Tag , x = column , ignore.case = TRUE)))

}

SBB$SBB <- FindSBB(Tag = "Southern Brown" , column = SBB$Keywords)
SBB$Fox <- FindSBB(Tag = "Fox" , column = SBB$Keywords)
SBB$Cat <- FindSBB(Tag = "Cat" , column = SBB$Keywords)
SBB$PygmyPos <- FindSBB(Tag = "Pygmy possum" , column = SBB$Keywords)

#SBB <- cbind(SBB , str_split(SBB$FileName , pattern = "_" , simplify = TRUE))
colnames(SBB)[14:15] <- c("stationID" , "SurDate")
SBB.summary <- ddply( .data = SBB,
                         .variables = .(SurDate , stationID),
                         .fun = summarise,
                         numberofimages = length(FileName),
                         tagged =  sum(!is.na(Keywords)),
                         Cat = sum(Cat , na.rm=TRUE) ,
                         Fox = sum(Fox , na.rm=TRUE),
                         #HeathMon = sum(Heath, na.rm=TRUE),
                         #PygmyPos = sum(PygmyPos, na.rm=TRUE),
                         FirstImage = min(posix , na.rm=TRUE),
                         LastImage = max(posix , na.rm=TRUE))

# write.csv(SBBFox.summary , "Z:/OEH SOS Share/Camera images/SthBrnBandicootFox/1.MacUniColab/SummaryFiles/FoxCameras_summary.csv")
 write.csv(SBB.summary , paste0(path_ext_remove(Path) , "_summary.csv"))
# write.csv(SBBTrial.summary , "Z:/OEH SOS Share/Camera images/SthBrnBandicootFox/1.MacUniColab/SummaryFiles/TrialCameras_summary.csv")
SBB$Day <- as.Date(SBB$posix, format = "%Y-%m-%d")

# heath -------------------------------------------------------------------


heath.monitor <- SBB[SBB$Heath == 1, ]
 heath.monitor <- ddply(heath.monitor , .(stationID , Day) ,
                        summarise ,
                        images = length(Day) )
 
 write.csv(heath.monitor,paste0(path_ext_remove(Path) , "_heath.csv"))

# poss --------------------------------------------------------------------

 
 pygmy.possum <- SBB[SBB$PygmyPos == 1, ]
 pygmy.possum <- ddply(pygmy.possum , .(stationID , Day) ,
                        summarise ,
                        images = length(Day) )
 
 write.csv(pygmy.possum,paste0(path_ext_remove(Path) , "_Pygmypos.csv"))

# SBB ---------------------------------------------------------------------

 SouthernBrown <- SBB[SBB$SBB == 1, ]
 SouthernBrown <- ddply(SouthernBrown , .(stationID , Day) ,
                       summarise ,
                       images = length(Day) )
 
 write.csv(SouthernBrown,paste0(path_ext_remove(Path) , "_SouthernBrown.csv"))
 