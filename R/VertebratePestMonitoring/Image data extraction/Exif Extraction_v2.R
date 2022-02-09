rm(list = ls())
library(stringr)
library(fs)
library(plyr)

####extract metadata####
#-----------------------------

actualStart <- proc.time()

# inputs ------------------------------------------------------------------

Outfile <- "Z:\\OEH SOS Share\\Pests\\10.DataAnalysis\\Image text files\\Feb2021"
ExiftoolLoc = "c:/Exiftool"
ImageDir = "Z:\\OEH SOS Share\\Pests\\2.Central\\Images\\Watagans\\Ingles"

# Functions ---------------------------------------------------------------
    #extract image metadata
    myextractImages <- function(Outfile , ExiftoolLoc, ImageDir){
      startTime <- Sys.time()  
      
      #check imputs
      if (file.exists(Outfile)) stop(paste0(path_file(Outfile) , "  already exists"))
      if (!dir.exists(path_dir(Outfile))) stop(paste0(path_dir(Outfile) , "  doesn't exists"))
      if (!file.exists(ExiftoolLoc)) stop(paste0(ExiftoolLoc , " doesn't exist"))
      if (!dir.exists(ImageDir)) stop(paste0(ImageDir , " doesn't exist")) 
      
      
      #writeFile <- file(Outfile)
      file.create(Outfile)
      
      headers <-
        paste(
          "FileName" ,
          "CreateDate" ,
          "Keywords" ,
          "AmbientTemperature" ,
          "BatteryVoltage" ,
          "Sequence" ,
          "EventNumber" ,
          "ExifImageWidth" ,
          "ExifImageHeight" ,
          sep = " -")
      
      headers <- paste0("-" , headers)
      
      headers
      
      mydir <- ImageDir
      command <- paste0(ExiftoolLoc , "/exiftool.exe")
      arg <- paste("-n -csv -q -b -r -ext jpg -ext JPG", headers , shQuote(mydir) , sep =  " ")
      
      system2(command = command , args = arg , stdout = Outfile)
      print(Sys.time() - startTime)
      return(read.csv(Outfile , stringsAsFactors = FALSE , na.strings = "NA"))
    }

   
# extract image metadata --------------------------------------------------

ImageData <- myextractImages(Outfile = Outfile ,
                                 ExiftoolLoc = ExiftoolLoc ,
                                 ImageDir = ImageDir)

# format the output -------------------------------------------------------
  #remove images without metadata
  ImageData <- ImageData[!is.na(ImageData$AmbientTemperature),]  
   
  #change date format
  ImageData$CreateDate <- as.POSIXct(ImageData$CreateDate , format="%Y:%m:%d %H:%M:%S" ,tz = "AUstralia/Brisbane")
  
  #Parse filename to site info, break elements by "_"
  Siteinfo <- as.data.frame(str_split(path_ext_remove(ImageData$FileName) , pattern = "_" , simplify = TRUE ))
  ImageData <- cbind(ImageData , Siteinfo)


# write image data to file ------------------------------------------------

  t.stamp <- format(Sys.time() , format= "%Y%m%d%H%M%S")
  write.csv(ImageData , paste0(path_dir(Outfile), "/ImageData" , t.stamp, ".csv"))

#######
