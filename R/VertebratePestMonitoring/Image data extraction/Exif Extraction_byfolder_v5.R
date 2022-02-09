

library(stringr)
library(fs)
library(plyr)

####extract metadata####
#-----------------------------

actualStart <- Sys.time()

# inputs ------------------------------------------------------------------
ParentDir = "x:\\OEH SOS Share\\Pests\\21. Image Database\\Sth Coast\\Bemboka Peak\\20210525"
Outfile <- "x:\\OEH SOS Share\\Pests\\10.DataAnalysis\\10.1 Data Processing\\Image text files\\Bemboka Peak_20210525"

#Directory where exiftools is saved
ExiftoolLoc = "c:/Exiftool"

# Functions ---------------------------------------------------------------
    #extract image metadata
    myextractImages <- function(Outfile , ExiftoolLoc, ImageDir){
      startTime <- Sys.time()  
      
      if (!dir.exists(path_dir(Outfile))) stop(paste0(path_dir(Outfile) , "  doesn't exists"))
      Outfile_2 <- gsub(path_dir(ParentDir) , path(Outfile) , path(ImageDir))
      Outfile_2 <- paste0(Outfile_2 , ".txt")
      
      #check imputs
      if (file.exists(Outfile_2)) stop(paste0(path(Outfile_2) , "  already exists"))
      
      if (!file.exists(ExiftoolLoc)) stop(paste0(ExiftoolLoc , " doesn't exist"))
      if (!dir.exists(ImageDir)) stop(paste0(ImageDir , " doesn't exist")) 
      
      #create dir
      dir_create(path_dir(Outfile_2))
      
      #writeFile <- file(Outfile)
      file_create(Outfile_2 )
      
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
      arg <- paste("-n -csv -q -b -ext jpg -ext JPG", headers , shQuote(mydir) , sep =  " ")
      
      system2(command = command , args = arg , stdout = Outfile_2)
      print(Sys.time() - startTime)
      cat("\n" , "---" ,Outfile_2 )
      print(Sys.time() - startTime)
      if(file_exists(Outfile_2)) return(try(read.csv(Outfile_2 , stringsAsFactors = FALSE , na.strings = "NA" , )))
      
    }


# find image directories from parents dir ---------------------------------
dir_daughter <- dir_ls(ParentDir , type = "directory" , recurse = TRUE)
ImageData_2 <- list()

ImageData_2 <- lapply(dir_daughter , FUN = myextractImages , 
                    Outfile = Outfile,
                    ExiftoolLoc = ExiftoolLoc)

imagedata_merge <- ldply(ImageData_2)
  

# format the output -------------------------------------------------------
  #remove images without metadata
imagedata_merge <- imagedata_merge[!is.na(imagedata_merge$CreateDate),]  
   
  
#change date format
imagedata_merge$CreateDate <- as.POSIXct(imagedata_merge$CreateDate , format="%Y:%m:%d %H:%M:%S" ,tz = "AUstralia/Brisbane")
  
  #Parse filename to site info, break elements by "_"
  Siteinfo <- as.data.frame(str_split(path_ext_remove(imagedata_merge$FileName) , pattern = "_" , simplify = TRUE ))
  imagedata_merge <- cbind(imagedata_merge , Siteinfo)
  cat(nrow(imagedata_merge) , " images proccessed\n")
  imagedata_merge <- imagedata_merge[!imagedata_merge$Keywords == "",]
  cat(nrow(imagedata_merge) , " images with tags\n")
# write image data to file ------------------------------------------------

  t.stamp <- format(Sys.time() , format= "%Y%m%d%H%M%S")
  write.csv(imagedata_merge , paste0(path(Outfile),"/" , basename(ParentDir), "_ImageData" , t.stamp, ".csv"))
  print(Sys.time()-actualStart)
#######

  