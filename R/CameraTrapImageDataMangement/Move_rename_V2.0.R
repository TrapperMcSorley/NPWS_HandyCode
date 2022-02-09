# script to rename a bulk load of images in a standadised file structure
#'and rename to Project_CameraStation_surveyDate_originalFileName.jpg
#'Should work on old model recony camera images with original file names IMG_xxxx.jpg
#'Needs some editing if using new reconyx or another model of camera
#'If there is sa mix of cmaera then well  need to add in some checking statements
#'
#'Assumed folder structure
#'
#'folder structure:
#'Site
#'   SurveyDate
#'           CamerStation
#'                    first9999
#'                    second9999
#'                    ....
#'
#'
#__________________________________________________________________________
#rm(list = ls())
library(fs)
library(stringr)
mypath <- c("d://Data/AlpineHorse/Horse_AM/")
Destination <- "d://Data/AlpineHorse/renamed/Horse_AM/"
Project.identifier <- "AlpineHorse"

#funtions________________

renamefolder <- function(foldername){
  SiteFolderNames <- gsub(pattern = "\\." , 
                          replacement = "_" ,
                          substr(foldername , start = 1, stop = 5))
  
  return(SiteFolderNames)
}

#sanatise stationIDs 
MakeStationID <- function(station){
  ID <- data.frame(ID = toupper(station))
  ID$num <- NA
  ID$Subs <- NA
  ID$ID <- sub(pattern = " " , "" , ID$ID)
  ID$num <- str_pad(gsub("[[:alpha:]]", "" , ID$ID),3,pad = "0")
  ID$Subs <- gsub("[[:digit:]]", "" , ID$ID)
  ID$station <- paste0(ID$Subs , ID$num)
  return(ID$station)
}


#_____________________________


mypathLong <- dir_ls(mypath , recurse = TRUE , type= "file" , glob = "*.jpg|*.JPG")
mypathDir <- path_dir(mypathLong)# get the directory part of the path

#remove duplicated so theres not an entry for each image
mypathDuplictes <- mypathDir[!duplicated(mypathDir)] 

# split up all the directories
mypathfinals <- data.frame(mypathDuplictes , str_split(string = mypathDuplictes , pattern = "/" , simplify = TRUE)) 

#make a new path for copying to detination folder

mypathfinals$newpath <- path(paste(Destination ,mypathfinals$X5 ,renamefolder(mypathfinals$X6) , mypathfinals$X7 ,sep= "/"))



#findfilesinonefolder
for (i in 1: length(mypathfinals$mypathDuplictes)){
  findfilesfolder <- path(mypathfinals[i,1] )
  myfiles <- dir_ls(findfilesfolder , recurse = TRUE , type= "file" , glob = "*.jpg|*.JPG") 
  if (!length(myfiles) == 0) { #skip empty folders
    myfileshort <- path_file(myfiles)
    
    #make a destination folder
    destinationFolder <- path(mypathfinals[i,"newpath"] )
    dir_create(destinationFolder)
    
    # #this part needs work to make it extract your original file names depending on the cmaera model
    # #____________________________________________________________________________________________________________________________
    # #Pairback the filename back to the original name
    # startLocs <- as.integer(regexpr(pattern = "IMG" , text = myfileshort)) 
    # #find where IMG text is in the image file name
    # myfileshort_2 <- vector(length = length(myfileshort))
    # iter = 1
    # for (j in 1: length(myfileshort)){
    #   myfileshort_2[j] <- paste0(substr(start = startLocs , stop = startLocs+ 3 , myfileshort[j]),
    #                              str_pad(iter , width = 5 , pad = "0"))
    #   iter =iter +1
    # }
    #____________________________________________________________________________________________________________________________
    
    # add surveydate and stationID to the image file name
    myfileNew <-
      paste0(
        Project.identifier,
        "_" ,
        renamefolder(mypathfinals$X6[i]),
        "_" ,
        mypathfinals$X5[i],
        "_" ,
        myfileshort,
        ".jpg"
      )
    
    if(length(myfileNew[duplicated(myfileNew)]) >0){
      print("FILE DUPLICATION" , mypathfinals$X6[i] , mypathfinals$X5[i] , "Proccess stopped")
      break
      }
    pathDestination <- paste0(destinationFolder ,"/" ,  myfileNew) #combine filename with destination path
    
    #Move the image files to nex destination with new image names
    try(file_copy(path = myfiles , new_path = pathDestination))

  }
  print (mypathfinals$newpath[i])
  flush.console()
}


