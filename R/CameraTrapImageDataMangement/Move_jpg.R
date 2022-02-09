rm(list = ls())
library(exifr)
library(stringr)
library(fs)
library(tictoc)

#change these to match the copy from and copy to locations. These can be the same location
#currently set up to rename files based on the file structer of [Project]/[CameraLocationID]/[SubFolder] Or images.jpg
Copy_From <-  "Z:/OEH SOS Share/Brush-tailed rock-wallaby/1. Images/Watagans"
Copy_To <- "D:/Watagans-unfiltered"
#-------------------------------------------------------------------------------------------------------------------------------

#extract metadata to get the time stamp
tic.clearlog()
tic("start" )

headers <- c("FileName" ,"CreateDate" , "Keywords" , "AmbientTemperature" , "BatteryVoltage" , "Sequence" , "EventNumber")

#imagefiles <- list.files(Copy_From , full.names = TRUE , recursive = TRUE)
tic("get image file info" )
imagefiles <- dir_ls(Copy_From , recurse = TRUE , type = "file" , glob = "*.JPG|*.jpg" , ignore.case = TRUE)
toc(log = TRUE)

# tic( "get image metatdata", log = TRUE)
# ImageData <- read_exif(imagefiles , tags = headers)
# toc(log = TRUE)

#CameraImages <- ImageData[!sapply(ImageData$Keywords, is.null),]
                         

new_fn <- str_replace_all(imagefiles , pattern = path_dir(imagefiles) , replacement = Copy_To)


tic("copy files", log = TRUE)
file_copy(path = imagefiles , new_path = new_fn , overwrite =TRUE)
toc(log = TRUE)


# #write image data to new folder
# 
# keywords <- data.frame(array(dim = c(length(CameraImages$Keywords) , 6)))
# colnames(keywords) = paste0("Tag" , c(1:5))
# if (class(CameraImages$Keywords) =="list"){ 
#   for(i in 1:length(CameraImages$Keywords)){
#     l <- length(unlist(CameraImages$Keywords[i]))
#     if (l >0){
#       for(j in 1:l)  keywords[i,j] <- unlist(CameraImages$Keywords[i])[j]
#     }
#   }
# }else {
#   for(i in 1:length(CameraImages$Keywords)){
#     f <- CameraImages$Keywords[i]
#     if(!is.na(f)){
#       if(substr(f, start = 1 , stop = 2) == "c("){
#         f <- sub(pattern = substr(f, start = 1 , stop = 2) , replacement = "" , x = f , fixed = TRUE)
#         f <- substr(f,1,str_length(f)-1)
#         f <- gsub(pattern = "\"" , replacement = "" , x = f , fixed = TRUE)
#         f <- gsub(pattern = ", " , replacement = "," , x = f , fixed = TRUE)
#       }
#       n=str_count(f,pattern = ",")
#       for(j in 1:(n+1)) keywords[i,j] <- as.character(as.factor(unlist(strsplit(f , split = ',' , fixed = TRUE))[j]))
#       print(i)
#       flush.console()
#       f= NULL
#     }}}
# 
# CameraImages.2 <- as.data.frame(cbind(CameraImages[,-8] , keywords))
# 
# STAMP <- format(strptime(timestamp(), format="##------ %a %b %d %H:%M:%S %Y ------##"), "%Y%m%d%H%M%S")
# CameraImages.2$SourceFileNew <- new_fn
# write.csv(CameraImages.2 , paste0(Copy_To ,"/imagelist-" , STAMP ,".csv"))


####
toc(log=TRUE)
unlist(tic.log())
length(new_fn)
sum(file_size(imagefiles))

