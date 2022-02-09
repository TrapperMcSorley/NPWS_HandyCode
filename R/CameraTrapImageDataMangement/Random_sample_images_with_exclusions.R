
###randomly select images form a folder
##selects even number from eachsubfolder within the randomimages_dir
##only selc images that are not in the Exclude_image_dir.
#Goldimages trained the model so should be excluded from hold out testing.

library(fs)

#___imports____
Exclude_images_dir <- path("E:\\GoldImages")
randomimages <- path("E:\\MegaDetectorOutput")
dest_dir <- path("E:\\RandomSelection")
number_of_images <- 600

#vector of sub directories
randomimages_dir <- dir_ls(randomimages , type = "directory" , recurse = TRUE)
#divide images evenly by sub directories
Images_per_dir <- number_of_images/length(randomimages_dir)
#path of images to exclude
Exclude_images <- path_file(dir_ls(Exclude_images_dir , type = "file" , glob = "*.jpg|*.JPG" , recurse = TRUE))

#___function_____
TakeSampleImages <- function(images_dir , Exclude_images = "" , Images_per_dir){
  imagefiles_abs <- dir_ls(images_dir , type = "file" , glob = "*.jpg|*.JPG")
  imagefiles_rel <- path_file(imagefiles_abs)

  
  if (!Exclude_images == ""){ # remove excluded images
    Excluded <- !imagefiles_rel %in% Exclude_images
    #sample whole population if sample sizeis larger than number of images
    if (length(imagefiles_rel[Excluded]) <= Images_per_dir){
      return(imagefiles_abs[Excluded])
    } else {
      return (sample(imagefiles_abs[Excluded] , size = Images_per_dir , replace = FALSE))
    }
    
  } else{ #nothing to exculde
    if (length(imagefiles_rel) <= Images_per_dir){ #are the number of images less than the sample size
      return(imagefiles_abs)
    }else{
      return(sample(imagefiles_abs, size = Images_per_dir , replace = FALSE))
    }
  }
  
}

#___run function____

myrandomimages <- path(unlist(sapply(
  randomimages_dir  ,
  FUN = TakeSampleImages ,
  Images_per_dir = Images_per_dir ,
  Exclude_images = Exclude_images
) , use.names = FALSE))

# replace directory in file names
copyTo <- gsub(pattern = randomimages , replacement = dest_dir , x = myrandomimages)
#make desination dir               
dir_create(path_dir(copyTo))
#copy files
file_copy(path = myrandomimages , new_path = copyTo)
