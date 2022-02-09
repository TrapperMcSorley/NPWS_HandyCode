
' will return which files have changed in the folder specified. logged into the change log, wether it exists or not.
It will look at items in sub folder but not in sub sub folders or at the base folder level.
If there is no snap shot it will create one in the snapshot path
'
library(utils)
library(fs)
library(stringr)
library(dplyr)

project_dir <- "c:/Users/mahonp/DPIE/MST_NPWS_Vertebrate pest monitoring - General/"

# paths -------------------------------------------------------------------
folderpath <- path_join(c(project_dir,"1.0 Camera deployment and service logs"))
snapshot_path <- path_join(c(project_dir,"5.0 File monitoring","5.1 Snapshots"))
Changelog <- path_join(c(project_dir,"5.0 File monitoring" , "5.2 Change Log" , "Change_log.txt"))
servicelog_paths <- path_join(c(project_dir, "3.0 Power BI data"  , "3.2 Formated logs"))
deploymentlog_path <- path_join(c(project_dir,"1.0 Camera deployment and service logs" , "Camera sampling points.xlsx"))

# save a snap shot --------------------------------------------------------

Take_A_snapshot <- function(folderpath =folderpath , snapshot_path=snapshot_path){
  subpaths <- dir_ls(folderpath)
  snapshot <- fileSnapshot(subpaths)
  save_path <- path_join(c(snapshot_path, paste0(basename(folderpath), "_" , format(Sys.time() , "%y%m%d_%H%M" ),".snap")))
  saveRDS(snapshot , file = save_path)
}

# Load a snapshot ---------------------------------------------------------
Load_latest_snapshot <- function(snapshot_path){
  saved_snaps <- basename(dir_ls(snapshot_path, glob = "*.snap"))
  snap_dates<- vector(length = length(saved_snaps))
  
  for (i in 1 : length(saved_snaps)){
    file_length <- nchar(saved_snaps[i])
    snap_dates[i] <- as.POSIXct(
      substr(saved_snaps[i], start = file_length - 15 , stop = file_length-5),
      format = "%y%m%d_%H%M")
  }
  
 latest_snap_file <- saved_snaps[snap_dates == max(snap_dates)]
 latest_snap_path <- path_join(c(snapshot_path , latest_snap_file))
 latest_snap <- readRDS(file = latest_snap_path)
 return(latest_snap)
}

# Check for changes -------------------------------------------------------
Log_changes <- function(folderpath =folderpath , snapshot_path=snapshot_path){
  
  subpaths <- dir_ls(folderpath)
  latest_snap <- Load_latest_snapshot(snapshot_path)
  current_state = fileSnapshot(subpaths)
  changed_files <- changedFiles(latest_snap , current_state)
  
  if (!length(changed_files$changed) == 0 ){ #are there changes?
    print(changed_files)
    change_to_log <- cbind( changed_files$changed , as.character(Sys.time()))
    
    #append to existing log
    
    change_to_log <-  "  Files changed:"
    write(change_to_log, file=Changelog, append=TRUE , ncolumns = 2 , sep = " ")
    write(changed_files$changed, file=Changelog, append=TRUE , ncolumns = 1 , sep = " ")
  
    #code to run if there is a change
    # source the import changes code ------------------------------------------
    #make_stand_endFails_Tables.R
    #importchange.R
    source(path_join(c(project_dir,"5.0 File monitoring/5.3 R/ImportChanges.R")))
    Update_start_end_dates(changed_files = changed_files ,
                           servicelog_paths = servicelog_paths ,
                           deploymentlog_path = deploymentlog_path ,
                           Changelog = Changelog)
    # return to log function --------------------------------------------------
  
  }
  
  #log new files
  if(!length(changed_files$added) == 0 ){
    change_to_log <-  "  Files adde:"
    write(change_to_log, file=Changelog, append=TRUE , ncolumns = 1 , sep = " ")
    write(changed_files$added, file=Changelog, append=TRUE , ncolumns = 1 , sep = " ")
  }
  
  #log deleted files
  if(!length(changed_files$deleted) == 0 ){
    change_to_log <-  "  Files deleted:"
    write(change_to_log, file=Changelog, append=TRUE , ncolumns = 1 , sep = "")
    write(changed_files$deleted, file=Changelog, append=TRUE , ncolumns = 1 , sep = " ")
  }
  
  #log the unchanged files
  change_to_log <-  "  Files unchanged:"
  write(change_to_log, file=Changelog, append=TRUE , ncolumns = 1 , sep = "")
  write(changed_files$unchanged, file=Changelog, append=TRUE , ncolumns = 1 , sep = " ")
    
  #make a new snapshot
  Take_A_snapshot(folderpath , snapshot_path)
}

# main --------------------------------------------------------------------
Existing_snapshots <- dir_ls(path = snapshot_path , glob = "*.snap")

if(length(Existing_snapshots) == 0){ # if there is no snap shot
  Take_A_snapshot(folderpath , snapshot_path) #create one
  
  #append to existing log
  change_to_log <- cbind( "**New snapshot created**" , as.character(Sys.time()))
  write(change_to_log, file=Changelog, append=TRUE , ncolumns = 2 , sep = ",")
  
} else{ #if there is one, then check for changes
  
  change_to_log <- paste0( "\n   **log " , as.character(Sys.time()) , " **")
  write(change_to_log, file=Changelog, append=TRUE , ncolumns = 1 , sep = "")
  
  Log_changes(folderpath =folderpath , snapshot_path=snapshot_path)
  #if there is a change can put more code
  
  write("**Excecution complete**\n", file=Changelog, append=TRUE , ncolumns = 1 , sep = "")
}



  