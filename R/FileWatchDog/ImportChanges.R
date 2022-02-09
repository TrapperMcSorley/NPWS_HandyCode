library(stringr)
library(readxl)

#sanatise stationIDs 
MakeStationID <- function(station){
  station <- gsub(" ", "" , station)
  ID <- data.frame(ID = toupper(station))
  ID$num <- NA
  ID$Subs <- NA
  ID$num[substr(ID$ID , 1,3) == "P78"] <- str_pad(gsub("P78", "" , ID$ID[substr(ID$ID , 1,3) == "P78"]),3,pad = "0")
  ID$num[!substr(ID$ID , 1,3) == "P78"] <- str_pad(gsub("[[:alpha:]]", "" , ID$ID[!substr(ID$ID , 1,3) == "P78"]),3,pad = "0")
  ID$Subs[substr(ID$ID , 1,3) == "P78"] <- "P78"
  ID$Subs[!substr(ID$ID , 1,3) == "P78"] <- gsub("[[:digit:]]", "" , ID$ID[!substr(ID$ID , 1,3) == "P78"])
  ID$station <- paste0(ID$Subs , ID$num)
  ID$station[station == ""] <- NA
  return(ID$station)
}

RemoveLastLetter <- function (mystrings){
  mystrings <- as.character(mystrings)
  lastchar <- substr(x = mystrings , start = nchar(mystrings) , stop = nchar(mystrings))
  temp.sp <- mystrings[is.na(as.numeric(lastchar))]
  temp.sp <- substr(x = temp.sp , start = 1, stop = nchar(temp.sp)-1)
  mystrings[is.na(as.numeric(lastchar))] <- temp.sp
  return(mystrings)
}

Update_start_end_dates <- function(changed_files , servicelog_paths , deploymentlog_path , Changelog){
  
  #load the service logs with changes
  txt_files_df <- lapply(changed_files$changed, function(x) {read_excel(x ,  col_types = "text")})
  servicelog <- bind_rows(txt_files_df)
  
# error check the service log dates ---------------------------------------
   
      #converte excel date value to posix date
      servicelog$posix <- format(as.POSIXct(as.numeric(servicelog$Date) * (60*60*24)
                                     , origin="1899-12-30"
                                     , tz="Australia/Brisbane"), "%Y-%m-%d")
      date_error <- servicelog$deploymentLocationID[is.na(servicelog$posix)]
      if(!length(date_error) == 0){
        cat("\nthese stations have date errors:\n" , paste0(date_error , collapse = "\n "))
        change_to_log <- cbind( "these stations have date errors:\n" , paste0(date_error , collapse = "\n "))
        write(change_to_log, file=Changelog, append=TRUE , ncolumns = 2 , sep = " ")
      }
      
      #check the faildates are sensible
        #start before end
        servicelog$S.NotFunctioning <- as.POSIXct(as.numeric(servicelog$S.NotFunctioning) * (60*60*24)
                                                         , origin="1899-12-30"
                                                         , tz="Australia/Brisbane")
        servicelog$E.NotFunctioning <- as.POSIXct(as.numeric(servicelog$E.NotFunctioning) * (60*60*24)
                                                         , origin="1899-12-30"
                                                         , tz="Australia/Brisbane")
        Fails <- servicelog[!is.na(servicelog$S.NotFunctioning),]
        
        #start fail after end fail?
        fail_date_error <- Fails$deploymentLocationID[Fails$E.NotFunctioning <Fails$S.NotFunctioning]
        if(!length(fail_date_error) == 0){
          cat("\nthese stations have start-fail-dates after end-fail-dates:\n" , paste0(fail_date_error , collapse = "\n ") , "\n")
          change_to_log <- cbind("these stations have start-fail-dates after end-fail-dates:\n" , paste0(fail_date_error , collapse = "\n "))
          write(change_to_log, file=Changelog, append=TRUE , ncolumns = 2 , sep = " ")
        }
        
        #are the fail dates within camera sampling period
        #to be done 
        
      #clean the station ID of the service logs
        temp_stationID <- RemoveLastLetter(servicelog$deploymentLocationID)
        servicelog$stationID <- MakeStationID(temp_stationID)
    
    #load the deployment log
        deploymentlog <- read_excel(deploymentlog_path , col_types = "text")
        deploymentlog$posix <- format(as.POSIXct(as.numeric(deploymentlog$Date) * (60*60*24)
                                                 , origin="1899-12-30"
                                                 , tz="Australia/Brisbane"), "%Y-%m-%d")
      #clean the stationID
        deploymentlog <- deploymentlog %>% filter(!is.na(SamplingLocationID))
        temp_stationID <- RemoveLastLetter(deploymentlog$SamplingLocationID)
        deploymentlog$stationID <- MakeStationID(temp_stationID)
    
    #check the stationIDs are present in both service log and deplyment log
        No_deployment <- unique(servicelog$stationID[!servicelog$stationID %in% deploymentlog$stationID])
        if(!length(No_deployment) == 0){
          cat("\nthese camera stations are not recorded in the deployment log (cameraSampling.csv):\n" , paste0(No_deployment , collapse = "\n "), "\n")
          change_to_log <- cbind("these camera stations are not recorded in the deployment log (cameraSampling.csv):\n" , paste0(No_deployment , collapse = "\n "))
          write(change_to_log, file=Changelog, append=TRUE , ncolumns = 2 , sep = " ")
        }
        
    #check the stationIDs have a date in the deployment log
        No_deployment_date <- deploymentlog$stationID[is.na(deploymentlog$posix) & deploymentlog$stationID %in% servicelog$stationID]
        if(!length(No_deployment_date) == 0){
          cat("\nthese camera stations do not have deployment dates (cameraSampling.csv):\n" , paste0(No_deployment_date , collapse = "\n "), "\n")
          change_to_log <- cbind("these camera stations do not have deployment dates (cameraSampling.csv):\n" , paste0(No_deployment_date , collapse = "\n "))
          write(change_to_log, file=Changelog, append=TRUE , ncolumns = 2 , sep = " ")
        }
        
        #remove the stations with errors
        servicelog <- servicelog[!servicelog$stationID %in% date_error,] #date error
        servicelog <- servicelog[!servicelog$stationID %in% No_deployment,] #not in the deployment log
        servicelog <- servicelog[!servicelog$stationID %in% No_deployment_date,] #no date in deployment log
        
        if(length(servicelog[,1]) == 0){
          change_to_log <- cbind("","Nothing Imported due to date errors or deployment log missmatch")
          write(change_to_log, file=Changelog, append=TRUE , ncolumns = 2 , sep = " ")
          next(cat("\nNothing Imported due to date errors or deployment log missmatch"))
          
        }

# fail_dates --------------------------------------------------------------
        
    #create table of fails
        new_fails <- na.omit(servicelog[!is.na(servicelog$S.NotFunctioning) &
                                           !servicelog$stationID %in% fail_date_error,
                                          c("stationID" ,"S.NotFunctioning" ,"E.NotFunctioning")])
        
        #load in existing table
        fail_dates_path <- path_join(c(servicelog_paths , "fail_dates.csv" ))
        old_fail <- read.csv(fail_dates_path , colClasses = "character")
        old_fail$S.NotFunctioning <-  as.POSIXct(old_fail$S.NotFunctioning)
        old_fail$E.NotFunctioning <-  as.POSIXct(old_fail$E.NotFunctioning)
        
      #append to fail dates
        fail_dates <- merge(old_fail , new_fails , all = TRUE)
        fail_dates$S.NotFunctioning <- format(fail_dates$S.NotFunctioning , "%Y-%m-%d")
        fail_dates$E.NotFunctioning <- format(fail_dates$E.NotFunctioning , "%Y-%m-%d")
        fail_dates  <- unique(fail_dates)
      #save fail_dates
        write.csv(x = fail_dates , file = fail_dates_path, row.names = FALSE)

# start end dates ---------------------------------------------------------

    #createtable of start and end dates
  
      #make table of closed dates
      closed_dates <- deploymentlog[!deploymentlog$DateClosed == "" , c("stationID" , "DateClosed")]
      closed_dates$DateClosed <-  format(as.POSIXct(as.numeric(closed_dates$DateClosed) * (60*60*24)
                                                    , origin="1899-12-30"
                                                    , tz="Australia/Brisbane"), "%Y-%m-%d")
      closed_dates <- closed_dates[!is.na(closed_dates$DateClosed),]
      
      #create new start and dates
      finish_dates <- plyr::ddply(servicelog[servicelog$ImageDataExported == "y",] , ('stationID') , plyr::summarise , finish = max(posix, na.rm = T))
      service_dates <- plyr::ddply(servicelog , ('stationID') , plyr::summarise , serviced = max(posix, na.rm = T))
      start_date <- plyr::ddply(deploymentlog , ('stationID') , plyr::summarise , start = min(posix, na.rm = T))
      
      #merge in start and finish dates
      start_finish_dates <- merge(start_date , finish_dates , by = "stationID" , all.y = TRUE)
      #merge in service dates
      start_finish_dates <- merge(start_finish_dates , service_dates , by = "stationID" , all.x = TRUE)
      #merge in close dates
      start_finish_dates <- merge(start_finish_dates , closed_dates , by = "stationID" , all.x = TRUE)
      start_finish_dates <- start_finish_dates[!is.na(start_finish_dates$start),]
      
      #merge in covariates
      start_finish_dates <- merge(start_finish_dates ,
                                  deploymentlog %>% select("Site",
                                                           "Subsite",
                                                           'stationID' ,
                                                           "SamplingMethod" ,
                                                           "NPWS.Branch" ,
                                                           "Latitude" ,
                                                           "Longitude" ,
                                                           "DataManager" )  ,
                                  by.x = 'stationID' ,
                                  by.y = "stationID" , 
                                  all.x = TRUE)
      
      #update existing stationIDs
      start_end_path <- path_join(c(servicelog_paths , "start_end_dates.csv"))
      old_start_end_dates <- read.csv(start_end_path , stringsAsFactors = F , colClasses = "character")
      start_end_keep <- old_start_end_dates[!old_start_end_dates$stationID %in% start_finish_dates$stationID,]
      #match formating
      
      start_finish_dates <- start_finish_dates %>%
                              mutate(across(everything(), as.character))
      #append new stationIDs
      new_start_end_dates <- bind_rows(start_finish_dates , start_end_keep)
      
      #re sumarise to remove duplicated stations
      new_start_end_dates <- new_start_end_dates %>% filter(!is.na(stationID)) %>% 
        group_by(stationID) %>% 
        summarise(start = min(start),
                  finish = max(finish),
                  serviced = max(serviced), 
                  DateClosed = max(DateClosed),
                  Latitude = mean(as.numeric(Latitude)),
                  Longitude = mean(as.numeric(Longitude))
                  ) %>% 
        left_join(distinct(new_start_end_dates %>% select(stationID,
                                                 Site,
                                                 Subsite,
                                                 SamplingMethod,
                                                 NPWS.Branch,
                                                 DataManager))
                  )
      
  
      #savestart_end_dates
      write.csv(new_start_end_dates , start_end_path , row.names = FALSE)
      
      change_to_log <- cbind("camera stations updated:\n" , length(new_start_end_dates$stationID))
      write(change_to_log, file=Changelog, append=TRUE , ncolumns = 2 , sep = " ")
     
  }
