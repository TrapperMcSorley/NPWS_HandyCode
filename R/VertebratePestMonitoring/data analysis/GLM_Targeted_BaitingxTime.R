library(tidyverse)
library(boot)
library(mgcv)
library(fs)
setwd("c:/Users/mcsorleya/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/")

# load files --------------------------------------------------------------
#load the manula baiting dates file because the auto gernated version needs some work
Baiting_dates <- read.csv(file = "3.2 Formated logs/Baiting_dates_manual.csv")
#import the start and end dates for each site
Subsites <- read.csv("3.2 Formated logs/start_end_dates.csv" , stringsAsFactors = FALSE)

#this is the first date of camera sampling. All Nights and Days are relative to this date
startnight <- as.POSIXct("2016-08-16")


#list all the files. Fles are saved fro another script with the suffix AnalysisPreped
files <- dir_ls("3.3 Data for analysis" , type = "any" , glob = "*AnalysisPreped*")

#read in the files into a list. one list for each species. 
list_df.trunc <- map(files, read.csv, row.names =1 , stringsAsFactors=TRUE)

#name the list from the files names
names(list_df.trunc) <- map_chr(strsplit(files, split = "_" ) ,.f = function(x) sub(".csv" ,"" ,x[2]))

#create a vector of the targeted subsite names
Sitenames <- list_df.trunc[[1]] %>% filter(SamplingMethod == "Targeted") %>% distinct(Subsite)

#Summarise cameras start and end dates to subsites
FirstLast <- Subsites %>%
  group_by(Subsite) %>%
  summarise(
    First = min(as.POSIXct(start, tz = "Australia/Brisbane")) ,
            Last = max(as.POSIXct(finish, tz = "Australia/Brisbane"))
    ) %>%
  filter (Subsite %in% Sitenames$Subsite)

#truncate data to targeted sites and summarise to detection per night across whole site
#summarise detection to targeted subsites
makeTargetedDailyDetection <- function(x){
  
  x %>% filter(SamplingMethod == "Targeted") %>% 
    group_by(Subsite , day) %>% 
    mutate(Date = as.POSIXct(Date , tz = "Australia/Brisbane")) %>% 
    summarise(
    no_det = sum(animal , na.rm =TRUE),
    detect = (if (no_det > 0) 1 else 0),
    no_opperating = sum(!is.na(animal)), 
    Baiting = unique(Baiting),
    #Fire = unique(Fire),
    Date = min(Date , na.rm = T) , .groups = "keep"
  )
}

#run the function above
list_df.targeted.sum <- map(list_df.trunc , .f = makeTargetedDailyDetection)
#check the names
names(list_df.targeted.sum)

####GLM - targeted sites - day x baiting####------------------------------------

#function for creating 3 GLM for each subsite.
#Where there are no baiting dates for a subsite it will only creat the Day model
#Result_1 is the full Baiting x Day model
#Result_2 is day only
#Result_3 is baiting only
#there is no null model at the moment
GLMperSite <- function(x,y){
  cat(as.character(unique(y$Subsite)),"\n")
  if ("after" %in% unique(x$Baiting)){
    cat("  Baiting x day - ")
    result_1 <- tryCatch(glm(detect ~ Baiting * day , data= x , family = "binomial"),
                         warning = function(c) cat(message(c) ,"\n"))
    
    cat("done", "\n")
    cat("  Baiting - ")
    result_3 <- tryCatch(glm(detect ~ Baiting , data= x , family = "binomial"),
                         warning = function(c) cat(message(c) ,"\n"))
    cat("done", "\n")
  } else{
    
    result_1 <- NULL
    cat("  Baiting x day - NULL")
    result_3 <- NULL
    cat("  Baiting - NULL")
  }
   cat("  day - ")
   result_2 <- tryCatch(glm(detect ~ day , data= x , family = "binomial"),
                       warning = function(c) cat(message(c) ,"\n"))
   cat("done", "\n")
  return(list(Baiting.Day = result_1, Day = result_2, Baiting = result_3))
}
#______________________________________________________________________________________________________

#Put each subsite thorugh the function to create the GLM objects for each subsite and GLM type.
#only use days with more than 6 operating cameras
#the out put will be a nested:
'List of species
    List of subsites
        List of GLM models'

glm.targeted.bait <- list()
for (i in 1:length(list_df.targeted.sum)){
  cat("\n***" , casefold(names(list_df.targeted.sum)[i] , upper = T), "***" , "\n")
  flush.console()
  
  glm.targeted.bait[[i]] <- list_df.targeted.sum[[i]] %>%
    filter(no_opperating>6) %>% #only use nights with 7+ active cameras
    group_by(Subsite) %>%
    group_map(.f = GLMperSite)
  
  names(glm.targeted.bait[[i]]) <- unique(list_df.targeted.sum[[i]]$Subsite)
  names(glm.targeted.bait)[i] <- names(list_df.targeted.sum)[i]
  
}

#_______________________________________________________________________________

# access the glm baiting vs day model parameters --------------------------
#model parameters for each GLM are calculated and stored in a single table
glm.target.par <- data.frame(expand.grid(Subsite = names(glm.targeted.bait[[1]]) ,
                                         Animal = names(list_df.targeted.sum)))

#storing AICs for comparison to select "best" model
glm.target.par$glm.status = NA
glm.target.par$baiting.P = NA
glm.target.par$baiting.P.3 = NA
glm.target.par$day.P = NA
glm.target.par$day.P.2 =NA
glm.target.par$baiting.day.P = NA
glm.target.par$AIC.1 = NA
glm.target.par$AIC.2 = NA
glm.target.par$AIC.3 = NA
glm.target.par$minAIC=NA
glm.target.par$model_pick =NA
glm.target.par$AIC_delta.1 = NA
glm.target.par$AIC_delta.2 = NA
glm.target.par$AIC_delta.3 = NA

#dynamicaly calculate the number of subsites and subsites x species for placement of values in the table
no.sites <- length(names(glm.targeted.bait[[1]]))
h <-  length(glm.targeted.bait) * no.sites  
i=0

#iterate through the species, subsite, model lists and extract model parameters
for (j in 1:length(glm.targeted.bait)) {
  for (l in 1:no.sites){
    i <-  l + ((j-1) * no.sites) #data frame offset for species
    
    glm.target.par$glm.status[i] <- paste0(class(glm.targeted.bait[[j]][[l]][[1]]) , collapse = "_")
    print(names(glm.targeted.bait[[j]][l]))
    
  
    #baiting x day model
    if (!glm.target.par$glm.status[i] == "NULL") { #did the model converge
      temp.sum <- summary(glm.targeted.bait[[j]][[l]][[1]])$coefficients
      glm.target.par$AIC.1[i]         = glm.targeted.bait[[j]][[l]][[1]]$aic
      glm.target.par$baiting.P[i]     = temp.sum[2,4]
      glm.target.par$day.P[i]         = temp.sum[3,4]
      glm.target.par$baiting.day.P[i] = temp.sum[4,4]
    }
    
    #day model coefficients
    status.2 <- paste0(class(glm.targeted.bait[[j]][[l]][[2]]) , collapse = "_")
    
    if(!status.2 == "NULL") {
      temp.sum <- summary(glm.targeted.bait[[j]][[l]][[2]])$coefficients
      glm.target.par$AIC.2[i] = glm.targeted.bait[[j]][[l]][[2]]$aic
      glm.target.par$day.P.2[i] = temp.sum[2,4]
    }
    
    #baiting model coefficients
    status.3 <- paste0(class(glm.targeted.bait[[j]][[l]][[3]]) , collapse = "_")
    
    if(!status.3 == "NULL") {
      temp.sum <- summary(glm.targeted.bait[[j]][[l]][[3]])$coefficients
      glm.target.par$AIC.3[i] = glm.targeted.bait[[j]][[l]][[3]]$aic
      glm.target.par$baiting.P.3[i] = temp.sum[2,4]
    }
  }
}

rm(temp.sum)
rm(status.2 , status.3)

#find model with min AIC
glm.target.par <- glm.target.par %>%
    mutate(minAIC = if_else(condition = is.na(AIC.1) & is.na(AIC.2)  & is.na(AIC.3),
                          true = NA_real_ ,
                          false = pmin(AIC.1 , AIC.2 , AIC.3 , na.rm = T)))

#add the model with the lowest AIC to model pick column  
  for (i in 1:length(glm.target.par[,1])){
    #if (is.na(glm.target.par$minAIC[i])) next
    glm.target.par$model_pick[i] <- match(glm.target.par$minAIC[i] , glm.target.par[i,c("AIC.1","AIC.2","AIC.3")])
  }

#find aic deltas  
glm.target.par <- glm.target.par %>%
  mutate(AIC_delta.1 = AIC.1 - minAIC,
         AIC_delta.2 = AIC.2 - minAIC,
         AIC_delta.3 = AIC.3 - minAIC)

write.csv(glm.target.par , "3.4 AnalysisResults/bulkGLM_results_ModelParameteres_targeted.csv")

####Access GLM- target sites baiting x day######

#Predict value for each model species subsite combination
#only models with AIC <=3 within the best model are predicted
#prediction are stored in model_prediction 
#prediction are dataframes within a list of each Species.

#list of subsites matching order of models
Subsitenames <- names(glm.targeted.bait[[1]])

#create a varible for the predicitons
model_prediction <- vector(mode = "list" , length = length(glm.targeted.bait))
names(model_prediction) <- names(glm.targeted.bait)

#one list for each species
for (j in 1:length(glm.targeted.bait)) {
  #one sub list for each model
  for (i in 1:3){
    model_prediction[[j]][[i]] <- data.frame(Subsite      = character(),
                                             day          = integer(),
                                             no_det       = integer(), 
                                             detect       = integer(), 
                                             no_opperating= integer(), 
                                             Baiting      = character(), 
                                             Date         = character(), 
                                             model        = integer(), 
                                             rate.log     = numeric(),
                                             SE           = numeric(), 
                                             rate         = numeric(), 
                                             CI90up       = numeric(), 
                                             CI90low      = numeric(),
                                             significant  = character())
    }
}

#iterate through each species, subsite and model

#iterate for species
for (j in 1:length(glm.targeted.bait)) { 
  #print some console progress
  flush.console()
  cat("***",casefold(names(glm.targeted.bait)[j] , upper = T),"***\n" )
  ANIMAL <- names(glm.targeted.bait)[j]
  
  #iterate for each subsite
  for (l in 1:no.sites){ 
    cat(names(glm.targeted.bait[[j]])[l] ,"\n")
    flush.console()
    SUBSITE <- Subsitenames[l]
    
    #which model is the best
    model_pick <- glm.target.par %>% filter(Animal == ANIMAL , Subsite == SUBSITE) %>% select(model_pick)
    
    #the delts for all modesls
    AICs <- glm.target.par %>% filter(Animal == ANIMAL , Subsite == SUBSITE) %>% select(AIC_delta.1, AIC_delta.2, AIC_delta.3)
    
    #make sure there are models if not skip
    if (is.na(model_pick$model_pick)) {#all models failed
      cat ("no model\n")
      next
      }
    
    #place some bits from the data into variables for east reference in the model iteration
    models <- glm.targeted.bait[[j]][[l]]
    temp.data <- vector("list" , length(models))
    NewData <- list_df.targeted.sum[[j]] %>% filter(Subsite == SUBSITE)
    
    #iterate for each model
    for (i in 1:length(models)){
      #check if the model exists if not skip to the next one
      if (is.na(AICs[i])) next
      
      
      #the model to predict
      model <- models[[i]] 
      
      #the data for prediction
      temp.data[[i]]          <- list_df.targeted.sum[[j]] %>% filter(Subsite == SUBSITE)
      
      #check is the model is with 3 AIC of the best model. If not mark as not sig
      if (AICs[i] >3 ) temp.data[[i]]$significant <- "no" else temp.data[[i]]$significant <- "Yes"
      
      
      #create variable for the model number
      temp.data[[i]]$model    <- i
     
      #predict in logit scale
      temp.data[[i]]$rate.log = tryCatch(predict(model , newdata = NewData),
                                warning = function(c) cat(message(c) ,"\n"))     #predict values
      
      #error in logit scale
      temp.data[[i]]$SE       = tryCatch(predict(model , newdata = NewData , se.fit = TRUE)$se.fit,
                                warning = function(c) cat(message(c) ,"\n"))  #predict SE
      
      #convert to respose scale
      temp.data[[i]]$rate    <- inv.logit(temp.data[[i]]$rate.log)
      temp.data[[i]]$CI90up  <- inv.logit(temp.data[[i]]$rate.log + temp.data[[i]]$SE *1.65)
      temp.data[[i]]$CI90low <- inv.logit(temp.data[[i]]$rate.log - temp.data[[i]]$SE *1.65)
      
      #bind data into data frame of all subsites for
      model_prediction[[j]][[i]] <- rbind(as.data.frame(temp.data[[i]]) , model_prediction[[j]][[i]])
    }
  }
}

rm(model)
rm(models)
rm(temp.data)
rm(NewData)
rm(AICs)
rm(SUBSITE)
rm(model_pick)

####GLM plots results targeted_________________________________________________
#plot the results with ggplot
#Create one plot for each subsite with each valid model on the plot
#vertical lines are used to indicate the date of aerial baiting

#format baiting dates for potting
Baiting_plot <- Baiting_dates %>% filter(Subsite %in% Subsitenames)
Baiting_plot$Date <- as.POSIXct(Baiting_plot$Date , tz="AUstralia/Brisbane")

#Convert day varible to a date using start night as the reference point
for (i in 1:length(model_prediction)){
    for(j in seq_along(model_prediction[[i]])) {
  model_prediction[[i]][[j]]$Date <- startnight + (model_prediction[[i]][[j]]$day*60*60*24)
  model_prediction[[i]][[j]]$Subsite <- as.character(model_prediction[[i]][[j]]$Subsite)
  }
}

#truncate predicted values to period of operating cameras for each subsite
#use FirstLast dates for truncation of each subsite
for (i in 1:length(model_prediction)){
  for(j in 1:3){
    model_prediction[[i]][[j]] <- model_prediction[[i]][[j]] %>%
      group_by(Subsite) %>%
      #using a function in group_modify to expose .y$Subsite as variable with the subsite name. Then filter on this variable
      group_modify( ~ {.x %>% filter(Date > FirstLast %>% filter(Subsite %in% .y$Subsite) %>% select(First) & #keep data greater than First date
                                     Date < FirstLast %>% filter(Subsite %in% .y$Subsite) %>% select(Last))} #keep data less than last date
                    )
  }}

#make site means for plotting. 
#The model weren't predicted on the site means but it displays better than the binary night detections
df.means <- vector(mode = "list" , length = length(list_df.targeted.sum))
for (i in 1:length(list_df.trunc)){
  df.means[[i]] <- list_df.trunc[[i]] %>%
    filter(SamplingMethod == "Targeted") %>% 
    group_by(Subsite , day)  %>%
    summarise (mean= mean(animal , na.rm = T))
  
  df.means[[i]]$Date <- startnight + (df.means[[i]]$day*60*60*24)
}

#plot colors using the dark2 palette. Alt palette commented out
library(RColorBrewer)
Pallete = "Dark2"
#c("#a6611a" , "#dfc27d", "#80cdc1")

#iterate through each species and subsite and plot models. 
#creates an individual plot for each subsite and species
#also cretes a faceted plot of all subsites. One facet plot per species
for (i in 1:length(list_df.targeted.sum)){
  
  #Put come data into variables for easy access
  animal <- names(list_df.targeted.sum)[i] 
  plotdata.Targeted <-model_prediction[[i]] %>%
    map_df(rbind) %>% 
    filter(significant == "Yes")
    
  plotdata.Targeted$model <- as.factor(plotdata.Targeted$model)
  
  #limit the plot so y axis isn't huge. Dynamic limiting is commented out
  #upperlimit <- max(plotdata.Targeted[plotdata.Targeted$CI90up < 1,3] , na.rm = T) * 1.1
  upperlimit <- 0.4

  model_labes <- glm.target.par %>% filter(Animal == animal)

  #faceted plots
  tempPlot <- ggplot(plotdata.Targeted , aes(x=Date , y = rate ))+
    geom_line(aes(col = model ) , linetype = 1,  size = 1 , alpha = 0.5)+
    #geom_line(aes(y= CI90up, col = model)  , linetype = 5 )+
    #geom_line(aes(y= CI90low, col = model)  , linetype = 5 )+
    geom_point(data = df.means[[i]] , aes(y=mean) , size = 0.2)+
    scale_y_continuous(limits = c(0,upperlimit) , breaks = c(0,upperlimit))+
    geom_vline(data= Baiting_plot , aes(xintercept = Date) , col = "grey" , linetype = "dotdash"  , size = 1)+
    facet_wrap(~Subsite , ncol =2 , strip.position = "left" , labeller = label_wrap_gen(width = 10 , multi_line = T) )+
    ylab("Detection Rate")+
    xlab("")+
    theme_bw()+
    scale_colour_brewer(palette = Pallete , labels = c("bait x day" , "day" , "baiting"))+
    scale_x_datetime(date_labels = "%b-%y" )+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 18 , face = "bold"),
          legend.title = element_text(size=9 , face ="bold") ,
          legend.text = element_text(size=8),
          plot.title = element_text(hjust = 0 , size = 18 , face = "bold"),
          strip.text = element_text(size = 8))
  
  ggsave(paste0("3.5 AnalysisPlots/targeted/glm_Results_BaitDay_Targeted_" , animal, ".png") , width = 29 , height = 21 , units = "cm")
  
  print("group plot -done")
  
  #iterate through individual plots
  for (j in 1:length(unique(plotdata.Targeted$Subsite))){
    
    #gather data relevant to this site into varible for easy access
    This.subsite <- unique(plotdata.Targeted$Subsite)[j]
    plotdata.subsite <- plotdata.Targeted %>% filter(Subsite == This.subsite)
    means.This.site<- df.means[[i]] %>% filter(Subsite == This.subsite)
    This.model_labes <- model_labes %>% filter(Subsite %in% This.subsite)
    Baiting_plot.This.site <- Baiting_plot %>% filter(Subsite %in% This.subsite)
    
    #finds the names and delta AICs of the significant models
    models_predicted <- unique(plotdata.subsite$model)
    AICs <-  glm.target.par %>% filter(Animal == animal , Subsite %in% This.subsite) %>% select(AIC_delta.1, AIC_delta.2, AIC_delta.3)
    modelLabels <-  data.frame(model = as.factor(1:3) , label = c("bait x day" , "day" , "baiting"))
    modelLabels <- modelLabels %>% mutate(AIC = t(round(AICs , 2)))
    lebels_use <- modelLabels %>% filter(model %in% models_predicted) %>% select(label , AIC)
    
    modelLabels$facet_label <- paste0(modelLabels$label, " (\U0394 " ,modelLabels$AIC,")")
    plotdata.subsite <- plotdata.subsite %>% left_join(y = modelLabels)
    
    #print to colsole
    print(paste0(as.character(This.subsite) , "-" , animal))
    
    Plot.This.site <- ggplot(plotdata.subsite , aes(x=Date , y = rate ))+
      geom_line( linetype = 1 , size = 1 )+
      geom_line(aes(y= CI90up ) , linetype = 5 , alpha = 0.5)+
      geom_line(aes(y= CI90low ) , linetype = 5 , alpha = 0.5)+
      geom_point(data = means.This.site , aes(y=mean) , size = 0.3)+
      scale_y_continuous(limits = c(0,upperlimit))+
      geom_vline(data= Baiting_plot.This.site , aes(xintercept = Date) , col = "grey" , linetype = "dotdash"  , size = 1)+
      ylab("Detection Rate")+
      xlab("")+
      ggtitle(paste(This.subsite , animal , sep = "-"))+
      theme_bw()+
      facet_wrap(~ facet_label)+
      scale_x_datetime(date_labels = "%b-%y" , )+
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 18 , face = "bold"),
            legend.title = element_text(size=9 , face ="bold") ,
            legend.text = element_text(size=12, face ="bold"),
            plot.title = element_text(hjust = 0 , size = 18 , face = "bold"),
            strip.text.x = element_text(size = 8))+
      theme(legend.justification=c(0,1), legend.position="none")

    ggsave(plot =Plot.This.site ,
           filename =  paste0("3.5 AnalysisPlots/targeted/IndividualPlots/" , This.subsite ,"_", animal , "_GLM_BaitingDay.png") ,
           width = 9 , height = 4, units = "in")
  }
  
}

rm(plotdata.Targeted , plotdata.subsite , means.This.site)


#get prediction when baiting is significant.-------------------------------------------

#Nah just do everything
sig_baiting_par <- glm.target.par %>% filter(!is.na(model_pick) )

#predict for before and after baiting
Baiting <- data.frame(Baiting = c("before" , "after"))
PREDICTION <- data.frame(
  Subsite        = character(),
  Species        = character(),
  Baiting        = character(),
  fit            = numeric(),
  se.fit         = numeric(), 
  residual.scale = integer()
)
for (i in seq_along(sig_baiting_par[,1])){
  Species  <- sig_baiting_par[i,2]
  Subsite <- sig_baiting_par[i,1]
  MODEL   <- glm.targeted.bait[[Species]][[Subsite]][[3]]
  if (is.null(MODEL)) next
  PREDICTION <- rbind(PREDICTION , cbind( Subsite , Species , Baiting , predict(MODEL , Baiting , type = "response" , se.fit = T)))
}

library(scales)
#Bar plots of results
plot_species <- unique(PREDICTION$Species) #one plot for each subsite
PREDICTION$Baiting <- factor(PREDICTION$Baiting , levels= c("before" , "after"))

for (i in seq_along(plot_species)){
  for( j in seq_along(PREDICTION$Subsite)){
      This_site <- PREDICTION$Subsite[j]
      plot_data <- PREDICTION %>% filter(Species %in% plot_species[i] , Subsite %in% This_site)
      
      ggplot(plot_data , aes(x= Baiting, y = fit))+
        geom_bar(stat="identity" , position = position_dodge2())+
        geom_errorbar(aes(ymax= fit + se.fit , ymin = fit - se.fit) ,width = 0.2,  position = position_dodge2())+
        #facet_wrap(~ Subsite)+
        scale_fill_brewer(palette = "Dark2")+
        xlab("")+
        ggtitle(paste0(This_site , "-" ,toupper(plot_species[i])))+
        ylab(paste0("predicated rate (\U00B1 se)\n"))+
        scale_y_continuous(labels = number_format(accuracy = 0.01))+
        theme(axis.text = element_text(size = 9),
              axis.title = element_text(size = 9 , face = "bold"),
              legend.title = element_text(size=9 , face ="bold") ,
              legend.text = element_text(size=12, face ="bold"),
              plot.title = element_text(hjust = 0 , size = 8 , face = "bold"),
              strip.text.x = element_text(size = 8))+
      
    ggsave(paste0("3.5 AnalysisPlots/BarPlots/" , This_site ,"_", toupper(plot_species[i]) , "_barplot.png") , width = 6 , height = 8 , units = "cm")
  }
}
