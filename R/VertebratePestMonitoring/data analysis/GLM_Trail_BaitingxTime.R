###analysis of post fire pest monitoring data. 
#####data loaded and manipulated by Prep_for_analysis.r
#rm(list = ls())
library(dplyr)
library(boot)
library(mgcv)
library(ggplot2)
library(tidyr)
library(fs)
library(purrr)
setwd("c:/Users/mcsorleya/DPIE/MST_NPWS_Vertebrate pest monitoring - General/3.0 Power BI data/")

# load files --------------------------------------------------------------
Baiting_dates <- read.csv(file = "3.2 Formated logs/Baiting_dates_manual.csv")
Subsites <- read.csv("3.2 Formated logs/start_end_dates.csv" , stringsAsFactors = FALSE)

startnight <- as.POSIXct("2016-08-16")

files <- dir_ls("3.3 Data for analysis" , type = "any" , glob = "*AnalysisPreped*")

list_df.trunc <- map(files, read.csv, row.names =1 , stringsAsFactors=TRUE)

names(list_df.trunc) <- map_chr(strsplit(files, split = "_" ) ,.f = function(x) sub(".csv" ,"" ,x[2]))

for (i in seq_along(list_df.trunc)){
  list_df.trunc[[i]] <- list_df.trunc[[i]] %>% filter(SamplingMethod == "Trail")
  list_df.trunc[[i]]$Subsite <- factor(list_df.trunc[[i]]$Subsite)
}

Sitenames <-  levels(list_df.trunc[[1]]$Subsite)

FirstLast <- Subsites %>%
  group_by(Subsite) %>%
  summarise(First = min(as.POSIXct(start)) , Last = max(as.POSIXct(finish))) %>%
  filter (Subsite %in% Sitenames)



# GLM Baiting x Time ------------------------------------------------------
#function
GLMperSite <- function(x,y){
  x=x %>% filter(!is.na(animal))
  cat(as.character(unique(y$Subsite)), "\n")
  if (length(unique(x$Baiting)) == 2){
    cat("    Baiting x day - " , unique(x$Baiting) , "\n")
    
    #Baiting * day model
    result_1 <- tryCatch(glm(animal ~ Baiting * day , data=x , family = "binomial",na.action="na.fail"),
                         warning = function(c) cat(message(c) ,"\n"))
    
    #baiting only model
    result_2 <- tryCatch(glm(animal ~ Baiting , data= x , family = "binomial"),
                         warning = function(c) cat(message(c) ,"\n"))
  } else {
    #makes these models NA if there is no baiting at the site
    result_1 <- NULL
    result_2 <- NULL
  }
    #null model
  result_3 <- tryCatch(glm(animal ~ day , data= x , family = "binomial"),
                       warning = function(c) cat(message(c) ,"\n"))
  
  result_null <- tryCatch(glm(animal ~ 1 , data= x , family = "binomial"),
                           warning = function(c) cat(message(c) ,"\n"))
  
  return(list(Baiting.Day = result_1, Baiting = result_2, Day = result_3, Null =result_null))
}

  # model call
glm.subsites.bait <- list()
for (i in 1:length(list_df.trunc)){
  cat(names(list_df.trunc)[i] , "\n")
  
  glm.subsites.bait[[i]] <- list_df.trunc[[i]]  %>% 
    group_by(Subsite) %>%
    group_map(.f = GLMperSite)
  
  names(glm.subsites.bait[[i]]) <-  Sitenames
  names(glm.subsites.bait)[i] <- names(list_df.trunc)[i]
  
}
#__________________________________________________________________________

# Extract the model parameters --------------------------------------------
#access the glm baiting vs day model parameters for significance levels

##record whcih subsite had a succeful glm
glm.par <- data.frame(expand.grid(Subsite = names(glm.subsites.bait[[1]]) ,
                                  Animal = names(list_df.trunc)))
glm.par$glm.status = NA
glm.par$baiting.P = NA
glm.par$baiting.P.3 = NA
glm.par$day.P = NA
glm.par$day.P.2 =NA
glm.par$baiting.day.P = NA
glm.par$AIC.1 = NA
glm.par$AIC.2 = NA
glm.par$AIC.3 = NA
glm.par$AIC.null =NA
glm.par$minAIC=NA
glm.par$model_pick =NA
glm.par$AIC_delta.1 = NA
glm.par$AIC_delta.2 = NA
glm.par$AIC_delta.3 = NA
glm.par$AIC_delta.null =NA

no.sites <- length(Sitenames)
h <-  length(glm.subsites.bait) * no.sites  
i=0
for (j in 1:length(glm.subsites.bait)) {
  for (l in 1:no.sites){
    i <-  l + ((j-1) * no.sites) #data frame offset for species
    #dataframe offset for before after
    glm.par$glm.status[i] <- paste0(class(glm.subsites.bait[[j]][[l]][[1]]) , collapse = "_")
    print(names(glm.subsites.bait[[j]][l]))
    
    if (!glm.par$glm.status[i] == "NULL") { #did the model converge
      temp.sum <- summary(glm.subsites.bait[[j]][[l]][[1]])$coefficients
      glm.par$AIC.1[i]           = glm.subsites.bait[[j]][[l]][[1]]$aic
      glm.par$baiting.P[i]     = temp.sum[2,4]
      glm.par$day.P[i]         = temp.sum[3,4]
      glm.par$baiting.day.P[i] = temp.sum[4,4]
    }
    
    #null model coefficients
    #day model coefficients
    status.2 <- paste0(class(glm.subsites.bait[[j]][[l]][[2]]) , collapse = "_")
    
    if(!status.2 == "NULL") {
      temp.sum <- summary(glm.subsites.bait[[j]][[l]][[2]])$coefficients
      glm.par$AIC.2[i] = glm.subsites.bait[[j]][[l]][[2]]$aic
      glm.par$day.P.2[i] = temp.sum[2,4]
    }
    
    #baiting model coefficients
    status.3 <- paste0(class(glm.subsites.bait[[j]][[l]][[3]]) , collapse = "_")
    
    if(!status.3 == "NULL") {
      temp.sum <- summary(glm.subsites.bait[[j]][[l]][[3]])$coefficients
      glm.par$AIC.3[i] = glm.subsites.bait[[j]][[l]][[3]]$aic
      glm.par$baiting.P.3[i] = temp.sum[2,4]
      
      status.4 <- paste0(class(glm.subsites.bait[[j]][[l]][[4]]) , collapse = "_")
      
      if(!status.3 == "NULL") {
        temp.sum <- summary(glm.subsites.bait[[j]][[l]][[4]])$coefficients
        glm.par$AIC.null[i] = glm.subsites.bait[[j]][[l]][[4]]$aic
      }
    }
  }
}

rm(temp.sum)
rm(status.2 , status.3 , status.4)

#find model with min AIC
glm.par <- glm.par %>%
  mutate(minAIC = if_else(condition = is.na(AIC.1) & is.na(AIC.2)  & is.na(AIC.3) & is.na(AIC.null),
                          true = NA_real_ ,
                          false = pmin(AIC.1 , AIC.2 , AIC.3 , AIC.null , na.rm = T))) 

for (i in 1:length(glm.par[,1])){
  #if (is.na(glm.par$minAIC[i])) next
  glm.par$model_pick[i] <- match(glm.par$minAIC[i] , glm.par[i,c("AIC.1","AIC.2","AIC.3","AIC.null")])
}

#find aic deltas  
glm.par <- glm.par %>%
  mutate(AIC_delta.1 = AIC.1 - minAIC,
         AIC_delta.2 = AIC.2 - minAIC,
         AIC_delta.3 = AIC.3 - minAIC,
         AIC_delta.null = AIC.null - minAIC)

write.csv(glm.par , "3.4 AnalysisResults/bulkGLM_results_ModelParameteres_Trail.csv")
#____________________________________________________________________________


# Predictions from the model ----------------------------------------------
  #predict on the main model 
  #if the main model because there wsnt both before and after baiting
  #then the null model will be estimated so it can be plotted

#create a varible for the predicitons
model_prediction <- vector(mode = "list" , length = length(glm.subsites.bait))
names(model_prediction) <- names(glm.subsites.bait)
'--------------------------------------------------'
#one list for each species
for (j in 1:length(glm.subsites.bait)) {
  #one sub list for each model
  for (i in 1:4){
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

for (j in 1:length(glm.subsites.bait)) { 
  #print some console progress
  flush.console()
  cat("***",casefold(names(glm.subsites.bait)[j] , upper = T),"***\n" )
  ANIMAL <- names(glm.subsites.bait)[j]
  
  #iterate for each subsite
  for (l in 1:no.sites){ 
    cat(names(glm.subsites.bait[[j]])[l] ,"\n")
    flush.console()
    SUBSITE <- Subsitenames[l]
    
    #which model is the best
    model_pick <- glm.par %>% filter(Animal == ANIMAL , Subsite == SUBSITE) %>% select(model_pick)
    
    #the delts for all modesls
    AICs <- glm.par %>%
      filter(Animal == ANIMAL , Subsite == SUBSITE) %>%
      select(AIC_delta.1, AIC_delta.2, AIC_delta.3 , AIC_delta.null)
    
    #make sure there are models if not skip
    if (is.na(model_pick$model_pick)) {#all models failed
      cat ("no model\n")
      next
    }
    
    #place some bits from the data into variables for east reference in the model iteration
    models <- glm.subsites.bait[[j]][[l]]
    temp.data <- vector("list" , length(models))
    NewData <- list_df.trunc[[j]] %>% filter(Subsite == SUBSITE)
    
    #iterate for each model
    for (i in 1:length(models)){
      #check if the model exists if not skip to the next one
      if (is.na(AICs[i])) next
      
      #the model to predict
      model <- models[[i]] 
      
      #the data for prediction
      temp.data[[i]]          <- list_df.trunc[[j]] %>% filter(Subsite == SUBSITE)
      
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


'------------------------------------------------------'
# glm.logit <- vector(mode = "list" , length = length(glm.subsites.bait))
# names(glm.logit) <- names(glm.subsites.bait)
# 
# for (j in 1:length(glm.subsites.bait)) {
#   glm.logit[[j]] <- list_df.trunc[[j]]
#   
#   glm.logit[[j]]$rate = NA
#   glm.logit[[j]]$SE = NA
# }
# 
# no.sites <- length(names(glm.subsites.bait[[1]]))
# Subsitenames <- names(glm.subsites.bait[[1]])

# for (j in 1:length(glm.subsites.bait)) {
#   flush.console()
#   cat("***",casefold(names(glm.subsites.bait)[j] , upper = T),"***\n" )
#   ANIMAL <- names(glm.subsites.bait)[j]
#   
#   for (l in 1:no.sites){
#     cat(names(glm.subsites.bait[[j]])[l] ,"\n")
#     flush.console()
#     SUBSITE <- Subsitenames[l]
#     model_pick <- glm.par %>% filter(Animal == ANIMAL , Subsite == SUBSITE) %>% select(model_pick)
#     
#     if (is.na(model_pick$model_pick)) {#all models failed
#       cat ("no model\n")
#       next
#     }
#     
#     model <- glm.subsites.bait[[j]][[l]][[model_pick$model_pick]]
#     
#     temp.data <- glm.logit[[j]][glm.logit[[j]]$Subsite == SUBSITE,]
#     temp.data$rate = tryCatch(predict(model , newdata = temp.data),
#                               warning = function(c) cat(message(c) ,"\n"))     #predict values
#     
#     temp.data$SE = tryCatch(predict(model , newdata = temp.data , se.fit = TRUE)$se.fit,
#                             warning = function(c) cat(message(c) ,"\n"))  #predict SE
#     
#     glm.logit[[j]][glm.logit[[j]]$Subsite == SUBSITE,] <- temp.data
#   }
# }
# 
# 
# 
# ##results on the response scale
# glm.results <- vector(mode = "list" , length = length(glm.subsites.bait))
# 
# for (j in 1:length(glm.subsites.bait)) {
#   glm.results[[j]] <- glm.logit[[j]]
#   glm.results[[j]]$rate <- inv.logit(glm.logit[[j]]$rate)
#   glm.results[[j]]$CI90up <- inv.logit(glm.logit[[j]]$rate + glm.logit[[j]]$SE *1.65)
#   glm.results[[j]]$CI90low <- inv.logit(glm.logit[[j]]$rate - glm.logit[[j]]$SE *1.65)
#   
#   #write.csv(glm.results[[j]] , paste0("AnalysisResults/bulkGLM_BaitDay_results_", names(glm.subsites.bait)[j] , ".csv"))
# }
#_______________________________________________________________________________


# Plot the results --------------------------------------------------------
  #individual plots and grouped by species
Baiting_plot <- Baiting_dates %>% filter(Subsite %in% Sitenames)
Baiting_plot$Date <- as.POSIXct(Baiting_plot$Date, tz="AUstralia/Brisbane")
'-------------------------------------------------------------------------------------'
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

##plot daily means per subsite and just trail sites - 
df.2020.means <- vector(mode = "list" , length = length(glm.subsites.bait))
for (i in 1:length(list_df.trunc)){
  df.2020.means[[i]] <- list_df.trunc[[i]] %>%
    filter(Subsite %in% Sitenames) %>% 
    group_by(Subsite , day)  %>%
    summarise (mean= mean(animal , na.rm = T))
  
  df.2020.means[[i]]$Date <- startnight + (df.2020.means[[i]]$day*60*60*24)
  
}

library(RColorBrewer)
Pallete = "Dark2"

#iterate through each species and subsite and plot models. 
#creates an individual plot for each subsite and species
#also cretes a faceted plot of all subsites. One facet plot per species
for (i in 1:length(list_df.trunc)){
  
  #Put come data into variables for easy access
  animal <- names(list_df.trunc)[i] 
  plotdata <-model_prediction[[i]] %>%
    map_df(rbind) %>% 
    filter(significant == "Yes")
  plotdata$model <- as.factor(plotdata$model)
  
  #limit the plot so y axis isn't huge. Dynamic limiting is commented out
  #upperlimit <- max(plotdata.Targeted[plotdata.Targeted$CI90up < 1,3] , na.rm = T) * 1.1
  upperlimit <- 0.4
  
  model_labes <- glm.par %>% filter(Animal == animal)
  
  #faceted plots
  tempPlot <- ggplot(plotdata , aes(x=Date , y = rate ))+
    geom_line(aes(col = model ) , linetype = 1,  size = 1 , alpha = 0.5)+
    #geom_line(aes(y= CI90up, col = model)  , linetype = 5 )+
    #geom_line(aes(y= CI90low, col = model)  , linetype = 5 )+
    geom_point(data = df.2020.means[[i]] , aes(y=mean) , size = 0.2)+
    scale_y_continuous(limits = c(0,upperlimit) , breaks = c(0,upperlimit))+
    geom_vline(data= Baiting_plot , aes(xintercept = Date) , col = "grey" , linetype = "dotdash"  , size = 1)+
    facet_wrap(~Subsite , ncol =2 , strip.position = "left" , labeller = label_wrap_gen(width = 10 , multi_line = T) )+
    ylab("Detection Rate")+
    xlab("")+
    theme_bw()+
    scale_colour_brewer(palette = Pallete , labels = c("bait x day" , "baiting" , "day"  , "null"))+
    scale_x_datetime(date_labels = "%b-%y" )+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 18 , face = "bold"),
          legend.title = element_text(size=9 , face ="bold") ,
          legend.text = element_text(size=8),
          plot.title = element_text(hjust = 0 , size = 18 , face = "bold"),
          strip.text = element_text(size = 8))
  
  ggsave(paste0("3.5 AnalysisPlots/Trail/glm_Results_BaitDay_Trail_" , animal, ".png") , width = 29 , height = 21 , units = "cm")
  
  print("group plot -done")
  
  #iterate through individual plots
  for (j in 1:length(unique(plotdata$Subsite))){
    
    #gather data relevant to this site into varible for easy access
    This.subsite <- unique(plotdata$Subsite)[j]
    plotdata.subsite <- plotdata %>% filter(Subsite == This.subsite)
    means.This.site<- df.2020.means[[i]] %>% filter(Subsite == This.subsite)
    This.model_labes <- model_labes %>% filter(Subsite %in% This.subsite)
    Baiting_plot.This.site <- Baiting_plot %>% filter(Subsite %in% This.subsite)
    
    #finds the names and delta AICs of the significant models
    models_predicted <- unique(plotdata.subsite$model)
    AICs <-  glm.par %>% filter(Animal == animal , Subsite %in% This.subsite) %>% select(AIC_delta.1, AIC_delta.2, AIC_delta.3 , AIC_delta.null)
    modelLabels <-  data.frame(model = as.factor(1:4) , label = c("bait x day" , "baiting" , "day"  , "null"))
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
           filename =  paste0("3.5 AnalysisPlots/Trail/IndividualPlots/" , This.subsite ,"_", animal , "_GLM_BaitingDay.png") ,
           width = 9 , height = 4, units = "in")
  }
  
}

rm(plotdata , plotdata.subsite , means.This.site)

'--------------------------------------------------------------------------------------'

#get prediction when baiting is significant.-------------------------------------------

#Nah just do everything
sig_baiting_par <- glm.par %>% filter(!is.na(model_pick) )

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
  MODEL   <- glm.subsites.bait[[Species]][[Subsite]][[2]]
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
#______________________________________________________________________________________________________________
