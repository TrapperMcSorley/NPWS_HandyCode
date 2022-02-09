# Input load. Please do not change #
`dataset` = read.csv('C:/Users/mcsorleya/REditorWrapper_714a6fb6-2e24-4b70-8450-a4d2fff3b04e/input_df_72943e07-123a-457f-91a9-d30cd826e94b.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
# The following code to create a dataframe and remove duplicated rows is always executed and acts as a preamble for your script: 

# dataset <- data.frame(ActiveCameras, animal.count, Rate, variable, start_end_dates.Subsite)
# dataset <- unique(dataset)

# Paste or type your script code here:
library(ggplot2)
library(scales)

if (length(dataset[,1]) == 150000){
  datacutwarning = "data size > 150K. data truncated"

  }else {
    datacutwarning = ""
    }

#colnames(dataset)[colnames(dataset) == "variable"] <- "night"
dataset$Date <- as.POSIXct(as.POSIXct("2016-08-17 AEST") + dataset$Night * 60 *60 *24 , tx = "Australia/Brisbane")

if(dataset$SamplingMethod == "trail") cams = 30 else cams = 10

dataset$ProportionActive <- dataset$ActiveCameras/cams
dataset$ProportionActive[dataset$ProportionActive > 1] <-  1

plotTitle <- paste0(unique(dataset$Species) ,"-", unique(dataset$SamplingMethod)," ", datacutwarning)
plotsub <- paste0(format(min(dataset$Date) , "%Y-%m-%d") ,  " to ", format(max(dataset$Date) , "%Y-%m-%d"))

ggplot(dataset , aes(x=Date , y=Rate ))+
  geom_bar(aes(y=ProportionActive ) ,stat = "identity" , fill = "lightgrey" , col = "lightgrey" )+
  geom_point(size = 0.5)+
  scale_x_datetime(date_labels = "%b-%Y")+
  scale_y_continuous(labels = number_format(1) , limit = c(0,1.0) ,   breaks = c(0,1) )+
  theme_bw()+
  ggtitle(plotTitle , plotsub)+
  ylab("detections")+
  facet_wrap(~Subsite , ncol = 1 , scale = "free_y" , strip.position = "left" , labeller = label_wrap_gen(width = 11 , multi_line = TRUE))+
  theme(title =element_text(face = "bold") )
  #theme(title =element_text(size = 9 , face = "bold"), axis.text = element_text(size = 7),
        #axis.title = element_text(size = 9 , face = "bold"),
        #legend.title = element_text(size=9 , face ="bold") , legend.text = element_text(size=8),
        #strip.text = element_text(size = 8))

