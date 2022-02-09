#make list of folder and number of image in each folder

library(fs)
library(stringr)
library(plyr)
mypath <- "E:/SBB"
mypathLong <- dir_ls(mypath , recurse = TRUE , type= "file" , glob = "*.jpg|*.JPG")

mypathSplit <- data.frame(str_split(string = mypathLong , pattern = "/" , simplify = TRUE))

df.Images <- ddply(mypathSplit , .(X3 ,X4,X5) , summarise, no.imges = length(X5))


write.csv(x = df.Images , file = "E:/SBB/QC/Table of Imges.csv")
