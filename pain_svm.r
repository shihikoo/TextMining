# Script for cateragering papers
#
# This scipt is designed for finding "pain" related papers by reading the abstract
# Current Data set is provided by Gillian and created by Gillian and Nickie
# In the future, a download script may be added to directly download paper from the Endnote.
#
# Linear Logistic regression will be used first and if the accurancy is not ideal, SVM with different will be used.
#
# Created by Jing Liao
# 12/05/2014
#

#function for set up the workpath, load the nesseary libraries
prepare <- function(workpath="M:\Win7\Desktop\GitHub\", libraries = ""){
  setwd(workpath)
  if(libraries != "") {
    for i in seq_along(libraries) {
      library(libraries[i])
    }
  }
  prepared <- T
}

#function for read the data from the file.
readData <- function(datafile="data/Jing_painrefs_070514_short.txt"){
# A manul preparition will be needed to change the original xlsx file to a tab seperated txt file, which may be done with Excel or similar program
# The "short" have the same rows as the original file (33819), but only 6 columns (ID, Abstract, s1.included, s1.excluded, s2.included, s2.excluded)
  classes <- c("integer", "character", "logical","logical","logical","logical")
  alldata <- read.delim(datafile, stringsAsFactors=F,colClasses=classes)
}

cookdata <- function(alldata){
  posdata <- alldata[alldata$s1.included == T & alldata$s2.included == T,1:2]
  posdata$included <- T
  print(dim(posdata))
  negdata <-alldata[alldata$s1.included == F & alldata$s2.included == F,1:2]
  negdata$included <- F
  print(dim(negdata))
  data <- rbind(posdata,negdata)
  data2=data[sample(nrow(data)),]
}

prepared <- F
if(prepared != T) prepare()
alldata <- readData()
data <- cookdata(alldata)