
prepare <- function(){
  setwd("M:/R/pain_svm")
  prepared <- T
}

read_originalData <- function(file){
  classes <- c("integer", "character", "logical","logical","logical","logical")
  alldata<-read.delim("Jing_painrefs_070514_short.txt", stringsAsFactors=F,colClasses=classes)
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
alldata <- read_originalData()
data <- cookdata(alldata)