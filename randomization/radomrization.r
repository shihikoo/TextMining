# Script designed for classifying papers into 'pain' related papers or not, by reading its text.
#
# Current Data set is provided by Gillian and created by Gillian and Nickie
# In the future, a download script may be added to directly download paper from the Endnote.
#
#
# Created by Jing Liao
# 12/05/2014
#

#function for setting up the workpath, load the nesseary libraries
initiation <- function(workpath="M:/GitHub/TextMining/"){
  setwd(workpath)
#  library(caret)
  prepared <- T
  options(stringsAsFactors = TRUE)
  source('textmining_v3.R')
}

#function for reading the data from the file.
# A manul preparition will be needed to change the original xlsx file to a tab seperated txt file, which may be done with Excel or similar program
# The "short" have the same rows as the original file (33819), but only 6 columns (ID, text, s1.included, s1.excluded, s2.included, s2.excluded)
# use colClasses to accelerate the reading speed. (should be useful if our databse gets even bigger)
# note: The reason we chose to use .txt file instead of direcly use the xlsx is due to the size of the file. Using package xlsx, xlsx2 and XLConnect all result in crash of Java due to the limit on the memory. Also, reading from txt file with is significantly faster than reading from the xlsx file.
readData <- function(){
  print("-- Start to read data and write the processed data into a text file for use in the future")
  alldata <- read.delim("data/PDF_QualityScore_2col.txt", stringsAsFactors=F)
  names(alldata) <- c("flag","hlink")
  data <- alldata[nchar(alldata$hlink) > 0,]
  ind <- grep('.pdf$', data$hlink, ignore.case = T)
  data <- data[ind,] 
  ind <- grep("MS Clinical trial Study[\\]UpdatedSearch16thAug[\\]ISIEmbase Update References",data$hlink,ignore.case=T)
  data[ind,]$hlink <- gsub('MS Clinical trial Study', 'Publications',data[ind,]$hlink)
  data[ind,]$hlink <- gsub('UpdatedSearch16thAug', 'MS Clinical Trial',data[ind,]$hlink)
  data[ind,]$hlink <- gsub('ISIEmbase Update References', 'Journals',data[ind,]$hlink)
  data[ind,]$hlink <- gsub('\\\\Pre8thSeptember', '', data[ind,]$hlink)  
  data[ind,]$hlink <- gsub('\\\\New Folder', '', data[ind,]$hlink)
  data$hlink <- gsub('\\\\Multiple Sclerosis', '', data$hlink)
  data$hlink <- gsub('\\\\Motor Neuron Disease', '', data$hlink)
  data$hlink <- gsub("\\\\Huntington[']s Disease", '', data$hlink)
  data$hlink <- gsub("\\\\Parkinson[']s Disease", '', data$hlink)
  data$hlink <- gsub('\\\\Journals\\Alzheimers disease', '', data$hlink)

  ind.good <- grep('^S:', data$hlink, ignore.case = T) 
  data[-ind.good,]$hlink <- gsub('^[\\][\\]DCNSKULL[\\]vol1','S:',data[-ind.good,]$hlink,ignore.case=T) 
  
  ind.good <- grep('^S:', data$hlink, ignore.case = T)
  data[-ind.good,]$hlink <- paste('S:\\trialdev\\camarades\\', data[-ind.good,]$hlink,sep='')
  
  nr <- nrow(data)
  data$txtlink <- ''
  data$text <-''
  exe <- 'C:\\xpdfbin-win-3.04\\bin64\\pdftotext.exe'
  
  for (ipub in 1:nr){
    uri <- data$hlink[ipub]
    
    if(file.exists(uri)){  
      system(paste("\"", exe, "\" \"", uri, "\"", sep = ""), wait = F)
      data$txtlink[ipub] <- sub(".pdf$", ".txt", uri)
      
    } #else  print(paste('file not found: ', uri))
  }

  for (ipub in 1:nr){
    filetxt <- data$txtlink[ipub]
    if (filetxt != ''){
      if (file.exists(filetxt)){
      txt <- readLines(filetxt, warn = F)
      data$text[ipub] <- paste(txt, collapse = '')
    }# else print(paste('file not exists: ', filetxt))
  }
  }
  newdata <- data[which(nchar(data$text)>1000),]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set up the variables
ml.model <-'penSVM'# "NB"
datasetratio1 <- 1
createwc <- F

cost<- 12
gamma <- 7e-5
sparselevel <- 0.9
k <- 1

plottype.list <- list("learning_curve" = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5,0.6, 0.7,0.8, 0.9, 1),
                      "feature_curve"= c(0.4,0.5,0.6,0.7,0.8,0.9),
                      "cost_curve"= c(11.5,11.7,12,12.2,12.5),
                      "gamma_curve" =  c(1e-5,5e-5,7e-5,8e-5,1e-4,2e-4,5e-4,1e-3),
                      "cost_gamma" = list("cost" = c(0.1, 0.5, 0.8, 0.9,1,2),"gamma" = c(0.0005,0.001,0.005,0.01)),
                      "k_curve" = c(1,3,5,7,9))
plottype <-  data.frame(plottype.list[1])
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# call functions
if (exists("prepared") == F) initiation()
if (exists("alldata") == F) alldata <- readData()
if (exists("cleandata") == F) cleandata <- preprocessing(alldata)
if (exists("text.df") == F)  text.df <- createDF(cleandata,sparselevel=sparselevel,ml.model=ml.model)
result.df <- traindata(text.df, ml.model=ml.model,cost=cost,gamma=gamma,k=k,sparselevel=sparselevel)

#if (exists("result.df") == F)
#   result.df <- evaluateprediction(plottype, ml.model,text.df,cleandata,cost=cost,gamma=gamma,sparselevel=sparselevel,k=k)

#plotcurve(plottype, ml.model,text.df,result.df,cost=cost,gamma=gamma,sparselevel=sparselevel)
#saveplot

# graphtitle <- paste("Validation: ", names(plottype), ", ",ml.model,"model")
# if (names(plottype) != "feature_curve") graphtitle <- paste(graphtitle,"sparse level:",sparselevel,", features num:",ncol(text.df))
# if ((ml.model == "SVM") & (names(plottype) != "cost_curve")) graphtitle<- paste(graphtitle,", C: ",cost)
# if ((ml.model == "SVM") & (names(plottype) != "gamma_curve")) graphtitle<- paste(graphtitle,", gamma",gamma)
# if (ml.model == "kNN" & names(plottype) != "k_curve") graphtitle <-  paste(graphtitle, ", k: ",k,sep="")
# 
# plot <- ggplot(data = result.df[-grep("test",result.df$variable),], aes(x=xvar,y=value,color=variable))+ylim(0.6, 1)+ ggtitle(graphtitle)
# plot <- plot+ geom_point(aes(shape = variable))+geom_line(data=result.df[grep("validate",result.df$variable),],size=1,linetype="dashed")+geom_line(data=result.df[grep("train",result.df$variable),],size=1,linetype="solid")
# plot <- plot+scale_color_manual(values=c("#56B4E9","#D55E00", "#009E73","#56B4E9", "#D55E00",   "#009E73"))
# plot <- plot+xlab(names(plottype))
# plot
# 
# filename <- paste("randomization/img/",names(plottype),"_",ml.model,sep="")
# if (names(plottype) != "feature_curve") filename <- paste(filename,"_sl",sparselevel,sep="")
# if (ml.model == "SVM" & names(plottype) != "cost_curve") filename <-  paste(filename, "_c",cost,sep="")
# if (ml.model == "SVM" & names(plottype) != "gamma_curve") filename <-  paste(filename, "_gamma",gamma,sep="")
# if (ml.model == "kNN" & names(plottype) != "k_curve") filename <-  paste(filename, "_k",k,sep="")
# filename <- gsub("[.]","",filename)
# ggsave(file=paste(filename, ".png",sep=""))
# 
# write.table(result.df, file = paste(filename, ".txt",sep=""), quote=FALSE, row.names = F, append = FALSE, sep = "\t")
# 
# if (names(plottype) == 'learning_curve'){
# graphtitle <- paste("Test: ", names(plottype), ", ",ml.model,"model")
# if (names(plottype) != "feature_curve") graphtitle <- paste(graphtitle,"sparse level:",sparselevel,", features num:",ncol(text.df))
# if ((ml.model == "SVM") & (names(plottype) != "cost_curve")) graphtitle<- paste(graphtitle,", C: ",cost)
# if ((ml.model == "SVM") & (names(plottype) != "gamma_curve")) graphtitle<- paste(graphtitle,", gamma",gamma)
# if (ml.model == "kNN" & names(plottype) != "k_curve") graphtitle <-  paste(graphtitle, ", k: ",k,sep="")
#  
# plot <- ggplot(data = result.df[-grep("validate",result.df$variable),], aes(x=xvar,y=value,color=variable))+ylim(0.6, 1)+ ggtitle(graphtitle)
# plot <- plot+ geom_point(aes(shape = variable))+geom_line(data=result.df[grep("test",result.df$variable),],size=1,linetype="dashed")+geom_line(data=result.df[grep("train",result.df$variable),],size=1,linetype="solid")
# plot <- plot+scale_color_manual(values=c("#56B4E9","#D55E00", "#009E73","#56B4E9", "#D55E00",   "#009E73"))
# plot <- plot+xlab(names(plottype))
# plot
# 
# filename <- paste("randomization/test_",names(plottype),"_",ml.model,sep="")
# if (names(plottype) != "feature_curve") filename <- paste(filename,"_sl",sparselevel,sep="")
# if (ml.model == "SVM" & names(plottype) != "cost_curve") filename <-  paste(filename, "_c",cost,sep="")
# if (ml.model == "SVM" & names(plottype) != "gamma_curve") filename <-  paste(filename, "_gamma",gamma,sep="")
# if (ml.model == "kNN" & names(plottype) != "k_curve") filename <-  paste(filename, "_k",k,sep="")
# filename <- gsub("[.]","",filename)
# ggsave(file=paste(filename, ".png",sep=""))
# 
# write.table(result.df, file = paste(filename, ".txt",sep=""), quote=FALSE, row.names = F, append = FALSE, sep = "\t")
# }
