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
  source('textmining.R')
}

#function for reading the data from the file.
# A manul preparition will be needed to change the original xlsx file to a tab seperated txt file, which may be done with Excel or similar program
# The "short" have the same rows as the original file (33819), but only 6 columns (ID, text, s1.included, s1.excluded, s2.included, s2.excluded)
# use colClasses to accelerate the reading speed. (should be useful if our databse gets even bigger)
# note: The reason we chose to use .txt file instead of direcly use the xlsx is due to the size of the file. Using package xlsx, xlsx2 and XLConnect all result in crash of Java due to the limit on the memory. Also, reading from txt file with is significantly faster than reading from the xlsx file.
readData <- function(){
  print("-- Start to read data and write the processed data into a text file for use in the future")

  alldata1 <- read.delim("data/Jing_painrefs_070514_short.txt", stringsAsFactors=F)
  names(alldata1) <- c("id","type","author","year","title","text","s1.included","s1.excluded","s2.included","s2.excluded")
  data1 <- alldata1[nchar(alldata1$title) > 0,]
  data1$title <- tolower(data1$title)
  alldata2 <- read.delim("data/Jing_NP_references.txt", stringsAsFactors=F,header=F,na.strings="")
  ind <- which(rowSums(is.na(alldata2)) == 13)
  alldata2 <- alldata2[-ind,]
  ind<- which(!is.na(alldata2$V2) & (alldata2$V2 > 2014))
  alldata2 <- alldata2[-ind,]
  ind<- which(!is.na(alldata2$V6) & (alldata2$V6 == "Book" | alldata2$V6 == "Book Section"),6)
  names(alldata2) <- c("author","year","title","journal","volumn","issue","pages","blank", "month","text","refn","type","jatype")
  data2 <- data.frame(title=tolower(alldata2$title[-ind]), jatype=tolower(alldata2$jatype[-ind]), stringsAsFactors=F)

  mergeddata <- merge(data1,data2,all=T)
  #  pie(table(mergeddata$jatype))
  mergeddata <- mergeddata[nchar(mergeddata$text) > 0,]
  mergeddata <- mergeddata[which(mergeddata$jatype == "article" | mergeddata$jatype == "review"| is.na(mergeddata$jatype)),]
  mergeddata <- mergeddata[!is.na(mergeddata$id),]
  mergeddata <- mergeddata[-grep("meeting",mergeddata$title),]
  mergeddata <- mergeddata[-grep("conference",mergeddata$title),]

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set up the variables
ml.model <- "SVM"
cost<- 2
gamma <- 0.002
sparselevel <- 0.99
k <- 1

range.dataset <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5,0.6, 0.7,0.8, 0.9, 1)
range.cost <- c(0.1, 0.5, 1,2,3,4,5)
range.gamma <- c(1e-4,1e-3,0.002, 0.005,0.006,0.007)
range.sl <- c(0.8,0.85,0.9,0.92,0.93,0.95,0.98,0.99)
range.k <- c(1,3,5,7,9)

plottype.list <- list("learning_curve" = range.dataset,
                      "feature_curve" = range.sl,
                      "cost_curve"= range.cost,
                      "gamma_curve" = range.gamma,
                      "k_curve" = range.k)
plottype <-  data.frame(plottype.list[6])
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# call functions
if (exists("prepared") == F) initiation()
if (exists("alldata") == F) alldata <- readData()
if (exists("cleandata") == F) cleandata <- preprocessing(alldata, doublescreening = 1)
if (exists("index") == F) index <- indexgenerator(nrow(cleandata), ratio.test = 0.2)
if (exists("best.para") == F) best.para <- tunemodel(cleandata[index$train, ], ml.model, range.cost = range.cost, range.gamma =  range.gamma, range.sl = range.sl, kfold = 0)

if (exists("text.df") == F)  text.df <- createDF(cleandata, sparselevel = best.para$best.sl[0], ml.model = ml.model, createwc = FALSE)

if (exists(classifier) == F) classifier <- traindata(text.df[index$train,], best.para)
result.test <- train(test.data, classifier, best.para)

#if (exists("result.df") == F)
   result.df <- evaluateprediction(plottype, ml.model, text.df, cleandata, cost = cost, gamma = gamma, sparselevel = sparselevel, k = k)

#plotcurve(plottype, ml.model,text.df,result.df,cost=cost,gamma=gamma,sparselevel=sparselevel)
#saveplot

graphtitle <- paste(names(plottype), ", ",ml.model,"model")
if (names(plottype) != "feature_curve") graphtitle <- paste(graphtitle,"sparse level:",sparselevel,", features num:",ncol(text.df))
if ((ml.model == "SVM") & (names(plottype) != "cost_curve")) graphtitle <- paste(graphtitle,", C: ",cost)
if ((ml.model == "SVM") & (names(plottype) != "gamma_curve")) graphtitle <- paste(graphtitle,", gamma",gamma)
if (ml.model == "kNN" & names(plottype) != "k_curve") graphtitle <-  paste(graphtitle, ", k: ",k,sep="")

plot <- ggplot(data = result.df, aes(x=xvar,y=value,color=variable))+ylim(0.6, 1)+ ggtitle(graphtitle)
# plot <- plot+ geom_point(aes(shape = variable))+geom_line(data=result.df[grep("test",result.df$variable),],size=1,linetype="dashed")+geom_line(data=result.df[grep("train",result.df$variable),],size=1,linetype="solid")
plot <- plot+ geom_point(aes(shape = variable))+geom_line(data=result.df[grep("validate",result.df$variable),],size=1,linetype="dashed")+geom_line(data=result.df[grep("train",result.df$variable),],size=1,linetype="solid")
plot <- plot+scale_color_manual(values=c("#56B4E9","#D55E00", "#009E73","#56B4E9", "#D55E00",   "#009E73"))
plot <- plot+xlab(names(plottype))
plot

filename <- paste("img/test_",names(plottype),"_",ml.model,sep="")
if (names(plottype) != "feature_curve") filename <- paste(filename,"_sl",sparselevel,sep="")
if (ml.model == "SVM" & names(plottype) != "cost_curve") filename <-  paste(filename, "_c",cost,sep="")
if (ml.model == "SVM" & names(plottype) != "gamma_curve") filename <-  paste(filename, "_gamma",gamma,sep="")
if (ml.model == "kNN" & names(plottype) != "k_curve") filename <-  paste(filename, "_k",k,sep="")
filename <- gsub("[.]","",filename)
filename <-  paste(filename, ".png",sep="")
ggsave(file=filename)
