# Script designed for classifying papers into 'pain' related papers or not, by reading its abstract.
#
# Current Data set is provided by Gillian and created by Gillian and Nickie
# In the future, a download script may be added to directly download paper from the Endnote.
#
# Linear Logistic regression will be used first and if the accurancy is not ideal, SVM with different will be used.
#
# Created by Jing Liao
# 12/05/2014
#

#function for setting up the workpath, load the nesseary libraries
initiation <- function(workpath="M:/Win7/Desktop/GitHub/pain_svm"){
  setwd(workpath)
  library(tm)
  library(wordcloud)
  library(e1071)
  library(class)
  library(caret)
  prepared <- T
  options(stringsAsFactors = TRUE)
}

#function for reading the data from the file.
# A manul preparition will be needed to change the original xlsx file to a tab seperated txt file, which may be done with Excel or similar program
# The "short" have the same rows as the original file (33819), but only 6 columns (ID, abstract, s1.included, s1.excluded, s2.included, s2.excluded)
# use colClasses to accelerate the reading speed. (should be useful if our databse gets even bigger)
# note: The reason we chose to use .txt file instead of direcly use the xlsx is due to the size of the file. Using package xlsx, xlsx2 and XLConnect all result in crash of Java due to the limit on the memory. Also, reading from txt file with is significantly faster than reading from the xlsx file. 
readData <- function(){
  #  classes <- c("integer", "character", "logical","logical","logical","logical")
  #   if (!file.exists("data/processed_NP_ref.txt") ){
  print("-- Start to read data and write the processed data into a text file for use in the future")
  alldata1 <- read.delim("data/Jing_painrefs_070514_short.txt", stringsAsFactors=F) 
  names(alldata1) <- c("id","type","author","year","title","abstract","s1.included","s1.excluded","s2.included","s2.excluded")
  data1 <- alldata1[nchar(alldata1$title) > 0,]
  data1$title <- tolower(data1$title)
  alldata2 <- read.delim("data/Jing_NP_references.txt", stringsAsFactors=F,header=F,na.strings="")
  ind <- which(rowSums(is.na(alldata2)) == 13)
  alldata2 <- alldata2[-ind,]
  ind<- which(!is.na(alldata2$V2) & (alldata2$V2 > 2014))
  alldata2 <- alldata2[-ind,] 
  ind<- which(!is.na(alldata2$V6) & (alldata2$V6 == "Book" | alldata2$V6 == "Book Section"),6)
  names(alldata2) <- c("author","year","title","journal","volumn","issue","pages","blank", "month","abstract","refn","type","jatype")
  data2 <- data.frame(title=tolower(alldata2$title[-ind]), jatype=tolower(alldata2$jatype[-ind]), stringsAsFactors=F)
  
  mergeddata <- merge(data1,data2,all=T)
  #  pie(table(mergeddata$jatype))
  mergeddata <- mergeddata[nchar(mergeddata$abstract) > 0,]
  mergeddata <- mergeddata[which(mergeddata$jatype == "article" | mergeddata$jatype == "review"| is.na(mergeddata$jatype)),]
  mergeddata <- mergeddata[!is.na(mergeddata$id),]
  mergeddata <- mergeddata[-grep("meeting",mergeddata$title),]
  mergeddata <- mergeddata[-grep("conference",mergeddata$title),]
  
  # write.table(mergeddata, file = "data/processed_NP_ref.txt", sep="\t", row.names=F)
  
  #  mergeddata
  #   }
  #   else {
  #     print("-- Start to read processed data")
  #     mergeddata <- read.delim("data/processed_NP_ref.txt", stringsAsFactors=F,na.strings="")
  #   }
}
# function for preparing the data for analysis. 
# 1. Only the papers both authors have the same conclusion are included.
# 2. Add one logical column as a factor column. 
# 3. Radomarize the data
preparedata <- function(alldata,datasetratio1=1){
  print("-- Start to prepare data")
  #select papers both authors included
  posdata <- alldata[alldata$s1.included == T & alldata$s2.included == T,c(2,6)]
  #add additional identifier column
  posdata$flag <- as.factor(T)
  print(paste("number of paper both agreed to be pain related:", dim(posdata)[1]))
  #select papers both authors excluded
  negdata <-alldata[alldata$s1.included == F & alldata$s2.included == F,c(2,6)]
  #add additional identifier column
  negdata$flag <- as.factor(F)
  print(paste("number of paper both agreed to be NOT pain related:", dim(negdata)[1]))
  #combine pos and neg data
  cleandata <- rbind(posdata,negdata)
  #normalize the abstract
  cleandata$abstract  <- normalizeabstract(cleandata$abstract)
  # randomalize the data
  cleandata[sample(round(nrow(cleandata)*datasetratio1)),]
}

# Detailed normalization on text
normalizeabstract <- function(abstract){
  print("---- Start abstract normalization")
  
  #lower-casing
  abstract <- tolower(abstract)
  
  #delete all the \n that was induced due to the conversion from xlsx to txt
  abstract <- gsub("\n", " ", abstract)
  
  #normalize numbers (all number including int, float are replaced with "anynumber", percentage to "percentage"), so they can be considered equally
  abstract <- gsub("[0-9]*[.]?[0-9]+", "anynumber", abstract) 
  abstract <- gsub("[0-9a-z]+@[0-9a-z]+","emailadd",abstract)
  abstract <- gsub("anynumber%","percentage",abstract)
  
  #normalize all non-word characters
  abstract <- gsub("<[^<>]+>"," ",abstract)
  abstract <- gsub("[+][/][-]","plusminus",abstract)
  abstract <- gsub("[()@$#.:&<>/*+=-^]", " ", abstract)
  abstract <- removePunctuation(abstract)
  
  abstract <- removeWords(abstract, c(stopwords("SMART")))
  print("-- Start to stem the text")
  abstract <- stemDocument(abstract)
  
  abstract <- stripWhitespace(abstract)
}

createwordcloud <- function(flag,textcorpus){
  print("Start to create word cloud")
  pain_indices <- which(flag == T)
  non_indices <- which(flag == F)
  wordcloud(textcorpus[pain_indices], min.freq=2000)
  wordcloud(textcorpus[non_indices], min.freq=6000)
}

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1),labels=c("No","Yes"))
}

creatematrix <- function(cleandata,createwc=F,sparselevel=0.8){
  print("-- Start to make corpus from the abstract")
  abstract.corpus <- Corpus(VectorSource(cleandata$abstract))
  if (createwc == T) createwordcloud(cleandata$flag, abstract.corpus)
  
  print("-- Start to build Document Term Matrix")
  abstract.dtm <- DocumentTermMatrix(abstract.corpus)
  abstract.dtm <- removeSparseTerms(abstract.dtm, sparselevel)
  if (ml.model == "Naive Bayes") {
    print("start to convert counts to 0 or 1")  
    abstract.dm <- apply(abstract.dtm, 2, convert_count)
  }
  else abstract.dm <- data.matrix(abstract.dtm)
}
traindata <- function(abstract.dm, flag, ml.model="SVM",cost=1,gamma=1,k=3,sparselevel=0.98){
  print("-- Start to split the data")
  ind.train <- sample(nrow(abstract.dm),round(length(flag)*0.6))
  ind.validate <- sample( (1:nrow(abstract.dm))[-ind.train], round(length(flag)*0.2))
  ind.test <- (1:nrow(abstract.dm))[-c(ind.train,ind.validate)]
  
  if (ml.model == "NB") {
    print(paste("-- Start naive Bayes training and prediction with sparse level:",sparselevel))
    classifier <- naiveBayes(abstract.dm[ind.train,], flag[ind.train])
    print("-- Start to calculate the prediction")
    prediction_train <- predict(classifier, newdata = abstract.dm[ind.train,])
    prediction_validate <- predict(classifier, newdata = abstract.dm[ind.validate,])
  }
  if (ml.model == "kNN") {
    print("-- Start k-Nearest Neighbour training and prediction")
    prediction_train  <- knn(abstract.dm[ind.train,], abstract.dm[ind.train,],flag[ind.train], k = k)
    prediction_validate <- knn(abstract.dm[ind.train,], abstract.dm[ind.validate,],flag[ind.train], k = k)
  }
  if (ml.model == "SVM") {
    print(paste("-- Start Support Vector Machines training and prediction with cost:", cost, ", gamma:",gamma,"sparse level:",sparselevel))
    classifier <- svm(abstract.dm[ind.train,], flag[ind.train],cost=cost,gamma=gamma)
    print("-- Start to calculate the prediction")
    prediction_train <- predict(classifier, newdata = abstract.dm[ind.train,])
    prediction_validate <- predict(classifier, newdata = abstract.dm[ind.validate,])
  }
  if (ml.model == "tune svm"){
    print("-- Start to tune SVM")
    obj <- tune.svm(abstract.dm[ind.train,],flag[ind.train],  
                    validation.x =abstract.dm[ind.validate,],ind.validate =flag[ind.validate],
                    gamma = 2^(-1:1), cost = 2^(1:4))
    summary(obj)
    plot(obj)
  }
  summary_train <- confusionMatrix(table(prediction_train, flag[ind.train]))
  print(summary_train)
  summary_validate <- confusionMatrix(table(prediction_validate, flag[ind.validate]))
  print(summary_validate)
  
  return(list(train=summary_train,validate=summary_validate))
}

evaluateprediction <- function(plottype, ml.model, abstract.dm, cleandata,cost=cost,gamma=gamma,x=x,sparselevel=sparselevel){
  print("Start to train and evaluate the prediction")

  result.df <- data.frame(xvar = numeric(0), variable = character(0), value = numeric(0))

  for(x in plottype[[1]]){
    subind <- seq_along(cleandata$flag)
    if( names(plottype) == "feature_curve") {
        sparselevel <- x
        abstract.dm <- creatematrix(cleandata,sparselevel=sparselevel)
    }
    if( names(plottype) == "learning_curve") {
      subind = sample(round(nrow(abstract.dm)*x))  
      x <- length(subind) 
    }
   
    if( names(plottype) == "cost_curve") cost <- x
    if( names(plottype) == "gamma_curve") gamma <- x
    if( names(plottype) == "k_curve") k <- x

    if( names(plottype) == "cost_gamma") {cost <- x[1]; gamma<-x[1]}
    print(paste(names(plottype),x))  
    summary<-traindata(abstract.dm[subind,],cleandata$flag[subind], ml.model=ml.model,cost=cost,gamma=gamma,k=k,sparselevel=sparselevel)

    result.df <- rbind(result.df, list(x, "train_tpr",summary$train$byClass[1])
                       ,list(x, "validate_tpr",summary$validate$byClass[1])
                       ,list(x, "train_ppv",summary$train$byClass[3])
                       ,list(x, "validate_ppv",summary$validate$byClass[3])
                       ,list(x, "train_f1", 2*summary$train$byClass[1]*summary$train$byClass[3]/(summary$train$byClass[1]+summary$train$byClass[3]))
                       ,list(x, "validate_f1", 2*summary$validate$byClass[1]*summary$validate$byClass[3]/(summary$validate$byClass[1]+summary$validate$byClass[3]))
                    #   ,list(x, "train_Baccuracy",summary$train$byClass[8])
                    #   ,list(x, "validate_Baccuracy",summary$validate$byClass[8])
                    #   ,list(x, "train_specificity",summary$train$byClass[2])
                    #   ,list(x, "validate_specificity",summary$validate$byClass[2])
                       )
  }
  names(result.df) <- c("xvar","variable","value")
  result.df
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set up the variables
ml.model <- "kNN"
datasetratio1 <- 1
createwc <- F

cost<- 0.5
gamma <- 0.003
sparselevel <- 0.95
k <- 1

plottype.list <- list("learning_curve" = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5,0.6, 0.7,0.8, 0.9, 1), 
                      "feature_curve"= c(0.8,0.85,0.9,0.92,0.93,0.95,0.98),
                      "cost_curve"= c(0.1, 0.5, 0.8, 0.9,1,2),
                      "gamma_curve" =  c(0.0005,0.001,0.005,0.01),
                      #             "cost_gamma" = list("cost" = c(0.1, 0.5, 0.8, 0.9,1,2),"gamma" = c(0.0005,0.001,0.005,0.01)),
                      "k_curve" = c(1,3,5))
plottype <- data.frame(plottype.list[2])
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (exists("prepared") == F) initiation()

if (exists("alldata") == F) alldata <- readData()

if (exists("cleandata") == F) cleandata <- preparedata(alldata,datasetratio1=datasetratio1)

if (exists("abstract.dm") == F) abstract.dm <- creatematrix(cleandata,sparselevel=sparselevel)

# if (exists("result.dfmelt") == F) 
result.df <- evaluateprediction(plottype, ml.model,abstract.dm,cleandata,cost=cost,gamma=gamma,x=x,sparselevel=sparselevel)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
graphtitle <- paste(names(plottype), ", ",ml.model,"model")
if (names(plottype) != "feature_curve") graphtitle <- paste(graphtitle,"sparse level:",sparselevel,", features num:",ncol(abstract.dm))
if (ml.model == "SVM") graphtitle<- paste(graphtitle,", C: ",cost,", gamma",gamma)
if (ml.model == "kNN") graphtitle <-  paste(graphtitle, ", k: ",k,sep="")
plot <- ggplot(data = result.df, aes(x=xvar,y=value,color=variable))+ylim(0, 1)+ ggtitle(graphtitle)
plot <- plot+ geom_point(aes(shape = variable))+geom_line(data=result.df[grep("validate",result.df$variable),],size=1,linetype="dashed")+geom_line(data=result.df[grep("train",result.df$variable),],size=1,linetype="solid")
plot <- plot+scale_color_manual(values=c("#56B4E9","#D55E00", "#009E73","#56B4E9", "#D55E00",   "#009E73"))
plot <- plot+xlab(names(plottype))
plot
filename <- paste("img/",names(plottype),"_",ml.model,sep="")
if (names(plottype) != "feature_curve") filename <- paste(filename,"_sl",sparselevel,sep="")
if (ml.model == "SVM" & names(plottype) != "cost_curve") filename <-  paste(filename, "_c",cost,sep="")
if (ml.model == "SVM" & names(plottype) != "gamma_curve") filename <-  paste(filename, "_gamma",gamma,sep="")
if (ml.model == "kNN" & names(plottype) != "k_curve") filename <-  paste(filename, "_k",k,sep="")
filename <- gsub("[.]","",filename)
filename <-  paste(filename, ".png",sep="")
ggsave(file=filename)
