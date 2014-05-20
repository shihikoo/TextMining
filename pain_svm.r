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
preparedata <- function(alldata){
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
  cleandata$abstract  <-  normalizeabstract(cleandata$abstract)
  # randomalize the data
  cleandata[sample(nrow(cleandata)),]
  if (usesmalldataset == T){cleandata <- cleandata[1:10000,]}
  cleandata
}

# Detailed normalization on text
normalizeabstract <- function(abstract){
  print("---- Start abstract normalization")
  
  #lower-casing
  abstract <- tolower(abstract)
  
  #delete all the \n that was induced due to the conversion from xlsx to txt
  abstract <- gsub("\n", " ", abstract)
  
  #normalize numbers (all number including int, float are replaced with "anynumber", percentage to "percentage"), so they can be considered equally
  abstract <- gsub("[0-9]*[.]?[0-9]+", " ", abstract) 
  abstract <- gsub("[0-9a-z]+@[0-9a-z]+","emailadd",abstract)
  abstract <- gsub(" %","percentage",abstract)
  
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ml.model <- "Support Vector Machines"
usesmalldataset <- F
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (exists("prepared") == F) initiation()
if (exists("alldata") == F) alldata <- readData()
if (exists("cleandata") == F) cleandata <- preparedata(alldata)
print(dim(cleandata)[1])
if (exists("abstract.dm") == F) {
  print("-- Start to make corpus from the abstract")
  abstract.corpus <- Corpus(VectorSource(cleandata$abstract))
   createwordcloud(cleandata$flag, abstract.corpus)
  
  print("-- Start to build Document Term Matrix")
  abstract.dtm.ori <- DocumentTermMatrix(abstract.corpus)
  abstract.dtm <- removeSparseTerms(abstract.dtm.ori, 0.95)
  if (ml.model == "Naive Bayes") {
    print("start to convert counts to 0 or 1")  
    abstract.dm <- apply(abstract.dtm, 2, convert_count)
  }
  
  print("-- Start to split the data")
  ind.train <- sample(nrow(cleandata),round(length(cleandata$flag)*0.6))
  ind.validation <- sample( (1:nrow(cleandata))[-ind.train], round(length(cleandata$flag)*0.2))
  ind.test <- (1:nrow(cleandata))[-c(ind.train,ind.validation)]
  if (ml.model != "Naive Bayes") abstract.dm <- data.matrix(abstract.dtm)
  abstract.df <- as.data.frame(abstract.dm , stringAsFactors = T)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (ml.model == "Naive Bayes") {
  print("-- Start naive Bayes training")
  classifier <- naiveBayes(abstract.dm[ind.train,], cleandata$flag[ind.train])
  print("-- Start to calculate the prediction")
  prediction <- predict(classifier, newdata = abstract.dm[ind.test,])
}
if (ml.model == "k-Nearest Neighbour") {
  print("-- Start k-Nearest Neighbour training and prediction")
  prediction <- knn(abstract.dm[ind.train,], abstract.dm[ind.test,],cleandata$flag[ind.train], k =3)
}
if (ml.model == "Support Vector Machines") {
  print("-- Start Support Vector Machines training and prediction")
  classifier <- svm(abstract.dm[ind.train,], cleandata$flag[ind.train],kernel = "linear",cost = 100) #,cross=5,cost=1,gamma=2)
  print("-- Start to calculate the prediction")
  prediction <- predict(classifier, newdata = abstract.dm[ind.test,])
}
if (ml.model == "tune svm"){
  print("-- Start to tune SVM")
  
    obj <- tune.svm(abstract.dm[ind.train,],cleandata$flag[ind.train],  
                    validation.x =abstract.dm[ind.validation,],validation.y =cleandata$flag[ind.validation],
                    gamma = 2^(-1:1), cost = 2^(1:3),cross =5)
    summary(obj)
    plot(obj)
}

summary <- confusionMatrix(table(prediction, cleandata$flag[ind.test]))
print(summary)
