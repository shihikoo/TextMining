# Functions for text mining
# The calling for those functions can be seen in pain.r
# In the future, a download script may be added to directly download paper from the Endnote.
#
# Created by Jing Liao
# 12/06/2014


# function for preparing the data for analysis. 
# 1. Only the papers both authors have the same conclusion are included.
# 2. Add one logical column as a factor column. 
# 3. Radomarize the data
preprocessing <- function(alldata,datasetratio1=1,twoauthor = 0){
  if (twoauthor == 1){
  #select papers both authors included
  posdata <- alldata[alldata$s1.included == T & alldata$s2.included == T,c(2,6)]
  #add additional identifier column
  posdata$flag <- T
  print(paste("number of paper both agreed to be pain related:", dim(posdata)[1]))
  #select papers both authors excluded
  negdata <-alldata[alldata$s1.included == F & alldata$s2.included == F,c(2,6)]
  #add additional identifier column
  negdata$flag <- F
  print(paste("number of paper both agreed to be NOT pain related:", dim(negdata)[1]))
  #combine pos and neg data
  cleandata <- rbind(posdata,negdata)
} else cleandata <- alldata
  #normalize the text
  cleandata$text  <- normalizetext(cleandata$text)
  
  cleandata$flag <- factor(cleandata$flag, levels = c(T,F), labels=c('TRUE','FALSE'))
  # randomalize the data
  set.seed(1)
  
  cleandata[sample(round(nrow(cleandata)*datasetratio1)),]
}

# Detailed normalization on text
normalizetext <- function(text){
  library(tm)
  print("---- Start text normalization")
  
  #lower-casing
  text <- tolower(text)
  
  #delete all the \n that was induced due to the conversion from xlsx to txt
  text <- gsub("\n", " ", text)
  
  #normalize numbers (all number including int, float are replaced with "anynumber", percentage to "percentage"), so they can be considered equally
  text <- gsub("[0-9]*[.]?[0-9]+", "anynumber", text) 
  text <- gsub("[0-9a-z]+@[0-9a-z]+","emailadd",text)
  text <- gsub("anynumber%","percentage",text)
  text <- gsub("anynumberanynumber", "anynumber", text) 
  
  #normalize all non-word characters
  text <- gsub("<[^<>]+>"," ",text)
  text <- gsub("[+][/][-]","plusminus",text)
  text <- gsub("[()@$#.:&<>/*+=-^]", " ", text)  
  text <- removePunctuation(text)
  
  text <- removeWords(text, c(stopwords("SMART")))
  print("-- Start to stem the text")
  text <- stemDocument(text)
  text <- stripWhitespace(text)
}

createwordcloud <- function(flag,textcorpus){
  library(wordcloud)
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

createDF<- function(cleandata,ml.model='SVM', createwc=F,sparselevel=0.8){
  library(tm)
  print("-- Start to make corpus from the text")
  text.corpus <- Corpus(VectorSource(cleandata$text))
  if (createwc == T) createwordcloud(cleandata$flag, text.corpus)
  
  print("-- Start to build Document Term Matrix")
  text.dtm <- DocumentTermMatrix(text.corpus, 
                                     control = list(weighting = weightTfIdf, minWordLength = 3))
  text.dtm <- removeSparseTerms(text.dtm, sparselevel)
  if (ml.model == "NB") {
    print("start to convert counts to 0 or 1")
    text.df <- as.data.frame(apply(text.dtm, 2, convert_count))
  } else {
    text.df <- as.data.frame(as.matrix(text.dtm))
  }
  text.df$flag <- cleandata$flag
  return(text.df)
}

traindata <- function(text.df, ml.model="SVM",cost=1,gamma=1,k=3,sparselevel=0.98,kfold=0){
  library(caret)
  print("-- Start to split the data")
  nr <- nrow(text.df)
  nc <- ncol(text.df)
  set.seed(42)
  ind.train <- sample(nr, round(nr*0.6))
  ind.validate <- sample( (1:nr)[-ind.train], round(nr*0.2))
  ind.test <- (1:nr)[-c(ind.train,ind.validate)]
  
  if (ml.model == "NB") {
    library(e1071)
    print(paste("-- Start naive Bayes training and prediction with sparse level:",sparselevel))
    classifier <- naiveBayes(flag ~ ., data = text.df[ind.train,])
    print("-- Start to calculate the prediction")
    prediction_train <- predict(classifier, newdata = text.df[ind.train,])
    prediction_validate <- predict(classifier, newdata = text.df[ind.validate,])
    prediction_test <- predict(classifier, newdata = text.df[ind.test,])
  }
  if (ml.model == "kNN") {
    print("-- Start k-Nearest Neighbour training and prediction")
  #  classifier <- knn3(flag ~ ., text.df[ind.train,], k = k) 
     library(class)
     prediction_train  <- knn(text.df[ind.train,-nc], text.df[ind.train,-nc],text.df$flag[ind.train], k = k)
     prediction_validate <- knn(text.df[ind.train,-nc], text.df[ind.validate,-nc],text.df$flag[ind.train], k = k)
     prediction_test <- knn(text.df[ind.train,-nc], text.df[ind.test,-nc],text.df$flag[ind.train], k = k)
  }
  if (ml.model == "SVM") {
    library(e1071)
    print(paste("-- Start Support Vector Machines training and prediction with cost:", cost, ", gamma:",gamma,"sparse level:",sparselevel))
    weight <- min(table(text.df$flag[ind.train]))/table(text.df$flag[ind.train])
    print(paste('weight: ',weight))
    classifier <- svm(flag ~ ., data = text.df[ind.train,],cost=cost,gamma=gamma, class.weights = weight)
#    classifier <- svm(text.df[ind.train,], text.df$flag[ind.train],cost=cost,gamma=gamma)
    print("-- Start to calculate the prediction")
    prediction_train <- predict(classifier, newdata = text.df[ind.train,])
    prediction_validate <- predict(classifier, newdata = text.df[ind.validate,])
    prediction_test <- predict(classifier, newdata = text.df[ind.test,])
  }
  if (ml.model == "tune svm"){
    library(e1071)
    print("-- Start to tune SVM")
    obj <- tune.svm(text.df[ind.train,],text.df$flag[ind.train],  
                    validation.x =text.df[ind.validate,],ind.validate =text.df$flag[ind.validate],
                    gamma = 2^(-1:1), cost = 2^(1:4))
    summary(obj)
    plot(obj)
  }
  if (ml.model == "tune svm"){
    library(e1071)
    print("-- Start to tune SVM")
    obj <- tune.svm(text.df[ind.train,],text.df$flag[ind.train],  
                  validation.x =text.df[ind.validate,],ind.validate =text.df$flag[ind.validate],
                  gamma = 2^(-1:1), cost = 2^(1:4))
    summary(obj)
    plot(obj)
  }

  summary_train <- confusionMatrix(table(prediction_train, text.df$flag[ind.train]))
  print(summary_train)
  summary_validate <- confusionMatrix(table(prediction_validate, text.df$flag[ind.validate]))
  print(summary_validate)
  summary_test <- confusionMatrix(table(prediction_test, text.df$flag[ind.test]))
  print(summary_test)
  return(list(train=summary_train,validate=summary_validate,test=summary_test))
}

evaluateprediction <- function(plottype, ml.model, text.df, cleandata,cost=cost,gamma=gamma,sparselevel=sparselevel,k=k){
  print("Start to train and evaluate the prediction")
  result.df <- data.frame(xvar = numeric(0), variable = character(0), value = numeric(0))
  for(x in plottype[[1]]){
    subind <- seq_along(text.df$flag)

    if( names(plottype) == "feature_curve") {
      sparselevel <- x
      text.df <- createDF(cleandata,sparselevel=sparselevel,ml.model=ml.model)
    }
    
    if( names(plottype) == "learning_curve") subind = 1:round(nrow(text.df)*x)
    
    if( names(plottype) == "cost_curve") cost <- x
    if( names(plottype) == "gamma_curve") gamma <- x
    if( names(plottype) == "k_curve") k <- x
    
    if( names(plottype) == "cost_gamma") {
      cost <- x$cost[1]
      gamma<-x$gamma[1]
    }
    if (names(plottype) == "learning_curve")  xvar <- length(subind) else xvar <- x
    print(paste(names(plottype),xvar))  
    summary <- traindata(text.df[subind,], ml.model=ml.model,cost=cost,gamma=gamma,k=k,sparselevel=sparselevel) 

    result.df <- rbind(result.df
                       ,list(xvar, "train_tpr",summary$train$byClass[1])
                       ,list(xvar, "train_BAccuracy",summary$train$byClass[8])
                       ,list(xvar, "train_tnr",summary$train$byClass[2])
                       ,list(xvar, "validate_tpr",summary$validate$byClass[1]) 
                       ,list(xvar, "validate_BAccuracy",summary$validate$byClass[8])                      
                       ,list(xvar, "validate_tnr",summary$validate$byClass[2])
                       ,list(xvar, "test_tpr",summary$test$byClass[1])
                       ,list(xvar, "test_BAccuracy",summary$test$byClass[8])
                       ,list(xvar, "test_tnr",summary$test$byClass[2])
                   
    )
  }
  names(result.df) <- c("xvar","variable","value")
  result.df
}

plotcurve <- function(plottype, ml.model,text.df,result.df,cost=cost,gamma=gamma,sparselevel=sparselevel,k=k){
  graphtitle <- paste(names(plottype), ", ",ml.model,"model")
  if (names(plottype) != "feature_curve") graphtitle <- paste(graphtitle,"sparse level:",sparselevel,", features num:",ncol(text.df))
  if ((ml.model == "SVM") & (names(plottype) != "cost_curve")) graphtitle<- paste(graphtitle,", C: ",cost)
  if ((ml.model == "SVM") & (names(plottype) != "gamma_curve")) graphtitle<- paste(graphtitle,", gamma",gamma)
  if (ml.model == "kNN") graphtitle <-  paste(graphtitle, ", k: ",k,sep="")
  
  plot <- ggplot(data = result.df, aes(x=xvar,y=value,color=variable))+ylim(0.6, 1)+ ggtitle(graphtitle)
  plot <- plot+ geom_point(aes(shape = variable))+geom_line(data=result.df[grep("validate",result.df$variable),],size=1,linetype="dashed")+geom_line(data=result.df[grep("train",result.df$variable),],size=1,linetype="solid")
  plot <- plot+scale_color_manual(values=c("#56B4E9","#D55E00", "#009E73","#56B4E9", "#D55E00",   "#009E73"))
  plot <- plot+xlab(names(plottype))
  plot
}

saveplot <- function(plottype, ml.model,cost=cost,gamma=gamma,sparselevel=sparselevel,k=k){
  filename <- paste("img/",names(plottype),"_",ml.model,sep="")
  if (names(plottype) != "feature_curve") filename <- paste(filename,"_sl",sparselevel,sep="")
  if (ml.model == "SVM" & names(plottype) != "cost_curve") filename <-  paste(filename, "_c",cost,sep="")
  if (ml.model == "SVM" & names(plottype) != "gamma_curve") filename <-  paste(filename, "_gamma",gamma,sep="")
  if (ml.model == "kNN" & names(plottype) != "k_curve") filename <-  paste(filename, "_k",k,sep="")
  filename <- gsub("[.]","",filename)
  filename <-  paste(filename, ".png",sep="")
  ggsave(file=filename)
  dev.off()
}