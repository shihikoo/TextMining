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
preprocessing <- function(alldata,datasetratio1=1){
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
  set.seed(1)
  cleandata[sample(round(nrow(cleandata)*datasetratio1)),]
}

# Detailed normalization on text
normalizeabstract <- function(abstract){
  library(tm)
  print("---- Start abstract normalization")
  
  #lower-casing
  abstract <- tolower(abstract)
  
  #delete all the \n that was induced due to the conversion from xlsx to txt
  abstract <- gsub("\n", " ", abstract)
  
  #normalize numbers (all number including int, float are replaced with "anynumber", percentage to "percentage"), so they can be considered equally
  abstract <- gsub("[0-9]*[.]?[0-9]+", "anynumber", abstract) 
  abstract <- gsub("[0-9a-z]+@[0-9a-z]+","emailadd",abstract)
  abstract <- gsub("anynumber%","percentage",abstract)
  abstract <- gsub("anynumberanynumber", "anynumber", abstract) 
  
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
  print("-- Start to make corpus from the abstract")
  abstract.corpus <- Corpus(VectorSource(cleandata$abstract))
  if (createwc == T) createwordcloud(cleandata$flag, abstract.corpus)
  
  print("-- Start to build Document Term Matrix")
  abstract.dtm <- DocumentTermMatrix(abstract.corpus, 
                                     control = list(weighting = weightTfIdf, minWordLength = 3))
  abstract.dtm <- removeSparseTerms(abstract.dtm, sparselevel)
  if (ml.model == "NB") {
    print("start to convert counts to 0 or 1")
    abstract.df <- as.data.frame(apply(abstract.dtm, 2, convert_count))
  } else {
    abstract.df <- as.data.frame(inspect(abstract.dtm))
  }
  abstract.df$flag <- cleandata$flag
  return(abstract.df)
}

traindata <- function(abstract.df, ml.model="SVM",cost=1,gamma=1,k=3,sparselevel=0.98,fold=0){
  library(caret)
  print("-- Start to split the data")
  nr <- nrow(abstract.df)
  nc <- ncol(abstract.df)
  set.seed(42)
  ind.train <- sample(nr,round(nr*0.6))
  ind.validate <- sample( (1:nr)[-ind.train], round(nr*0.2))
  ind.test <- (1:nr)[-c(ind.train,ind.validate)]
 
  index <- 1:nr
  ind.test <- index(1:round(nr*0.2))
  for(kfold in 1:(fold+1)){  
     ind.validate <- (kfold*(nr/fold)):((1+kfold)*(nr/fold))
     ind.train <- index [-c(ind.test,ind.train)]
  }
  
  
  if (ml.model == "NB") {
    library(e1071)
    print(paste("-- Start naive Bayes training and prediction with sparse level:",sparselevel))
    classifier <- naiveBayes(flag ~ ., data = abstract.df[ind.train,])
    print("-- Start to calculate the prediction")
    prediction_train <- predict(classifier, newdata = abstract.df[ind.train,])
    prediction_validate <- predict(classifier, newdata = abstract.df[ind.validate,])
  }
  if (ml.model == "kNN") {
    library(caret)
    print("-- Start k-Nearest Neighbour training and prediction")
  #  classifier <- knn3(flag ~ ., abstract.df[ind.train,], k = k) 
     library(class)
     prediction_train  <- knn(abstract.df[ind.train,-nc], abstract.df[ind.train,-nc],abstract.df$flag[ind.train], k = k)
     prediction_validate <- knn(abstract.df[ind.train,-nc], abstract.df[ind.validate,-nc],abstract.df$flag[ind.train], k = k)
  }
  if (ml.model == "SVM") {
    library(e1071)
    print(paste("-- Start Support Vector Machines training and prediction with cost:", cost, ", gamma:",gamma,"sparse level:",sparselevel))
    weight <- min(table(abstract.df$flag[ind.train]))/table(abstract.df$flag[ind.train])
    print(paste('weight: ',weight))
    classifier <- svm(flag ~ ., data = abstract.df[ind.train,],cost=cost,gamma=gamma, class.weights = weight)
#    classifier <- svm(abstract.df[ind.train,], abstract.df$flag[ind.train],cost=cost,gamma=gamma)
    print("-- Start to calculate the prediction")
    prediction_train <- predict(classifier, newdata = abstract.df[ind.train,])
    prediction_validate <- predict(classifier, newdata = abstract.df[ind.validate,])
  }
  if (ml.model == "tune svm"){
    library(e1071)
    print("-- Start to tune SVM")
    obj <- tune.svm(abstract.df[ind.train,],abstract.df$flag[ind.train],  
                    validation.x =abstract.df[ind.validate,],ind.validate =abstract.df$flag[ind.validate],
                    gamma = 2^(-1:1), cost = 2^(1:4))
    summary(obj)
    plot(obj)
  }

  summary_train <- confusionMatrix(table(prediction_train, abstract.df$flag[ind.train]))
  print(summary_train)
  summary_validate <- confusionMatrix(table(prediction_validate, abstract.df$flag[ind.validate]))
  print(summary_validate)
  
  return(list(train=summary_train,validate=summary_validate))
}

evaluateprediction <- function(plottype, ml.model, abstract.df, cleandata,cost=cost,gamma=gamma,sparselevel=sparselevel,k=k){
  print("Start to train and evaluate the prediction")
  result.df <- data.frame(xvar = numeric(0), variable = character(0), value = numeric(0))
  for(x in plottype[[1]]){
    subind <- seq_along(abstract.df$flag)

    if( names(plottype) == "feature_curve") {
      sparselevel <- x
      abstract.df <- createDF(cleandata,sparselevel=sparselevel)
    }
    
    if( names(plottype) == "learning_curve") subind = 1:round(nrow(abstract.df)*x)
    
    if( names(plottype) == "cost_curve") cost <- x
    if( names(plottype) == "gamma_curve") gamma <- x
    if( names(plottype) == "k_curve") k <- x
    
    if( names(plottype) == "cost_gamma") {
      cost <- x$cost[1]
      gamma<-x$gamma[1]
    }
    if (names(plottype) == "learning_curve")  xvar <- length(subind) else xvar <- x
    print(paste(names(plottype),xvar))  
    summary <- traindata(abstract.df[subind,], ml.model=ml.model,cost=cost,gamma=gamma,k=k,sparselevel=sparselevel) 

    result.df <- rbind(result.df
                       ,list(xvar, "train_tpr",summary$train$byClass[1])
                       ,list(xvar, "validate_tpr",summary$validate$byClass[1])
                   #    ,list(xvar, "train_ppv",summary$train$byClass[3])
                   #    ,list(xvar, "validate_ppv",summary$validate$byClass[3])
                   #    ,list(xvar, "train_f1", 2*summary$train$byClass[1]*summary$train$byClass[3]/(summary$train$byClass[1]+summary$train$byClass[3]))
                   #    ,list(xvar, "validate_f1", 2*summary$validate$byClass[1]*summary$validate$byClass[3]/(summary$validate$byClass[1]+summary$validate$byClass[3]))
                       ,list(xvar, "train_BAccuracy",summary$train$byClass[8])
                       ,list(xvar, "validate_BAccuracy",summary$validate$byClass[8])
                       ,list(xvar, "train_tnr",summary$train$byClass[2])
                       ,list(xvar, "validate_tnr",summary$validate$byClass[2])
    )
  }
  names(result.df) <- c("xvar","variable","value")
  result.df
}

plotcurve <- function(plottype, ml.model,abstract.df,result.df,cost=cost,gamma=gamma,sparselevel=sparselevel,k=k){
  graphtitle <- paste(names(plottype), ", ",ml.model,"model")
  if (names(plottype) != "feature_curve") graphtitle <- paste(graphtitle,"sparse level:",sparselevel,", features num:",ncol(abstract.df))
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
}