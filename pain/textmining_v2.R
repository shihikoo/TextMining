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


preprocessing <- function(alldata, doublescreening=0){
  print("-- Start to prepare data")
  if (doublescreening == 1){
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
} else cleandata <- alldata
  #normalize the text
  cleandata$text  <- normalizetext(cleandata$text)
  # randomalize the data
  cleandata
}

# Detailed normalization on text
normalizetext <- function(text){
  library(tm)
  print("---- Start text normalization")
  
  #lower-casing
  print("-- Start to lower the case")
  text <- tolower(text)
  
  #delete all the \n that was induced due to the conversion from xlsx to txt
  print("-- Start to remove \n \f")
  text <- gsub("\n|\f", " ", text)
  
  print("-- Start to remove numbers")
  text <- gsub("[0-9]*[.]?[0-9]+", " ", text)
  #  removeNumbers(text)
  
  print("-- Start to remove punctuation")
  text <- removePunctuation(text)
  
  print('-- start to remove stopwords')
  text <- removeWords(text, c(stopwords("SMART")))
  
  print("-- Start to stem the text")
  text <- stemDocument(text)
  
  print("-- Start to strip white space")
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

indexgenerator <- function(n,ratio.train = -1,ratio.test = 0.2){
    set.seed(42)
#we keep part of data for testing the model. The rest of data are used for training and validation
    ind.test <- sample(n, round(n*ratio.test))
    if ((ratio.train < 0) | (ratio.train+ratio.test > 1)){
        ind.train <- (1:n)[-ind.test]
    } else{
        if (random == TRUE) ind.train <- sample((1:n)[-ind.test],round(n*ratio.train))
    }
    index <- list(test=ind.test,train=ind.train)
}

traindata <- function(text.df, ml.model='SVM',cost=2,gamma=0.002,k=1){
  print("-- Start to split the data")
  if (ml.model == "NB") {
    library(e1071)
    print(paste("-- Start naive Bayes training and prediction with sparse level:",sparselevel))
    classifier <- naiveBayes(flag ~ ., data = text.df)
  }
  if (ml.model == "kNN") {
    library(caret)
    print("-- Start k-Nearest Neighbour training and prediction")
    classifier <- knn3(flag ~ ., data = text.df, k = k)
  }
  if (ml.model == "SVM") {
    library(e1071)
    print(paste("-- Start Support Vector Machines training and prediction with cost:", cost, ", gamma:",gamma,"sparse level:",sparselevel))
    weight <- min(table(text.df$flag))/table(text.df$flag)
    print(paste('SVM weight: ',weight))
    classifier <- svm(flag ~ ., data = text.df, cost=cost, gamma=gamma, class.weights = weight)
  }
  return(classifier)
}

predict <- function(classifier,text.df){
    library(caret)
    print("-- Start to calculate the prediction")
    prediction <- predict(classifier, newdata = text.df)
    summary <- confusionMatrix(table(prediction, text.df$flag))
    print(summary)
    return(summary)
}

tunemodel <- function(cleandata,ml.model='SVM',range.cost = c(0.01, 0.1, 1, 10), range.gamma = c(0.001,0.1,1,10), range.sl = c(0.8, 0.9, 0.95, 0.99), range.k = c(1,3,5,7,9), kfold = 0){
    result.df <- data.frame(sparselevel = numberic(0), cost = numeric(0), gamma = numeric(0), train.sensitivity = numeric(0), train.specivicity = numeric(0),train.BalancedAccuracy = numeric(0), test.sensitivity = numeric(0), test.specivicity = numeric(0), test.BalancedAccuracy = numeric(0))
    nr = nrow(cleandata)
    if (kfold > 0) {
      for(ifold in 1:kfold){
          ind.validate <- ((ifold-1)*(nr/kfold)+1):(ifold*(nr/kfold))
          ind.train <- (1:nr)[-ind.validate]
  #        print (paste(ind.validate[1],ind.validate[length(ind.validate)]))
   #       print (paste(ind.train[1], ind.train[length(ind.train)]))
           result <- tune(ind.train, ind.validate)
  }
} else {
    ind.validate <- 1:round(nr*0.2)
    ind.train <- (1:nr)[-ind.validate]
    result <- tune(ind.train, ind.validate)
}

tune <- function(ind.train, ind.validate){
    for(sparselevel in rangel.sl){
        text.df <- createDF(cleandata, sparselevel = sparselevel, ml.model = ml.model, createwc = FALSE)
        for (cost in range.cost){
            for (gamma in range.gamma){
                classifier <- traindata(text.df[ind.train], ml.model = ml.model,cost=cost,gamma=gamma,sparselevel=sparselevel)
                summary <- predict(classifier, text.df[ind.validate,])
                result.df <- rbind(result.df,c(sparselevel,cost, gamma,summary$train$byClass[1],summary$train$byClass[2],summary$train$byClass[8],summary$test$byClass[1],summary$test$byClass[2],summary$test$byClass[8]))
            }
        }
    }
    return(result.df)
}
    sensitivity <- result.df[,7]
    specificity <- result.df[,8]
    BA <- result.df[,9]
#set up the rule here for choising the best parameter for the best model.
    good.ind <- which(sensitivity > 0.8)
    best.ind <- which.max(BA[good.ind])

    result.df$best.sl <- sparselevel[good.ind[best.ind]]
    result.df$best.cost <- cost[good.ind[best.ind]]
    result.df$best.gamma <- gamma[good.ind[best.ind]]

    return(result.df)
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
}
