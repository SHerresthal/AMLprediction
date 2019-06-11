# core function for predicting with nine algorithms: 
# linear, radial, polynomial, sigmoid support vector machine, prediction analyis of microarrays (PAM), linear discriminant analysis (LDA), k nearest neighbours (KNN), LASSO, random forest (RTF)

classify <- function(ts, # training set
                     classes.ts, # true classes of the training set
                     vs, # validation set
                     classes.vs,  # true classes ofthe validation set
                     classifiers = c("SVM_linear", "SVM_radial", "SVM_polynomial", "SVM_sigmoid", "PAM", "LDA", "KNN", "LASSO", "RTF"), 
                     measures = c("AUC", "train.error", "test.error", "SENS", "SPEC", "ACC"), # measures to be calculated
                     predictions_table = F, # should a list be printed that shows the individual results for each validation sample
                     nperm = 1) # number of permutations
{
  # create result table
  results <- data.table(classifier = c(rep(classifiers, each = (length(measures)))), 
                        measure = rep(measures, length(classifiers)),
                        value = as.numeric(0), 
                        perm = nperm,
                        number_features = ifelse(is.vector(ts), 1, dim(ts)[1]), 
                        size.ts = ifelse(is.vector(ts), length(ts), dim(ts)[2]))
  
  if(is.vector(vs) == T){
    result.predictions <- data.table(Filename = names(vs),
                                     true_class = classes.vs, 
                                     perm = nperm)
    
  }else{
    result.predictions <- data.table(Filename = colnames(vs),
                                     true_class = classes.vs, 
                                     perm = nperm)
  }
  
  if(is.matrix(ts)){
    if(dim(ts)[1] > 2){
      print(paste("transpose"))
      ts  <- t(ts)
      vs <- t(vs)
      for (i in 1:length(classifiers)){
        
        if (classifiers[i] == "SVM_linear"){
          print(paste(classifiers[i], Sys.time()))
          model.temp <-  svm(ts, as.factor(classes.ts), kernel="linear", cross=10, probability = TRUE)
          pred.all <- attr(predict(model.temp, vs, probability=T), "probabilities")
          pred.dist <- pred.all[,which(colnames(pred.all)=="CONTROL")]
          train.error <- 1- mean(predict(model.temp, ts) == classes.ts)
        }
        if (classifiers[i] == "SVM_radial"){
          print(paste(classifiers[i], Sys.time()))
          model.temp  <- svm(ts, as.factor(classes.ts), kernel="radial", cross=10, probability = TRUE)
          pred.all <- attr(predict(model.temp, vs, probability=T), "probabilities")
          pred.dist <- pred.all[,which(colnames(pred.all)=="CONTROL")]
          train.error <- 1- mean(predict(model.temp, ts) == classes.ts)
        }
        if (classifiers[i] == "SVM_polynomial"){
          print(paste(classifiers[i], Sys.time()))
          model.temp <- svm(ts, as.factor(classes.ts), kernel="polynomial", cross=10, probability = TRUE)
          pred.all <- attr(predict(model.temp, vs, probability=T), "probabilities")
          pred.dist <- pred.all[,which(colnames(pred.all)=="CONTROL")]
          train.error <- 1- mean(predict(model.temp, ts) == classes.ts)
        }
        if (classifiers[i] == "SVM_sigmoid"){
          print(paste(classifiers[i], Sys.time()))
          model.temp <- svm(ts, as.factor(classes.ts), kernel="sigmoid", cross=10, probability = TRUE)
          pred.all <- attr(predict(model.temp, vs, probability=T), "probabilities")
          pred.dist <- pred.all[,which(colnames(pred.all)=="CONTROL")]
          train.error <- 1- mean(predict(model.temp, ts) == classes.ts)
        }
        if (classifiers[i] == "PAM"){
          print(paste(classifiers[i], Sys.time()))
          model.temp <- pamr.train(list(x = as.matrix(t(ts)), threshold = 0, y = classes.ts))
          pred.all <- pamr.predict(model.temp, as.matrix(t(vs)), 0, type="posterior")
          pred.dist <- pred.all[,which(colnames(pred.all)=="CONTROL")]
          train.error <- 1- mean(pamr.predict(model.temp, as.matrix(t(ts)), 0) == classes.ts)
          
        }
        if (classifiers[i] == "LDA"){
          print(paste(classifiers[i], Sys.time()))
          subset <- colSums(ts)!=0 | colSums(vs)!=0
          ts.lda <- ts[,subset]
          model.temp <- lda(x = ts.lda, grouping = classes.ts)
          model.pred <- predict(model.temp, vs[,subset])$posterior
          pred.dist <- model.pred[,which(colnames(model.pred)=="CONTROL")]
          train.error <- 1- mean(predict(model.temp)$class == classes.ts)
          }
        if (classifiers[i] == "KNN"){
          print(paste(classifiers[i], Sys.time()))
          model.temp <- knn(train = ts, test = ts, cl = as.factor(classes.ts), k = 6)
          train.error <- 1- mean(model.temp == classes.ts)
          model.temp <- knn(train = ts, test = vs, cl = as.factor(classes.ts), k = 6, prob = T)
          pred <- attr(model.temp, "prob")
          pred.dist <- ifelse(model.temp == "CASE", 1-pred, pred)
        }
        if (classifiers[i] == "LASSO"){
          print(paste(classifiers[i], Sys.time()))
          model.temp <- cv.glmnet(x = ts, y = as.factor(classes.ts), family = "binomial", alpha = 1) # cv to find optimal s (penaltly parameter lambda)
          train.error <- 1- mean(predict.cv.glmnet(model.temp, newx = ts, type = "class") == classes.ts)
          pred.dist <- predict(model.temp, newx = vs, type = "response", prob = T, s = model.temp$lambda.min) # prediction using optimal lambda
        }
        if (classifiers[i] == "RTF"){
          print(paste(classifiers[i], Sys.time()))
          model.temp <-  randomForest(x = ts, y = as.factor(classes.ts), importance = T)
          train.error <- 1- mean(predict(model.temp, newdata = ts) == classes.ts)
          pred.dist <- predict(model.temp, newdata = vs, type = "prob")[,which(colnames(pred.all)=="CONTROL")]
        }
        result.predictions$probablity_control <- pred.dist
        AUC <- performance(prediction(pred.dist,as.factor(classes.vs)),measure="auc")@y.values[[1]]
        
        result.predictions[[classifiers[i]]] <- ifelse(pred.dist >= 0.5, "CONTROL", "CASE") 
        test.error <- 1 - mean(result.predictions[[classifiers[i]]] == classes.vs)
        P <- sum(result.predictions[[classifiers[i]]] == "CASE") # all positive predictions
        TP <- sum(classes.vs[which(result.predictions[[classifiers[i]]] == "CASE")] == "CASE") # true positives
        FP <- sum(classes.vs[which(result.predictions[[classifiers[i]]] == "CASE")] == "CONTROL") # false positives
        
        N <- sum(result.predictions[[classifiers[i]]] == "CONTROL") # all negative predictions
        TN <- sum(classes.vs[which(result.predictions[[classifiers[i]]] == "CONTROL")] == "CONTROL") # true negatives
        FN <- sum(classes.vs[which(result.predictions[[classifiers[i]]] == "CONTROL")] == "CASE") # false negatives
        
        SENS <- TP/P
        SPEC <- ifelse(N > 0, TN/N, 0)
        ACC <- (TP+TN)/(TP+FP+FN+TN)
        
        
        
        # get results    
        if ("AUC" %in% measures){results[classifier == classifiers[i] & measure == "AUC"]$value <- AUC}
        if ("train.error" %in% measures){results[classifier == classifiers[i] &  measure == "train.error"]$value <- train.error}
        if ("test.error" %in% measures){results[classifier == classifiers[i] &  measure == "test.error"]$value <- test.error}
        if ("SENS" %in% measures){results[classifier == classifiers[i] & measure == "SENS"]$value <- SENS}
        if ("SPEC" %in% measures){results[classifier == classifiers[i] &  measure == "SPEC"]$value <-  SPEC}    
        if ("ACC" %in% measures){results[classifier == classifiers[i] & measure == "ACC"]$value <-  ACC} 
      }
    }
  }
  print(paste("returning results"))
  if (predictions_table == T){
    return(list(results, result.predictions))
  } else {return(results)}
}

# random sampling of case and control samples in each training and test set
randomsampling <- function(info, # metadata
                           data, # data
                           dir, # output directory
                           nperm = 10, # number of permutations
                           server = F, # run on server?
                           classifiers. = c("SVM_linear", "SVM_radial", "SVM_polynomial", "SVM_sigmoid", "PAM", "LDA", "KNN", "LASSO", "RTF"),
                           size.ts = 50, # training set size
                           size.vs = 500, # validation set size
                           leukemia = F, # run only leukemia samples
                           print_predictions = F,  # output also individual prediction results
                           cores = 12 # cores for parallel processing
)
{ 
  
  ifelse(server == T, cl <- makeCluster(cores, outfile = ""), cl <- makeCluster(1, outfile = getwd()))
  
  registerDoParallel(cl)
  
  result <- foreach(j = 1:nperm,
                    .combine = rbind,
                    .packages = c("randomForest", "data.table", "class", "e1071", "ROCR", "multtest", "pamr", "MASS", "glmnet", "dplyr", "limma", "randomForest", "gtools"),
                    .export = c("classify")
  ) %dopar% {
    print(paste("Permutation", j, sep = ":"))
    
    info$Filename <- as.character(info$Filename)
    if (leukemia == F){
      set.seed(j)
      files.ts <- c(sample(info$Filename, size = size.ts))
      set.seed(j)
      files.vs <- c(sample_n(info[!info$Filename %in% files.ts,], size = size.vs)$Filename)
    } else {
      set.seed(j)
      files.ts <- c(sample_n(info[info$Disease %in% c("AML", "AMKL", "ALL", "CLL", "CML", "MDS", "DS transient myeloproliferative disorder", "T.ALL"), ], size = size.ts)$Filename)
      set.seed(j)
      files.vs <- c(sample_n(info[!info$Filename %in% files.ts & info$Disease %in% c("AML", "AMKL", "ALL", "CLL", "CML", "MDS", "DS transient myeloproliferative disorder", "T.ALL"),], size = size.vs)$Filename)
    }
    
    ts. <- data[,files.ts]
    vs. <- data[,files.vs]
    info.ts. <- info[info$Filename %in% files.ts,]
    info.vs. <- info[info$Filename %in% files.vs,]
    
    info.ts. <- info.ts.[order(rownames(info.ts.)),]
    ts. <- ts.[,order(colnames(ts.))]
    info.vs. <- info.vs.[order(rownames(info.vs.)),]
    vs. <- vs.[,order(colnames(vs.))]
    
    classes.ts. <- info.ts.$Condition
    classes.vs. <- info.vs.$Condition
    
    result <- classify(ts = ts., classes.ts = classes.ts., vs = vs., classes.vs = classes.vs., nperm = j, classifiers = classifiers., predictions_table = print_predictions)
    if(print_predictions == T){
      list(result[[2]])
    } else {
      list(result)
    }
  }
  setwd(dir)
  
  # reorder the output to a data frame
  res.values <- data.table()
  for (i in 1:nperm){
    res.values <- rbind(res.values, result[[i]])
  }
  
  # write output to folder
  if(print_predictions == T){
    write.table(res.values, file = paste("results.predictions", "Perm", nperm, "sizeTS", size.ts, "sizeVS", size.vs, "txt", sep = "."), quote = F, sep = "\t")
    
    
  } else {
    write.table(res.values, file = paste("results.values", "Perm", nperm, "sizeTS", size.ts, "sizeVS", size.vs, "txt", sep = "."), quote = F, sep = "\t")
  }
  stopCluster(cl)
  
  
}

# function for cross-study sampling
crossstudy <- function(nperm=10, # number of permutations
                       server = F, # run on server?
                       classifiers. = c("SVM_linear", "SVM_radial", "SVM_polynomial", "SVM_sigmoid", "PAM", "LDA", "KNN", "LASSO", "RTF"),
                       info, # metadata file
                       data, # data 
                       size.ts = 10, # size of training set
                       size.vs = 500, # size of testing set
                       dir, # output directory
                       print_predictions = F, # should individual prediction results be printed
                       cores = 12, # number of cores
                       leukemia = F, # should prediction be run only on leukemia samples?
                       indices.test. = indices.test, # indices test set
                       indices.train. = indices.train, # indices training set
                       y.train. = y.train){ 
  
  ifelse(server == T, cl <- makeCluster(cores, outfile = ""), cl <- makeCluster(1, outfile = getwd()))
  
  registerDoParallel(cl)
  
  result <- foreach(j = 1:nperm,
                    .combine = rbind,
                    .packages = c("randomForest", "data.table", "class", "e1071", "ROCR", "multtest", "pamr", "MASS", "glmnet", "dplyr", "limma", "randomForest", "gtools"),
                    .export = c("classify")
  ) %dopar% {
    print(paste("Permutation", j, sep = ":"))
    # select indices
    
    if(size.ts > length(indices.train.[[j]])){
      indices_used <- indices.train.[[j]]} else {
        set.seed(j); indices_used <- sample(indices.train.[[j]],size.ts)
      }
    
    if(size.ts == 100){
      k<-0
      while(k<=500){
        if(sum(y.train.[indices_used])/100 < 0.1) {set.seed(i+rnorm(1));indices_used<-sample(indices.train.[[j]],100)} 
        # if prevalence of training set for sample size 100 is smaller than 0.1, sample again (do that max 500 times)
        k<-k+1
      }
    }
    
    if(size.ts == 250){
      k<-0
      while(k<=500){
        if(sum(y.train.[indices_used])/250 < 0.1) {set.seed(i+rnorm(1));indices_used <- sample(indices.train.[[j]],250)} 
        # if prevalence of training set for sample size 250 is smaller than 0.1, sample again (do that max 500 times)
        k<-k+1
      }
    }
    
    
    # create trainig set and validation set
    ts. <- data[,indices_used]
    vs. <- data[,indices.test.[[j]]]
    
    info.ts. <- info[info$Filename %in% colnames(ts.),]
    info.vs. <- info[info$Filename %in% colnames(vs.),]
    
    # make sure that annotation and data are in the same order
    info.ts. <- info.ts.[order(rownames(info.ts.)),]
    ts. <- ts.[,order(colnames(ts.))]
    info.vs. <- info.vs.[order(rownames(info.vs.)),]
    vs. <- vs.[,order(colnames(vs.))]
    
    classes.ts. <- info.ts.$Condition
    classes.vs. <- info.vs.$Condition
    
    # calculations
    result <- classify(ts = ts., classes.ts = classes.ts., vs = vs., classes.vs = classes.vs., nperm = j, classifiers = classifiers., predictions_table = print_predictions)
    if(print_predictions == T){
      list(result[[2]])
    } else {
      list(result)
    }
  }
  setwd(dir)
  
  # reorder the output to a data frame
  res.values <- data.table()
  for (i in 1:nperm){
    res.values <- rbind(res.values, result[[i]])
  }
  
  # write output to folder
  if(print_predictions == T){
    write.table(res.values, file = paste("results.predictions", "Perm", nperm, "sizeTS", size.ts, "sizeVS", size.vs, "txt", sep = "."), quote = F, sep = "\t")
    
    
  } else {
    write.table(res.values, file = paste("results.values", "Perm", nperm, "sizeTS", size.ts, "sizeVS", size.vs, "txt", sep = "."), quote = F, sep = "\t")
  }
  stopCluster(cl)
  
}



# cross-platform prediction
crossplatform <- function(nperm=10, # number of permutations
                          server = F, # run on server
                          classifiers. = c("SVM_linear", "SVM_radial", "SVM_polynomial", "SVM_sigmoid", "PAM", "LDA", "KNN", "LASSO", "RTF"),
                          info.ts, # metadata for training set
                          data.ts, # trainig set data
                          info.vs, # metadata for testing set
                          data.vs, # testing set
                          size.ts = 10, # size of testing set (randomly subsampled)
                          size.vs = 500, # size of validation set (randomly subsamples)
                          dir, # output directory
                          print_predictions = F, # should individual predictions be printes
                          cores = 12, # numbers of cores
                          rank = F # should rank transformation be performed
){ 
  
  ifelse(server == T, cl <- makeCluster(cores, outfile = ""), cl <- makeCluster(1, outfile = getwd()))
  
  registerDoParallel(cl)
  
  result <- foreach(j = 1:nperm,
                    .combine = rbind,
                    .packages = c("randomForest", "data.table", "class", "e1071", "ROCR", "multtest", "pamr", "MASS", "glmnet", "dplyr", "limma", "randomForest", "gtools"),
                    .export = c("classify")
  ) %dopar% {
    print(paste("Permutation", j, sep = ":"))
    info.ts$Filename <- as.character(info.ts$Filename)
    info.vs$Filename <- as.character(info.vs$Filename)
    set.seed(j)
    files.ts <- c(sample(info.ts$Filename, size = size.ts))
    set.seed(j)
    files.vs <- c(sample(info.vs$Filename, size = size.vs))
    
    ts. <- data.ts[,files.ts]
    vs. <- data.vs[,files.vs]
    
    # optional rank transformation
    if (rank == T) { 
      ts. <- t(ts.)
      vs. <- t(vs.)  
      ts.<-apply(ts.,2,rank)/(nrow(ts.)+1)
      vs.<-apply(vs.,2,rank)/(nrow(vs.)+1)
      ts.<-apply(ts.,2,qnorm)
      vs.<-apply(vs.,2,qnorm)
      ts. <- t(ts.)
      vs. <- t(vs.)
    }
    
    info.ts. <- info.ts[info.ts$Filename %in% files.ts,]
    info.vs. <- info.vs[info.vs$Filename %in% files.vs,]
    
    info.ts. <- info.ts.[order(rownames(info.ts.)),]
    ts. <- ts.[,order(colnames(ts.))]
    info.vs. <- info.vs.[order(rownames(info.vs.)),]
    vs. <- vs.[,order(colnames(vs.))]
    
    classes.ts. <- info.ts.$Condition
    classes.vs. <- info.vs.$Condition
    
    
    result <- classify(ts = ts., 
                       classes.ts = classes.ts., 
                       vs = vs., 
                       classes.vs = classes.vs., 
                       nperm = j, 
                       classifiers = classifiers., 
                       predictions_table = print_predictions)
    if(print_predictions == T){
      list(result[[2]])
    } else {
      list(result)
    }
  }
  setwd(dir)
  
  # reorder the output to a data frame
  res.values <- data.table()
  for (i in 1:nperm){
    res.values <- rbind(res.values, result[[i]])
  }
  
  # write output to folder
  if(print_predictions == T){
    write.table(res.values, file = paste("results.predictions", "Perm", nperm, "sizeTS", size.ts, "sizeVS", size.vs, "txt", sep = "."), quote = F, sep = "\t")
    
    
  } else {
    write.table(res.values, file = paste("results.values", "Perm", nperm, "sizeTS", size.ts, "sizeVS", size.vs, "txt", sep = "."), quote = F, sep = "\t")
  }
  stopCluster(cl)
}
