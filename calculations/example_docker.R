# test in docker: 
docker run -v /e/Stefanie/Classifier/Scripts_github:/dats/scripts -it sherresthal/classifer:0.0.1 bash
R

# in R:

library("data.table")
library("class")
library("e1071")
library("ROCR")
library("MASS")
library("randomForest")
library("glmnet")
library("foreach")
library("doParallel")
library("multtest")
library("affy")
library("pamr")
library("dplyr")
library("limma")
source("/data/scripts/functions/classify.R")
load("/data/scripts/data/Datasets.RData")

nperm_random <- 2 # for random sampling
nperm_crossstudy <- 2 # for cross study sampling
nperm_crossplatform <- 2 # for cross platform sampling
linux_run <- T # for linux-machine = T, for windows = F
ncores <- 1 # number of cores for parallel processing
  
  
randomsampling(nperm = 2, # number of permutations
               info = info.1, # metadata 
               data = data.1, # data
               server = T, # for linux-machine = T
               size.ts = 10, # training set size
               size.vs = 10, # test set size
               cores = 1, # number of cores for parallel processing, 
               classifiers. = c("SVM_linear", "SVM_radial", "SVM_polynomial", "SVM_sigmoid", "PAM", "KNN", "LASSO", "RTF"),
               leukemia =  F,       
               dir = "Randomsampling/Dataset_1/all/")