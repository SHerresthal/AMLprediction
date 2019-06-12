# loading libraries
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
print("all libraries loaded")


# load environment variables for R 
#source(paste(Sys.getenv(c("STEFFI_R_HOME")), "Classifier/R_envo.R", sep = ""))

#source(paste(scripts_location, "Functions.R", sep = ""))
#load(paste(data_location, "Datasets_V2.RData", sep = ""))

#source(paste(Sys.getenv(c("STEFFI_R_HOME")), "Classifier/R_envo.R", sep = ""))


#print("all data loaded")
# print(paste(server_var, data_location, output_calc))

# create output directories
#dir.create(paste(output_calc,"Randomsampling/Dataset_1/all/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Randomsampling/Dataset_1/leukemia/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Randomsampling/Dataset_2/all/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Randomsampling/Dataset_2/leukemia/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Randomsampling/Dataset_3/all/", sep = ""), showWarnings = F, recursive = T)

#dir.create(paste(output_calc,"Crossstudy/Dataset_1/all/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Crossstudy/Dataset_1/leukemia/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Crossstudy/Dataset_2/all/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Crossstudy/Dataset_2/leukemia/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Crossstudy/Dataset_3/all/", sep = ""), showWarnings = F, recursive = T)

#dir.create(paste(output_calc,"Crossplatform/raw/D1_D2/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Crossplatform/raw/D1_D3/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Crossplatform/raw/D2_D3/", sep = ""), showWarnings = F, recursive = T)

#dir.create(paste(output_calc,"Crossplatform/rank/D1_D2/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Crossplatform/rank/D1_D3/", sep = ""), showWarnings = F, recursive = T)
#dir.create(paste(output_calc,"Crossplatform/rank/D2_D3/", sep = ""), showWarnings = F, recursive = T)


# Calculations
# 1.A Random Sampling Dataset 1, all samples
#ntest <- 500
#for(ntrain in c(100, 250, 500, 1000, 1500, 2000)){
#  print(ntrain)
#  randomsampling(nperm = 2, # number of permutations
#                       info = info.1, # metadata 
#                       data = data.1, # data
#                       server = server_var, # for linux-machine = T, for windows = F
#                       size.ts = ntrain, # training set size
#                       size.vs = ntest, # test set size
#                       cores = ncores, # number of cores for parallel processing
#                       leukemia =  F,       
#                       dir = paste(output_calc,"Randomsampling/Dataset_1/all/", sep = "")) # output directory
#}
