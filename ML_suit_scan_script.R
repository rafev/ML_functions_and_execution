setwd("~/Google Drive/My Drive/AIBL.VITACOG_3class/")
load("Oct 2024 post-reprocessed/AIBL.VITACOG_3class_reprocessed.rda")
module_df = readRDS("Oct 2024 post-reprocessed/module_df_Boruta.trimmed.rds")
source("3-class functions.R")

train_pheno$disease.status.abv = factor(train_pheno$disease.status.abv, levels = c("CN", "MCI", "AD"))
all(train_pheno$gsm == rownames(train_grp))

valid_pheno$disease.status.abv = factor(valid_pheno$disease.status.abv, levels = c("CN", "MCI", "AD"))
all(valid_pheno$gsm == rownames(valid_grp))


## --------------
# Load in libraries
## --------------
library(dplyr)
library(doFuture)
library(pROC)
library(caret)
library(caTools)
library(data.table)
library(stringr)
library(progressr)


## --------------
# Set up objects for testing
## --------------

##-- set up cross-validation parameters
set.seed(1234)
folds = 10
repeats = 5
cctrl1 = trainControl(method = "repeatedcv",   
                      number = folds,     # number of folds
                      repeats = repeats,  # number of repeats
                      classProbs = T,     # assigns class probability
                      summaryFunction = multiClassSummary, # for 3+ class problems
                      selectionFunction = best, # chooses the class with the highest probability
                      sampling = "up")  # method of sumbampling within Train's sampling routine

## --------------
# Parallell and Progress setup
## --------------
registerDoFuture()
plan(multicore)

options(future.globals.maxSize = 1932735283) # changes the maxsize for globals to be 1.8 GiB for future objects
options(doFuture.rng.orMisuse = "ignore")   # avoid error message for random number generation being used within function

#Set up progress bar
handlers(global = TRUE)

## --------------
# 1way Module Algorithm
## --------------
start.time = Sys.time()
oneway_class_list = list()
oneway_class_list = ML_suite_scan(one_way_modlist, "disease.status.abv", "gsm")
names(oneway_class_list) = one_way_modlist
end.time = Sys.time()

#time spent processing loop
difftime(end.time, start.time)













## ------------------------------
# Assess validation results
## ------------------------------
valid_pheno$disease.status.abv = factor(valid_pheno$disease.status.abv, levels = c("CN", "MCI", "AD"))
if(!all(valid_pheno$gsm == rownames(valid_grp))){
  valid_pheno = valid_pheno[match(rownames(valid_grp), valid_pheno$gsm),]
}
all(valid_pheno$gsm == rownames(valid_grp))

ML_suite_scan_CF = list()

for(i in names(ML_suite_scan)) {
  
  cat("\nProcessing validation set Confusion Matrix for module", i, "...\n")
  
  ML.ROC_validation = sapply(names(ML_suite_scan[[i]][["ML_models"]]), function(j){
    
    cat("..",j, "model\n")
    Prediction.valid = predict(object = ML_suite_scan[[i]][["ML_models"]][[j]],
                               newdata = valid_grp[, module_df[module_df$colors == i, 1]])
    
    cf = caret::confusionMatrix(Prediction.valid,
                                valid_pheno$disease.status.abv)
    
    return(cf)
  }, simplify = F, USE.NAMES = T)
  
  ML_suite_scan_CF[[i]] = ML.ROC_validation
}