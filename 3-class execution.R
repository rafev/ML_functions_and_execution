## --------------
# Setting working directory and loading functions
## --------------
setwd("~/Google Drive/My Drive/ADNI Public Dataset/")
source("~/Google Drive/My Drive/ADNI Public Dataset/3-class functions.R")


## --------------
# Load in libraries
## --------------
library(glmnet)
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

##-- Read in ONE of the objects needed from RStudio
load("three.class_loop.rda")


## --------------
# Parallell and Progress setup
## --------------
registerDoFuture()
plan(multicore)

options(future.globals.maxSize = 1932735283) # changes the maxsize for globals to be 1.8 GiB for future objects
options(doFuture.rng.orMisuse = "ignore")   # avoid error message for random number generation being used within function

#Set up progress bar
handlers(global = TRUE)
#handlers("cli")
#handlers("progress")
handlers(handler_progress(
  format = ":spin :current/:total [:bar] (:message) :percent in :elapsed ETA: :eta",
  width = 150,
  complete = "=")
)


## --------------
# 1way Module Algorithm
## --------------
start.time = Sys.time()
oneway_class_list = list()
oneway_class_list = my_1way.mods(one_way_modlist)
names(oneway_class_list) = one_way_modlist
end.time = Sys.time()

#time spent processing loop
difftime(end.time, start.time)

## --- Push back data
saveRDS(oneway_class_list, file = "oneway_class_list.rds")



## --------------
# 1way SEQUENTIAL RF Module Algorithm
## --------------
start.time = Sys.time()
oneway_class_list = my_seq.RF_1way.mods(one_way_modlist)
names(oneway_class_list) = one_way_modlist
end.time = Sys.time()

#time spent processing loop
difftime(end.time, start.time)

## --- Push back data
saveRDS(oneway_class_list, file = "oneway_class_list.rds")



## --------------
# 2way Module Algorithm
## --------------
start.time = Sys.time()
twoway_class_list = list()
twoway_class_list = my_2way.mods(two_way_modlist)
names(twoway_class_list) = two_way_modlist
end.time = Sys.time()

#time spent processing loop
difftime(end.time, start.time)

## --- Push back data
saveRDS(twoway_class_list, file = "twoway_class_list.rds")



## --------------
# 3way Module Algorithm
## --------------
start.time = Sys.time()
threeway_class_list = list()
threeway_class_list = my_3way.mods(three_way_modlist)
names(threeway_class_list) = three_way_modlist
end.time = Sys.time()

#time spent processing loop
difftime(end.time, start.time)

## --- Push back data
saveRDS(threeway_class_list, file = "threeway_class_list.rds")
