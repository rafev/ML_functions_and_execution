## --------------
# Useful Functions
## --------------
NorRMSE = function(actual, prediction) {rmse(actual, prediction)/
    (max(actual) - min(prediction))}

# An empty function for Comments
Comment <- function(`@Comments`) {invisible()}



## --------------
# 1way Module Function
## --------------
my_1way.mods = function(one_way_modlist) {
  p = progressor(along = one_way_modlist)
  foreach (i = one_way_modlist) %dopar% {
    # Run GLMNET
    set.seed(32)
    Net.fit = caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                           y = train_pheno[[covariate_interest]],
                           method = "glmnet",
                           trControl = cctrl1,
                           metric = "Mean_Balanced_Accuracy",
                           tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                  .lambda = seq(0.001, 1, 0.005)),
                           family = "multinomial",
                           type.multinomial = "grouped",
                           maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors == i, 1]],
                         type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors == i, 1]],
                               type = "prob")
    # Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    # Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    # Save outputs to single object then to main list for later reading
    p(sprintf("i=%s", i))
    output = data.frame(alpha = Net.fit$bestTune$alpha,
                        lambda = Net.fit$bestTune$lambda,
                        train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    training = data.frame(Prediction = Prediction, Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
    
    
    ## --- Class outcome for split model
    #CN
    outcome.CN = coef(Net.fit$finalModel,
                                 s=Net.fit$bestTune$lambda)$CN
	outcome.CN = as.matrix(outcome.CN[outcome.CN[,1]!=0,])
	outcome.CN = as.data.frame(outcome.CN) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
	
	#MCI
	outcome.MCI = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$MCI
	outcome.MCI = as.matrix(outcome.MCI[outcome.MCI[,1]!=0,])
	outcome.MCI = as.data.frame(outcome.MCI) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	#AD
	outcome.AD = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$AD
	outcome.AD = as.matrix(outcome.AD[outcome.AD[,1]!=0,])
	outcome.AD = as.data.frame(outcome.AD) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	outcome = cbind(outcome.CN, outcome.MCI, outcome.AD)
	colnames(outcome) = c("CN", "MCI", "AD")
    
    ## --- Export Iteration Results
    oneway_class_list[[i]] = list(output = output,
    									outcome = outcome,
    									model = Net.fit,
    									training = training,
    									validation = valid,
    									train.roc.CN = train.roc.CN,	
    									train.roc.MCI = train.roc.MCI,
    									train.roc.AD = train.roc.AD,
    									valid.roc.CN = valid.roc.CN,
    									valid.roc.MCI = valid.roc.MCI,
    									valid.roc.AD = valid.roc.AD)
  }
}


my_seq_1way.mods = function(one_way_modlist) {
  res_out = list()
  p = progressor(along = one_way_modlist)
  for (i in one_way_modlist) {
    cat("\n",i,"\n")
    # Run GLMNET
    set.seed(32)
    Net.fit = caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                           y = train_pheno[[covariate_interest]],
                           method = "glmnet",
                           trControl = cctrl1,
                           metric = "Mean_Balanced_Accuracy",
                           tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                  .lambda = seq(0.001, 1, 0.005)),
                           family = "multinomial",
                           type.multinomial = "grouped",
                           maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors == i, 1]],
                         type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors == i, 1]],
                               type = "prob")
    # Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                    "CN", # IF MATCH response
                                    "non-CN"), # ELSE response
                             Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                     "MCI", # IF MATCH response
                                     "non-MCI"), # ELSE response
                              Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                    "AD", # IF MATCH response
                                    "non-AD"), # ELSE response
                             Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    # Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                    "CN", # IF MATCH response
                                    "non-CN"), # ELSE response
                             Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                     "MCI", # IF MATCH response
                                     "non-MCI"), # ELSE response
                              Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                    "AD", # IF MATCH response
                                    "non-AD"), # ELSE response
                             Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    # Save outputs to single object then to main list for later reading
    output = data.frame(alpha = Net.fit$bestTune$alpha,
                        lambda = Net.fit$bestTune$lambda,
                        train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    training = data.frame(Prediction = Prediction, Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
    
    
    p(sprintf("i=%s", i))
    
    ## --- Class outcome for split model
    #CN
    outcome.CN = coef(Net.fit$finalModel,
                      s=Net.fit$bestTune$lambda)$CN
    outcome.CN = as.matrix(outcome.CN[outcome.CN[,1]!=0,])
    outcome.CN = as.data.frame(outcome.CN) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
    
    #MCI
    outcome.MCI = coef(Net.fit$finalModel,
                       s=Net.fit$bestTune$lambda)$MCI
    outcome.MCI = as.matrix(outcome.MCI[outcome.MCI[,1]!=0,])
    outcome.MCI = as.data.frame(outcome.MCI) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
    
    #AD
    outcome.AD = coef(Net.fit$finalModel,
                      s=Net.fit$bestTune$lambda)$AD
    outcome.AD = as.matrix(outcome.AD[outcome.AD[,1]!=0,])
    outcome.AD = as.data.frame(outcome.AD) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
    
    outcome = cbind(outcome.CN, outcome.MCI, outcome.AD)
    colnames(outcome) = c("CN", "MCI", "AD")
    
    ## --- Export Iteration Results
    res_out[[i]] = list(output = output,
                        outcome = outcome,
                        model = Net.fit,
                        training = training,
                        validation = valid,
                        train.roc.CN = train.roc.CN,	
                        train.roc.MCI = train.roc.MCI,
                        train.roc.AD = train.roc.AD,
                        valid.roc.CN = valid.roc.CN,
                        valid.roc.MCI = valid.roc.MCI,
                        valid.roc.AD = valid.roc.AD)
  }
  return(res_out)
}


my_1way.PCA.mods = function(one_way_modlist) {
  p = progressor(along = one_way_modlist)
  foreach (i = one_way_modlist) %dopar% {
    # Run GLMNET
    set.seed(32)
    Net.fit = caret::train(x = train_grp[[i]][["x"]],
                           y = train_pheno[[covariate_interest]],
                           method = "glmnet",
                           trControl = cctrl1,
                           metric = "Mean_Balanced_Accuracy",
                           tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                  .lambda = seq(0.001, 1, 0.005)),
                           family = "multinomial",
                           type.multinomial = "grouped",
                           maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[[i]][["x"]],
                         type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[[i]],
                               type = "prob")
    # Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                    "CN", # IF MATCH response
                                    "non-CN"), # ELSE response
                             Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                     "MCI", # IF MATCH response
                                     "non-MCI"), # ELSE response
                              Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                    "AD", # IF MATCH response
                                    "non-AD"), # ELSE response
                             Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    # Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                    "CN", # IF MATCH response
                                    "non-CN"), # ELSE response
                             Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                     "MCI", # IF MATCH response
                                     "non-MCI"), # ELSE response
                              Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                    "AD", # IF MATCH response
                                    "non-AD"), # ELSE response
                             Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    # Save outputs to single object then to main list for later reading
    p(sprintf("i=%s", i))
    output = data.frame(alpha = Net.fit$bestTune$alpha,
                        lambda = Net.fit$bestTune$lambda,
                        train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    training = data.frame(Prediction = Prediction, Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
    
    
    ## --- Class outcome for split model
    #CN
    outcome.CN = coef(Net.fit$finalModel,
                      s=Net.fit$bestTune$lambda)$CN
    outcome.CN = as.matrix(outcome.CN[outcome.CN[,1]!=0,])
    outcome.CN = as.data.frame(outcome.CN) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
    
    #MCI
    outcome.MCI = coef(Net.fit$finalModel,
                       s=Net.fit$bestTune$lambda)$MCI
    outcome.MCI = as.matrix(outcome.MCI[outcome.MCI[,1]!=0,])
    outcome.MCI = as.data.frame(outcome.MCI) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
    
    #AD
    outcome.AD = coef(Net.fit$finalModel,
                      s=Net.fit$bestTune$lambda)$AD
    outcome.AD = as.matrix(outcome.AD[outcome.AD[,1]!=0,])
    outcome.AD = as.data.frame(outcome.AD) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
    
    outcome = cbind(outcome.CN, outcome.MCI, outcome.AD)
    colnames(outcome) = c("CN", "MCI", "AD")
    
    ## --- Export Iteration Results
    oneway_class_list[[i]] = list(output = output,
                                  outcome = outcome,
                                  model = Net.fit,
                                  training = training,
                                  validation = valid,
                                  train.roc.CN = train.roc.CN,	
                                  train.roc.MCI = train.roc.MCI,
                                  train.roc.AD = train.roc.AD,
                                  valid.roc.CN = valid.roc.CN,
                                  valid.roc.MCI = valid.roc.MCI,
                                  valid.roc.AD = valid.roc.AD)
  }
}


my_RF_1way.mods = function(one_way_modlist) {
  p = progressor(along = one_way_modlist)
  foreach (i = one_way_modlist) %dopar% {
    # Run GLMNET
    set.seed(32)
    Net.fit = caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                           y = train_pheno[[covariate_interest]],
                           method = "rf",
                           trControl = cctrl1,
                           metric = "Mean_Balanced_Accuracy",
                           family = "multinomial",
                           type.multinomial = "grouped",
                           maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors == i, 1]],
                         type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors == i, 1]],
                               type = "prob")
    # Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                    "CN", # IF MATCH response
                                    "non-CN"), # ELSE response
                             Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                     "MCI", # IF MATCH response
                                     "non-MCI"), # ELSE response
                              Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                    "AD", # IF MATCH response
                                    "non-AD"), # ELSE response
                             Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    # Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                    "CN", # IF MATCH response
                                    "non-CN"), # ELSE response
                             Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                     "MCI", # IF MATCH response
                                     "non-MCI"), # ELSE response
                              Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                    "AD", # IF MATCH response
                                    "non-AD"), # ELSE response
                             Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    # Save outputs to single object then to main list for later reading
    p(sprintf("i=%s", i))
    output = data.frame(train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    training = data.frame(Prediction = Prediction, Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
    
    
    
    ## --- Export Iteration Results
    oneway_class_list[[i]] = list(output = output,
                                  model = Net.fit,
                                  training = training,
                                  validation = valid,
                                  train.roc.CN = train.roc.CN,	
                                  train.roc.MCI = train.roc.MCI,
                                  train.roc.AD = train.roc.AD,
                                  valid.roc.CN = valid.roc.CN,
                                  valid.roc.MCI = valid.roc.MCI,
                                  valid.roc.AD = valid.roc.AD)
  }
}


my_seq.RF_1way.mods = function(one_way_modlist) {
  res_out = list()
  p = progressor(along = one_way_modlist)
  for (i in one_way_modlist) {
    cat("\n",i,"\n")
    # Run Random Forest
    set.seed(32)
    Net.fit = caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                           y = train_pheno[[covariate_interest]],
                           method = "rf",
                           trControl = cctrl1,
                           tuneLength = 10,
                           metric = "Mean_Balanced_Accuracy",
                           family = "multinomial",
                           type.multinomial = "grouped",
                           maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors == i, 1]],
                         type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors == i, 1]],
                               type = "prob")
    # Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                    "CN", # IF MATCH response
                                    "non-CN"), # ELSE response
                             Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                     "MCI", # IF MATCH response
                                     "non-MCI"), # ELSE response
                              Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                    "AD", # IF MATCH response
                                    "non-AD"), # ELSE response
                             Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    # Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                    "CN", # IF MATCH response
                                    "non-CN"), # ELSE response
                             Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                     "MCI", # IF MATCH response
                                     "non-MCI"), # ELSE response
                              Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                    "AD", # IF MATCH response
                                    "non-AD"), # ELSE response
                             Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    # Save outputs to single object then to main list for later reading
    p(sprintf("i=%s", i))
    output = data.frame(train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    training = data.frame(Prediction = Prediction, Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid, Actual = valid_pheno[[covariate_interest]])
    
        
    ## --- Export Iteration Results
    res_out[[i]] = list(output = output,
                                  model = Net.fit,
                                  training = training,
                                  validation = valid,
                                  train.roc.CN = train.roc.CN,	
                                  train.roc.MCI = train.roc.MCI,
                                  train.roc.AD = train.roc.AD,
                                  valid.roc.CN = valid.roc.CN,
                                  valid.roc.MCI = valid.roc.MCI,
                                  valid.roc.AD = valid.roc.AD)
  }
  return(res_out)
}


## --------------
# 2way Module Function
## --------------
my_2way.mods = function(two_way_modlist) {
  p = progressor(along = two_way_modlist)
  foreach (i = two_way_modlist) %dopar% {
    ## --- Run GLMNET
    set.seed(32)
    Net.fit = caret::train(x = train_grp[, module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                       module_df$colors == strsplit(i, "_")[[1]][2]
                                                     ,1]
    ],
    y = train_pheno[[covariate_interest]],
    method = "glmnet",
    trControl = cctrl1,
    metric = "Mean_Balanced_Accuracy",
    tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                           .lambda = seq(0.001, 1, 0.005)),
    family = "multinomial",
    type.multinomial = "grouped",
    maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                                    module_df$colors == strsplit(i, "_")[[1]][2]
                                                                  ,1]
    ],
    type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                                          module_df$colors == strsplit(i, "_")[[1]][2]
                                                                        ,1]
    ],
    type = "prob")
    ## --- Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    ## --- Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    ## --- Save outputs to single object then to main list for later reading
    p(sprintf("i=%s", i))
    output = data.frame(alpha = Net.fit$bestTune$alpha,
                        lambda = Net.fit$bestTune$lambda,
                        train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    training = data.frame(Prediction = Prediction,
                                                 Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid,
                                              Actual = valid_pheno[[covariate_interest]])
    
    ## --- Class outcome for split model
    #CN
    outcome.CN = coef(Net.fit$finalModel,
                                 s=Net.fit$bestTune$lambda)$CN
	outcome.CN = as.matrix(outcome.CN[outcome.CN[,1]!=0,])
	outcome.CN = as.data.frame(outcome.CN) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
	
	#MCI
	outcome.MCI = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$MCI
	outcome.MCI = as.matrix(outcome.MCI[outcome.MCI[,1]!=0,])
	outcome.MCI = as.data.frame(outcome.MCI) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	#AD
	outcome.AD = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$AD
	outcome.AD = as.matrix(outcome.AD[outcome.AD[,1]!=0,])
	outcome.AD = as.data.frame(outcome.AD) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	outcome = cbind(outcome.CN, outcome.MCI, outcome.AD)
	colnames(outcome) = c("CN", "MCI", "AD")
    
    ## --- Export Iteration Results
    twoway_class_list[[i]] = list(output = output,
    									outcome = outcome,
    									model = Net.fit,
    									training = training,
    									validation = valid,
    									train.roc.CN = train.roc.CN,	
    									train.roc.MCI = train.roc.MCI,
    									train.roc.AD = train.roc.AD,
    									valid.roc.CN = valid.roc.CN,
    									valid.roc.MCI = valid.roc.MCI,
    									valid.roc.AD = valid.roc.AD)
  }
}



EH_2way.mods = function(two_way_modlist) {
  p = progressor(along = two_way_modlist)
  foreach (i = two_way_modlist) %dopar% {
    
    
    # Run GLMNET
    set.seed(32)
    Net.fit = caret::train(x = train_grp[, c(module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                       module_df$colors == strsplit(i, "_")[[1]][2]
                                                     ,1],
                                                     selected_columns)],
    y = train_pheno[[covariate_interest]],
    method = "glmnet",
    trControl = cctrl1,
    metric = "Mean_Balanced_Accuracy",
    tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                           .lambda = seq(0.001, 1, 0.005)),
    family = "multinomial",
    type.multinomial = "grouped",
    maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[, c(module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                       module_df$colors == strsplit(i, "_")[[1]][2]
                                                     ,1],
                                                     selected_columns)],
    type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[, c(module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                       module_df$colors == strsplit(i, "_")[[1]][2]
                                                     ,1],
                                                     selected_columns)],
    type = "prob")
    # Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    # Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    # Save outputs to single object then to main list for later reading
    p(sprintf("i=%s", i))
    output = data.frame(#df = length(coef(Net.fit$finalModel, s=Net.fit$bestTune$lambda)@i)-1,
                        alpha = Net.fit$bestTune$alpha,
                        lambda = Net.fit$bestTune$lambda,
                        train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    covariates = coef(Net.fit$finalModel, s=Net.fit$bestTune$lambda)$CN
    covariates = as.matrix(covariates[covariates[,1]!=0,])
    covariates = as.data.frame(covariates) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] %>% rownames(.)
    
    training = data.frame(Prediction = Prediction,
                                                 Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid,
                                              Actual = valid_pheno[[covariate_interest]])
    
    ## --- Class outcome for split model
    #CN
    outcome.CN = coef(Net.fit$finalModel,
                                 s=Net.fit$bestTune$lambda)$CN
	outcome.CN = as.matrix(outcome.CN[outcome.CN[,1]!=0,])
	outcome.CN = as.data.frame(outcome.CN) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
	
	#MCI
	outcome.MCI = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$MCI
	outcome.MCI = as.matrix(outcome.MCI[outcome.MCI[,1]!=0,])
	outcome.MCI = as.data.frame(outcome.MCI) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	#AD
	outcome.AD = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$AD
	outcome.AD = as.matrix(outcome.AD[outcome.AD[,1]!=0,])
	outcome.AD = as.data.frame(outcome.AD) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	outcome = cbind(outcome.CN, outcome.MCI, outcome.AD)
	colnames(outcome) = c("CN", "MCI", "AD")
    
    ## --- Export Iteration Results
    twoway_class_list[[i]] = list(output = output,
    									outcome = outcome,
    									model = Net.fit,
    									training = training,
    									validation = valid,
    									train.roc.CN = train.roc.CN,	
    									train.roc.MCI = train.roc.MCI,
    									train.roc.AD = train.roc.AD,
    									valid.roc.CN = valid.roc.CN,
    									valid.roc.MCI = valid.roc.MCI,
    									valid.roc.AD = valid.roc.AD)
  }
}



## --------------
# 3way Module Function
## --------------
my_3way.mods = function(three_way_modlist) {
  p = progressor(along = three_way_modlist)
  foreach (i = three_way_modlist) %dopar% {
    
    # Run GLMNET
    set.seed(32)
    Net.fit = caret::train(x = train_grp[, module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                       module_df$colors == strsplit(i, "_")[[1]][2] |
                                                       module_df$colors == strsplit(i, "_")[[1]][3]
                                                     ,1]
    ],
    y = train_pheno[[covariate_interest]],
    method = "glmnet",
    trControl = cctrl1,
    metric = "Mean_Balanced_Accuracy",
    tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                           .lambda = seq(0.001, 1, 0.005)),
    family = "multinomial",
    type.multinomial = "grouped",
    maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[, module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                                    module_df$colors == strsplit(i, "_")[[1]][2] |
                                                                    module_df$colors == strsplit(i, "_")[[1]][3]
                                                                  ,1]
    ],
    type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[, module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                                          module_df$colors == strsplit(i, "_")[[1]][2] |
                                                                          module_df$colors == strsplit(i, "_")[[1]][3]
                                                                        ,1]
    ],
    type = "prob")
    # Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    # Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    # Save outputs to single object then to main list for later reading
    p(sprintf("i=%s", i))
    output = data.frame(#df = length(coef(Net.fit$finalModel, s=Net.fit$bestTune$lambda)@i)-1,
                        alpha = Net.fit$bestTune$alpha,
                        lambda = Net.fit$bestTune$lambda,
                        train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    training = data.frame(Prediction = Prediction,
                                                 Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid,
                                              Actual = valid_pheno[[covariate_interest]])
    
    ## --- Class outcome for split model
    #CN
    outcome.CN = coef(Net.fit$finalModel,
                                 s=Net.fit$bestTune$lambda)$CN
	outcome.CN = as.matrix(outcome.CN[outcome.CN[,1]!=0,])
	outcome.CN = as.data.frame(outcome.CN) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
	
	#MCI
	outcome.MCI = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$MCI
	outcome.MCI = as.matrix(outcome.MCI[outcome.MCI[,1]!=0,])
	outcome.MCI = as.data.frame(outcome.MCI) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	#AD
	outcome.AD = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$AD
	outcome.AD = as.matrix(outcome.AD[outcome.AD[,1]!=0,])
	outcome.AD = as.data.frame(outcome.AD) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	outcome = cbind(outcome.CN, outcome.MCI, outcome.AD)
	colnames(outcome) = c("CN", "MCI", "AD")
    
    ## --- Export Iteration Results
    threeway_class_list[[i]] = list(output = output,
    									outcome = outcome,
    									model = Net.fit,
    									training = training,
    									validation = valid,
    									train.roc.CN = train.roc.CN,	
    									train.roc.MCI = train.roc.MCI,
    									train.roc.AD = train.roc.AD,
    									valid.roc.CN = valid.roc.CN,
    									valid.roc.MCI = valid.roc.MCI,
    									valid.roc.AD = valid.roc.AD)
  }
}



EH_3way.mods = function(three_way_modlist) {
  p = progressor(along = three_way_modlist)
  foreach (i = three_way_modlist) %dopar% {
    
    # Run GLMNET
    set.seed(32)
    Net.fit = caret::train(x = train_grp[, c(module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                       module_df$colors == strsplit(i, "_")[[1]][2] |
                                                       module_df$colors == strsplit(i, "_")[[1]][3]
                                                     ,1],
                                                     selected_columns)],
    y = train_pheno[[covariate_interest]],
    method = "glmnet",
    trControl = cctrl1,
    metric = "Mean_Balanced_Accuracy",
    tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                           .lambda = seq(0.001, 1, 0.005)),
    family = "multinomial",
    type.multinomial = "grouped",
    maximize = T)
    
    Prediction = predict(Net.fit, newdata = train_grp[, c(module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                       module_df$colors == strsplit(i, "_")[[1]][2] |
                                                       module_df$colors == strsplit(i, "_")[[1]][3]
                                                     ,1],
                                                     selected_columns)],
    type = "prob")
    Prediction.valid = predict(Net.fit, newdata = valid_grp[, c(module_df[module_df$colors == strsplit(i, "_")[[1]][1] |
                                                       module_df$colors == strsplit(i, "_")[[1]][2] |
                                                       module_df$colors == strsplit(i, "_")[[1]][3]
                                                     ,1],
                                                     selected_columns)],
    type = "prob")
    # Calculate TRAINING AUC values
    train.roc.CN = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "CN", # condition
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction[, "CN"])
    
    train.roc.MCI = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "MCI", # condition
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction[, "MCI"])
    
    train.roc.AD = pROC::roc(ifelse(train_pheno[[covariate_interest]] == "AD", # condition
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction[, "AD"])
    
    
    train.AUC = mean(c(train.roc.CN$auc, train.roc.MCI$auc, train.roc.AD$auc))
    
    
    # Calculate VALIDATION AUC values
    valid.roc.CN = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "CN",
                                             "CN", # IF MATCH response
                                             "non-CN"), # ELSE response
                                      Prediction.valid[, "CN"])
    
    valid.roc.MCI = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "MCI",
                                        "MCI", # IF MATCH response
                                        "non-MCI"), # ELSE response
                                 Prediction.valid[, "MCI"])
    
    valid.roc.AD = pROC::roc(ifelse(valid_pheno[[covariate_interest]] == "AD",
                                        "AD", # IF MATCH response
                                        "non-AD"), # ELSE response
                                 Prediction.valid[, "AD"])
    
    
    valid.AUC = mean(c(valid.roc.CN$auc, valid.roc.MCI$auc, valid.roc.AD$auc))
    
    
    
    # Save outputs to single object then to main list for later reading
    p(sprintf("i=%s", i))
    output = data.frame(alpha = Net.fit$bestTune$alpha,
                        lambda = Net.fit$bestTune$lambda,
                        train.AUC = train.AUC,
                        valid.AUC = valid.AUC
    )
    covariates = coef(Net.fit$finalModel, s=Net.fit$bestTune$lambda)$CN
    covariates = as.matrix(covariates[covariates[,1]!=0,])
    covariates = as.data.frame(covariates) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] %>% rownames(.)
    
    training = data.frame(Prediction = Prediction,
                                                 Actual = train_pheno[[covariate_interest]])
    valid = data.frame(Prediction = Prediction.valid,
                                              Actual = valid_pheno[[covariate_interest]])
    
    ## --- Class outcome for split model
    #CN
    outcome.CN = coef(Net.fit$finalModel,
                                 s=Net.fit$bestTune$lambda)$CN
	outcome.CN = as.matrix(outcome.CN[outcome.CN[,1]!=0,])
	outcome.CN = as.data.frame(outcome.CN) %>%.[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)
	
	#MCI
	outcome.MCI = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$MCI
	outcome.MCI = as.matrix(outcome.MCI[outcome.MCI[,1]!=0,])
	outcome.MCI = as.data.frame(outcome.MCI) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	#AD
	outcome.AD = coef(Net.fit$finalModel,
                            s=Net.fit$bestTune$lambda)$AD
	outcome.AD = as.matrix(outcome.AD[outcome.AD[,1]!=0,])
	outcome.AD = as.data.frame(outcome.AD) %>% .[row.names(.) !="(Intercept)", ,drop = FALSE] #%>% rownames(.)

	outcome = cbind(outcome.CN, outcome.MCI, outcome.AD)
	colnames(outcome) = c("CN", "MCI", "AD")
    
    ## --- Export Iteration Results
    threeway_class_list[[i]] = list(output = output,
    									outcome = outcome,
    									model = Net.fit,
    									training = training,
    									validation = valid,
    									train.roc.CN = train.roc.CN,	
    									train.roc.MCI = train.roc.MCI,
    									train.roc.AD = train.roc.AD,
    									valid.roc.CN = valid.roc.CN,
    									valid.roc.MCI = valid.roc.MCI,
    									valid.roc.AD = valid.roc.AD)
  }
}




## --------------
# Full ML scan function
# Useful when have already narrowed down to a set of features 
## --------------

ML_suite_scan = function(modules_final, covariate_interest, id_column) {
  
  res_out_all = list()
  
  for (i in modules_final) {
    cat("\nWorking on module:", i,"\n")
    
    train_grp.B_SVM.DF = merge(as.data.frame(train_pheno[,c(id_column, covariate_interest), with = F]),
                               as.data.frame(train_grp[, module_df[module_df$colors == i, 1]]),
                               by.x = id_column, by.y = 0)
    colnames(train_grp.B_SVM.DF)[2] = "phenotype"
    
    # Bagged CART
    cat("Bagged CART\n")
    set.seed(1234)
    fit.treebag <- caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                                y = train_pheno[[covariate_interest]],
                                family = "multinomial",
                                type.multinomial = "grouped",
                                metric = "Mean_Balanced_Accuracy", maximize = T,
                                method = "treebag", trControl = cctrl1)
    
    # RF
    cat("RF\n")
    set.seed(1234)
    fit.rf <- caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                           y = train_pheno[[covariate_interest]],
                           family = "multinomial",
                           type.multinomial = "grouped",
                           metric = "Mean_Balanced_Accuracy", maximize = T,
                           method = "rf", trControl = cctrl1)
    
    # C5.0
    cat("C5.0\n")
    set.seed(1234)
    fit.c50 <- caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                            y = train_pheno[[covariate_interest]],
                            family = "multinomial",
                            type.multinomial = "grouped",
                            metric = "Mean_Balanced_Accuracy", maximize = T,
                            method = "C5.0", trControl = cctrl1)
    
    # LDA - Linear Discriminate Analysis
    cat("LDA\n")
    set.seed(1234)
    fit.lda <- caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                            y = train_pheno[[covariate_interest]],
                            family = "multinomial",
                            type.multinomial = "grouped",
                            metric = "Mean_Balanced_Accuracy", maximize = T,
                            method="lda", trControl=cctrl1)
    
    # GLMNET - Regularized Logistic Regression
    cat("ENET\n")
    set.seed(1234)
    fit.glmnet <- caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                               y = train_pheno[[covariate_interest]],
                               method = "glmnet",
                               trCN = cctrl1,
                               tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                      .lambda = seq(0.001, 1, 0.005)),
                               family = "multinomial",
                               type.multinomial = "grouped",
                               metric = "Mean_Balanced_Accuracy", maximize = T)
                               
    # KNN - k-Nearest Neighbors 
    cat("KNN\n")
    set.seed(1234)
    fit.knn <- caret::train(x = train_grp[, module_df[module_df$colors == i, 1]],
                            y = train_pheno[[covariate_interest]],
                            metric = "Mean_Balanced_Accuracy", maximize = T,
                            method="knn",
                            tuneGrid = data.frame(k = seq(11,85,by=2)),
                            trControl=cctrl1)
    
    # SVM Linear - Support Vector Machine, 
    set.seed(1234)
    cat("SVM-L\n")
    fit.svmLin <- train(phenotype~.,
                        data = train_grp.B_SVM.DF[,2:ncol(train_grp.B_SVM.DF)],
                        method="svmLinear", trControl=cctrl1,
                        metric = "Mean_Balanced_Accuracy", maximize = T,
                        tuneGrid = expand.grid(C = seq(0, 2, length = 20)))
    
    # SVM Radial - Support Vector Machine, 
    cat("SVM-R\n")
    set.seed(1234)
    fit.svmRad <- train(phenotype~.,
                        data = train_grp.B_SVM.DF[,2:ncol(train_grp.B_SVM.DF)],
                        method="svmRadial", trControl=cctrl1,
                        metric = "Mean_Balanced_Accuracy", maximize = T,
                        tuneLength = 20)
    
    
    # Colate models into single object
    ML_models = list("Bagged CART" = fit.treebag,
                     "Random Forest" = fit.rf,
                     "C5.0" = fit.c50,
                     "LDA" = fit.lda,
                     "GLMNET" = fit.glmnet,
                     "KNN" = fit.knn,
                     "Linear SVM" = fit.svmLin,
                     "Radial SVM" = fit.svmRad
    )
    rm(fit.treebag, fit.rf, fit.c50, fit.lda, fit.glmnet, fit.knn, fit.svm.Lin, fit.svmRad)
    gc()
    
    
    ## ------------------------------
    # Assess training results
    ## ------------------------------
    
    ML.Bal.Acc_training = sapply(names(ML_models), function(j){
      
      Prediction.train = predict(object = ML_models[[j]],
                                 newdata = train_grp[, module_df[module_df$colors == i, 1]])
      
      out = caret::confusionMatrix(Prediction.train,
                                   train_pheno$disease.status.abv)
      
      return(out)
    }, simplify = F, USE.NAMES = T)    
    
    
    ## ------------------------------
    # Assess validation results
    ## ------------------------------

    ML.Bal.Acc_validation = sapply(names(ML_models), function(j){
      
      Prediction.valid = predict(object = ML_models[[j]],
                                 newdata = valid_grp[, module_df[module_df$colors == i, 1]])
      
      out = caret::confusionMatrix(Prediction.valid,
                                  valid_pheno$disease.status.abv)
      
      return(out)
    }, simplify = F, USE.NAMES = T)
    
    
    res_out_all[[i]] = list(ML_models = ML_models,
                            ML.Bal.Acc_training = ML.Bal.Acc_training,
                            ML.Bal.Acc_validation = ML.Bal.Acc_validation)
                       
  }
  return(res_out_all)
}