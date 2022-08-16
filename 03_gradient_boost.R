
# advanced gradient boosting (xgboost)

# install packages if not already install
list.of.packages <- c("xgboost", "MLmetrics", "PRROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library(xgboost)
library(MLmetrics)
library(PRROC)

setwd("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/DatasetsAnalysis/Tour_prediction/fg")

source("02_links_preprocess.R")

# Jaccard = 0.61
keep <- c("pig_type_i", "pig_type_j", "farm_type_dest_i", 
          "farm_type_source_i", "farm_type_source_j", "farm_type_dest_j", 
          "n_pigs_i", "n_pigs_j", "dist_km_i", "dist_km_j", "t_freq_i", 
          "t_freq_j", "same_trader_id", "dist_ss", "dist_sd", "dist_ds", 
          "dist_dd", "month") 

# Worse
#keep <- c("same_trader_id", "dist_dd", "farm_type_dest_i", "dist_ss", "farm_type_dest_j", "dist_sd", "dist_ds",
#         "pig_type_i", "pig_type_j", "relDiff_n_pigs", "relDiff_t_freq", "relDiff_dist_km")

dtrain <- xgb.DMatrix(data = data.matrix(subset(train_set,
                                                select = keep)),
                      label = as.numeric(train_set$same_tour))
dtest <- xgb.DMatrix(data = data.matrix(subset(test_set,
                                               select = keep)),
                     label = as.numeric(test_set$same_tour))

watchlist <- list(train = dtrain, test = dtest)

m01 <- xgb.train(data = dtrain,
                 max.depth = 7, # max. number of splits per tree
                 nrounds = 40, # max. n. of trees (where each following tree is an improved tree for those data that were not predicted correctly)
                 eval.metric = "aucpr",
                 eval.metric = "logloss",
                 #eval.metric = "error",
                 objective = "binary:logistic",
                 watchlist = watchlist,
                 )

summary(m01)


# Make predictions on the test data
test_set$prob <- predict(m01, dtest)

# Compute model prediction accuracy rate
mean(as.logical(test_set$same_tour) == (test_set$prob > 0.5)) # 0.979501

# confusionMatrix
table(as.logical(test_set$same_tour), test_set$prob > 0.5)
table(as.logical(test_set$same_tour), test_set$prob > 0.25)
table(as.logical(test_set$same_tour), test_set$prob > 0.75)
table(as.logical(test_set$same_tour), test_set$prob > 0.95)


F1 <- F1_Score(as.logical(test_set$same_tour), test_set$prob > 0.5,
               positive = TRUE); F1 # 0.4950959
F1 <- F1_Score(as.logical(test_set$same_tour), test_set$prob > 0.25,
               positive = TRUE); F1 # 0.5546891
F1 <- F1_Score(as.logical(test_set$same_tour), test_set$prob > 0.75,
               positive = TRUE); F1 # 0.4950959
# get.max_f1(test_set$prob,test_set$same_tour)
# get.max_missrate(test_set$prob,test_set$same_tour) # minimize false negative
# get.max_precision(test_set$prob,test_set$same_tour) # maximize precision (PPV)



# plot precision-recall curve
# PR_obj <- pr.curve(scores.class0 = test_set$prob,
#                    weights.class0 = test_set$same_tour, curve = TRUE)
# plot(PR_obj)

#confusion matrix
factor_data  <- as.factor(as.numeric((test_set$same_tour)))
factor_pred  <- ifelse(test_set$prob<0.91,0,1)
#factor_pred   <- as.factor(test_set$prob)
factor_pred  <- as.factor(factor_pred)
confusionMatrix(factor_pred,factor_data, positive = "1")

imp = xgb.importance(model = m01)
xgb.plot.importance(imp)


# get.max_f1 <- function(preds, labels) {
#   
#   DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
#   cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
#   nump <- sum(labels)
#   numn <- length(labels) - nump
#   
#   DT[, fp_v := cumsum(y_true == 1)]
#   DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
#   DT[, tp_v := nump - fp_v]
#   DT <- DT[cleaner, ]
#   DT[, f1s := 2 * tp_v / (2 * tp_v + fp_v + fn_v)]
#   
#   best_row <- which.max(DT$f1s)
#   
#   if (length(best_row) > 0) {
#     return(c(DT$f1s[best_row[1]], DT$y_prob[best_row[1]]))
#   } else {
#     return(c(-1, -1))
#   }
#   
# }
# 
# 
# m02 <- xgb.train(data = dtrain,
#                  max.depth = 10, # max. number of splits per tree
#                  nrounds = 30, # max. n. of trees (where each following tree is an improved tree for those data that were not predicted correctly)
#                  eval.metric = get.max_f1(test_set$prob,test_set$same_tour),
#                  #eval.metric = "error",
#                  objective = "binary:logistic",
#                  watchlist = watchlist
# )
# 
# 
# 
# get.max_missrate <- function(preds, labels) {
#   
#   DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
#   cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
#   nump <- sum(labels)
#   numn <- length(labels) - nump
#   
#   DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
#   DT[, tp_v := nump - cumsum(y_true == 1)]
#   DT <- DT[cleaner, ]
#   DT[, miss := fn_v / (tp_v + fn_v)]
#   
#   best_row <- which.min(DT$miss)
#   
#   if (length(best_row) > 0) {
#     return(c(DT$miss[best_row[1]], DT$y_prob[best_row[1]]))
#   } else {
#     return(c(-1, -1))
#   }
#   
# }
# 
# 
# get.max_precision <- function(preds, labels) {
#   
#   DT <- data.table(y_true = labels, y_prob = preds, key = "y_prob")
#   cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
#   nump <- sum(labels)
#   
#   DT[, fp_v := cumsum(y_true == 1)]
#   DT[, tp_v := nump - fp_v]
#   DT <- DT[cleaner, ]
#   DT[, prec := tp_v / (tp_v + fp_v)]
#   
#   best_row <- which.max(DT$prec)
#   
#   if (length(best_row) > 0) {
#     return(c(DT$prec[best_row[1]], DT$y_prob[best_row[1]]))
#   } else {
#     return(c(-1, -1))
#   }
#   
# }
# 
# 
# 
# 
# 
# 
# xgb_grid_1 = expand.grid(
#   nrounds = 30,
#   eta = c(0.01, 0.001, 0.0001),
#   max_depth = c(2, 4, 6, 8, 10),
#   gamma = 1,
#   colsample_bytree = c(.6, .8),
#   min_child_weight = c(1),
#   subsample = seq(.5, 1, by=1)
# )
# 
# xgb_trcontrol_1 = trainControl(
#   method = "cv",
#   number = 5,
#   verboseIter = TRUE,
#   returnData = TRUE,
#   returnResamp = "all",                                                        # save losses across all models
#   classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
#   summaryFunction = defaultSummary,
#   allowParallel = TRUE, 
#   savePredictions = "final",
# )
# 
# same_tour = as.numeric(train_set$same_tour)
# # this model doesn't work yet. I get weird probabilities!
# m02 = train(
#   x = as.matrix(subset(train_set,
#                        select = -c(date, v1_i, v1_j,
#                                    same_tour))),
#   y = same_tour,
#   trControl = xgb_trcontrol_1,
#   tuneGrid = xgb_grid_1,
#   method = "xgbTree"
# )
# 
# 
# ggplot(m02$results, aes(x = as.factor(eta), y = max_depth, size = MAE, color = MAE)) +
#   geom_point() +
#   theme_bw() +
#   scale_size_continuous(guide = "none")
# 
# test_set$prob <- NULL
# test_set$prob <- predict(m02, test_set) # this doesn't work, I get weird probabilities!
# factor_data  <- as.factor(test_set$same_tour)
# factor_pred  <- ifelse(test_set$prob.tour<0.51,0,1)
# factor_pred  <- as.factor(factor_pred)
# confusionMatrix(factor_pred,factor_data, positive = "1")
