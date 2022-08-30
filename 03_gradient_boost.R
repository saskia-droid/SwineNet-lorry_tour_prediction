
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

keep <- c("pig_type_i", "pig_type_j", "farm_type_dest_i", 
          "farm_type_source_i", "farm_type_source_j", "farm_type_dest_j", 
          "n_pigs_i", "n_pigs_j", "dist_km_i", "dist_km_j", "t_freq_i", 
          "t_freq_j", "same_trader_id", "dist_ss", "dist_sd", "dist_ds", 
          "dist_dd", "month") 

dtrain <- xgb.DMatrix(data = data.matrix(subset(train_set,
                                                select = keep)),
                      label = as.numeric(train_set$same_tour))
dtest <- xgb.DMatrix(data = data.matrix(subset(test_set,
                                               select = keep)),
                     label = as.numeric(test_set$same_tour))

watchlist <- list(train = dtrain, test = dtest)

m01 <- xgb.train(data = dtrain,
                 max.depth = 7, # max. number of splits per tree
                 nrounds = 40, 
                 # max. n. of trees (where each following tree is an improved 
                 # tree for those data that were not predicted correctly)
                 eval.metric = "aucpr",
                 eval.metric = "logloss",
                 objective = "binary:logistic",
                 watchlist = watchlist,
                 )

summary(m01)

# make predictions from testset 
# result in a "probability" 
test_set$prob <- predict(m01, dtest)

### testing different threshold for the "probability" 

# compute model prediction accuracy rate
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

#confusion matrix
factor_data  <- as.factor(as.numeric((test_set$same_tour))) # ? 
factor_pred  <- ifelse(test_set$prob<0.91,0,1)
factor_pred  <- as.factor(factor_pred)
confusionMatrix(factor_pred,factor_data, positive = "1")

# variable importance plot
imp = xgb.importance(model = m01)
xgb.plot.importance(imp)


