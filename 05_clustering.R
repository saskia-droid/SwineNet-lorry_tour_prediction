# clustering

library(dplyr)
library(PRROC)
library(MLmetrics)
library(caret)

source("03_gradient_boost.R")


# import linear model to define number of clusters
calclutnumb <- readRDS("calclutnumb.rds")

test_set$pred <- 0

test_set <- test_set[order(test_set$date),]

dates <- unique(test_set$date)

predset <- test_set[FALSE,] # create an empty data frame "predset" with same structure as test_set

jaccard=0 # initialize jaccard index
counter=0 # initialize denominator for index
clus_size=0
real_size=0

# as to be done for each dates
for (d in dates){

  subsetD <- test_set[which(test_set$date == d),]

  # sort by prob
  subsetD <- subsetD[order(subsetD$prob, decreasing = TRUE),]
  
  # get the nodes
  sources <- subsetD %>% 
    distinct(v1_i) %>% 
    rename(label = v1_i)
  
  destinations <- subsetD %>% 
    distinct(v1_j) %>% 
    rename(label = v1_j)
  
  nodes <- full_join(sources, destinations, by = "label")
  
  # number of clusters k
  n_transports <- nrow(nodes)
  k <- round(calclutnumb$coefficients[[1]] + n_transports * calclutnumb$coefficients[[2]]) + 1
    # k is the threshold of clusters. The line above corresponds to the equation of the linear model (a + x*b)
    # perhaps I could find a better function than linear. 
  
  # start with each nodes its own cluster
  clusters <- as.list(nodes[[1]])
  reals <- as.list(nodes[[1]])
  
  for (row in 1:nrow(subsetD)){
  
    id1 <- subsetD[row,'v1_i']; id1 # semicolon and id1 is to print id1
    id2 <- subsetD[row, 'v1_j']; id2
    
    i_toRemoved <- which(sapply(reals, function(y) id2 %in% y)); i_toRemoved
    
    i_toKeep <- which(sapply(reals, function(y) id1 %in% y))
    
    if (i_toRemoved != i_toKeep & 
        subsetD[row, 'same_tour'] ==1
    ){
      # find where id1 is and append id2 to it 
      reals[[i_toKeep]] <- append(reals[[i_toKeep]], reals[[i_toRemoved]])
      
      # remove id2 from clusters
      reals[[i_toRemoved]] <- NULL; reals
    }
  } 
  
    
  
  for (row in 1:nrow(subsetD)){
    
    id1 <- subsetD[row,'v1_i']; id1 # semicolon and id1 is to print id1
    id2 <- subsetD[row, 'v1_j']; id2
  
    i_toRemoved <- which(sapply(clusters, function(y) id2 %in% y)); i_toRemoved
    
    i_toKeep <- which(sapply(clusters, function(y) id1 %in% y))
    
    if (i_toRemoved != i_toKeep & 
        (length(clusters[[i_toRemoved]]) + length(clusters[[i_toKeep]])) < 5 | #& 
        (length(clusters[[i_toRemoved]])==1 | length(clusters[[i_toKeep]])==1) # THIS LINE IS NEW AND HAS TO BE TESTED
        # -> 4 being the maximum number of transport which I find reasonable 
        # to be part of the same tour
        #testset20171205[row,'same_trader_id'] == 1
        ){
    # find where id1 is and append id2 to it 
    clusters[[i_toKeep]] <- append(clusters[[i_toKeep]], clusters[[i_toRemoved]])
    
    # remove id2 from clusters
    clusters[[i_toRemoved]] <- NULL; clusters
    }
   
    # break when k is reached
    if (length(clusters) <= k){
      break
    }
    
  }
  
  # check the mean number of transports by tour
  
  for (row in 1:nrow(subsetD)){
    
    idi <- subsetD[row,'v1_i']; id1
    idj <- subsetD[row, 'v1_j']; id2
    
    # look if the id (v1) are in the same index in the cluster
    if (which(sapply(clusters, function(y) idi %in% y)) == which(sapply(clusters, function(y) idj %in% y))){
      subsetD[row, 'pred'] <- 1
    }
    
  }
  
  predset <- rbind(predset, subsetD)
  clus_size = clus_size+length(clusters)
  real_size = real_size+length(reals)
  
  # calculate Jaccard index to evaluate clustering performance
  for (i in 1:length(clusters)) {
    for (j in 1:length(reals))   {
      if (length(intersect(as.vector(clusters[[i]]),as.vector(reals[[j]]))) > 0) {
        j = (length(intersect(as.vector(clusters[[i]]),as.vector(reals[[j]])))) / (length(union(as.vector(clusters[[i]]),as.vector(reals[[j]]))))
        jaccard = jaccard + j
        counter = counter + 1
      }
    }
  }
  
}

jaccard_final = jaccard/counter
jaccard_final
clus_real_size_ratio = clus_size/real_size
clus_real_size_ratio


# check different metrics

# table(predset$same_tour, predset$pred)
# table(predset$same_tour)

# plot precision-recall curve
# PR_obj <- pr.curve(scores.class0 = predset$pred, 
#                    weights.class0 = predset$same_tour, curve = TRUE)
# plot(PR_obj)

F1 <- F1_Score(as.logical(predset$same_tour), as.logical(predset$pred), 
               positive = TRUE); F1 # 0.5150894

# Compute model prediction accuracy rate
mean(as.logical(predset$same_tour) == as.logical(predset$pred)) # 0.9766712

# confusionMatrix
library(caret)
CM_gradient_boost <- confusionMatrix(as.factor(predset$pred),
                      as.factor(as.numeric(predset$same_tour)), positive = "1"); CM_gradient_boost



reals = clusters
jaccard=0
counter=0
for (i in 1:length(clusters)) {
  for (j in 1:length(reals))   {
    if (length(intersect(as.vector(clusters[[i]]),as.vector(clusters[[j]]))) > 0) {
      j = (length(intersect(as.vector(clusters[[i]]),as.vector(clusters[[j]])))) / (length(union(as.vector(clusters[[i]]),as.vector(clusters[[j]]))))
      jaccard = jaccard + j
      counter = counter + 1
    }
  }
}


clusters <- as.list(nodes[[1]])
reals <- as.list(nodes[[1]])


for (row in 1:nrow(subsetD)){
  
  id1 <- subsetD[row,'v1_i']; id1 # semicolon and id1 is to print id1
  id2 <- subsetD[row, 'v1_j']; id2
  
  i_toRemoved <- which(sapply(reals, function(y) id2 %in% y)); i_toRemoved
  
  i_toKeep <- which(sapply(reals, function(y) id1 %in% y))
  
  if (i_toRemoved != i_toKeep & 
     subsetD[row, 'same_tour'] ==1
  ){
    # find where id1 is and append id2 to it 
    reals[[i_toKeep]] <- append(reals[[i_toKeep]], reals[[i_toRemoved]])
    
    # remove id2 from clusters
    reals[[i_toRemoved]] <- NULL; reals
  }
  
}




