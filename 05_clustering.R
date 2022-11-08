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

  # **sort by prob**
  # this is really important, because when doing the clustering,
  # we will first cluster transports which have the highest 'score' of
  # being in the same lorry tour
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
    # k is the threshold of clusters.
    # The line above corresponds to the equation of the linear model (a + x*b)
    # perhaps a different function than linear would fit better our data.

  # start with each nodes its own cluster
  clusters <- as.list(nodes[[1]])
  reals <- as.list(nodes[[1]])

  # for creating the real list of clusters per day
  for (row in 1:nrow(subsetD)){

    id1 <- subsetD[row,'v1_i']; id1 # semicolon and id1 is to print id1
    id2 <- subsetD[row, 'v1_j']; id2

    i_toRemoved <- which(sapply(reals, function(y) id2 %in% y)); i_toRemoved

    i_toKeep <- which(sapply(reals, function(y) id1 %in% y))

    if (i_toRemoved != i_toKeep &
        subsetD[row, 'same_tour'] == 1){
      # add id2 (and every id from it previous cluster) to
      # the cluster where id1 is
      reals[[i_toKeep]] <- append(reals[[i_toKeep]], reals[[i_toRemoved]])

      # remove cluster where id2 was from the list of clusters (here: reals)
      reals[[i_toRemoved]] <- NULL; reals
    }
  }

  # increase the number of real clusters (real_size)
  # by the number of real clusters of the day
  real_size = real_size+length(reals)

  # for creating the assumed cluster per day
  for (row in 1:nrow(subsetD)){

    id1 <- subsetD[row,'v1_i']; id1 # semicolon and id1 is to print id1
    id2 <- subsetD[row, 'v1_j']; id2

    i_toRemoved <- which(sapply(clusters, function(y) id2 %in% y)); i_toRemoved

    i_toKeep <- which(sapply(clusters, function(y) id1 %in% y))

    if (i_toRemoved != i_toKeep &
        (length(clusters[[i_toRemoved]]) + length(clusters[[i_toKeep]])) < 5 |
        (length(clusters[[i_toRemoved]])==1 | length(clusters[[i_toKeep]])==1)
        # -> 4 being the maximum number of transport which I find reasonable
        # to be part of the same tour
        # but if we only add one more transport to the tour, go ahead ;)
        ){
      # add id2 (and every id from it previous cluster) to
      # the cluster where id1 is
      clusters[[i_toKeep]] <- append(clusters[[i_toKeep]], clusters[[i_toRemoved]])

      # remove cluster where id2 was from the list of clusters
      clusters[[i_toRemoved]] <- NULL; clusters
      }

    # break when k is reached
    if (length(clusters) <= k){
      break
    }

  }

  # increase the number of predicted clusters (clus_size)
  # by the number of predicted clusters of the day
  clus_size = clus_size+length(clusters)

  # assign the value 1 in pred when the two transports were predicted
  # as belonging to the same tour (cluster)
  for (row in 1:nrow(subsetD)){

    idi <- subsetD[row,'v1_i']; id1
    idj <- subsetD[row, 'v1_j']; id2

    # look if the id (v1) are in the same index in the cluster
    if (which(sapply(clusters, function(y) idi %in% y)) == which(sapply(clusters, function(y) idj %in% y))){
      subsetD[row, 'pred'] <- 1
    }

  }

  predset <- rbind(predset, subsetD)

  # calculate Jaccard index to evaluate clustering performance
  for (i in 1:length(clusters)){
    for (j in 1:length(reals)){
      # when we find a predicted cluster and a real one that
      # share at least one transport
      # the jaccard indice is calculated
      if (length(intersect(as.vector(clusters[[i]]),as.vector(reals[[j]]))) > 0){
        j = (length(intersect(as.vector(clusters[[i]]),as.vector(reals[[j]])))) /
          (length(union(as.vector(clusters[[i]]),as.vector(reals[[j]]))))
        jaccard = jaccard + j
        counter = counter + 1
      }
    }
  }
} # end of the master for-loop which go through all days

### evaluation of the proposed methods

# mean jaccard index over all clusters from all days
jaccard_final = jaccard/counter; jaccard_final # 0.6052732

clus_real_size_ratio = clus_size/real_size; clus_real_size_ratio # 1.010613
# with this method,
# there are a little bit more predicted clusters than real ones,
# but the fact that it is close to one is pretty good

F1 <- F1_Score(as.logical(predset$same_tour), as.logical(predset$pred),
               positive = TRUE); F1 # 0.5676647

# confusionMatrix
CM_gradient_boost <- confusionMatrix(as.factor(predset$pred),
                      as.factor(as.numeric(predset$same_tour)), positive = "1"); CM_gradient_boost

table(predset$same_tour, predset$pred)
# compute model prediction accuracy rate:
mean(as.logical(predset$same_tour) == as.logical(predset$pred)) # 0.9807644
