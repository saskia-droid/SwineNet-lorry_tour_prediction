
# playing with graphs
# Saskia Perret-Gentil

# install packages if not already install
list.of.packages <- c("tidyverse", "network")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

# libraries
library(tidyverse)
library(network)

source("05_clustering.R")

dates <- unique(predset$date)
dates_5 <- sample(dates, 5, replace = FALSE)
dates_5 <- sort(dates_5)

testset_d1 <- predset[which(predset$date == dates_5[1]),]
table(testset_d1$same_tour)
table(testset_d1$pred)
#testset_d1 <- subset(testset_d1, (same_tour == 1 | pred == 1))

testset_d2 <- predset[which(predset$date == dates_5[2]),]
table(testset_d2$same_tour)
table(testset_d2$pred)
testset_d2 <- subset(testset_d2, (same_tour == 1 | pred == 1))

testset_d3 <- predset[which(predset$date == dates_5[3]),]
table(testset_d3$same_tour)
table(testset_d3$pred)
#testset_d3 <- subset(testset_d3, (same_tour == 1 | pred == 1))

testset_d4 <- predset[which(predset$date == dates_5[4]),]
table(testset_d4$same_tour)
table(testset_d4$pred)
testset_d4 <- subset(testset_d4, (same_tour == 1 | pred == 1))

testset_d5 <- predset[which(predset$date == dates_5[5]),]
table(testset_d5$same_tour)
table(testset_d5$pred)
#testset_d5 <- subset(testset_d5, (same_tour == 1 | pred == 1))

as.Date(unique(testset_d4$date), origin = "1970-01-01")
# for d4 - 2017-12-05

testset_d4$same_tour <- as.logical(testset_d4$same_tour)
testset_d4$pred <- as.logical(testset_d4$pred)

testset_d4 <- testset_d4 %>% select(v1_i, v1_j, same_tour, pred) %>% 
  mutate(link = case_when(same_tour & pred ~ 1, 
                          !same_tour & pred ~ 2, 
                          same_tour & !pred ~ 3))

sources <- testset_d4 %>% 
  distinct(v1_i) %>% 
  rename(label = v1_i)

destinations <- testset_d4 %>% 
  distinct(v1_j) %>% 
  rename(label = v1_j)

nodes <- full_join(sources, destinations, by = "label")
nodes <- nodes %>% rowid_to_column("id")

edges <- testset_d4 %>% 
  left_join(nodes, by = c("v1_i" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("v1_j" = "label")) %>%
  rename(to = id)

edges <- select(edges, from, to, link)

links_net <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", 
                     directed = FALSE, ignore.eval = FALSE)

plot(links_net, usearrows = FALSE, vertex.col = "grey", edge.col = "link", #edge.label = "link", 
     main = "2017-12-05"
     )

