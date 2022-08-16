
# links pre-processing
# Saskia Perret-Gentil

# install packages if not already install
list.of.packages <- c("dplyr","data.table", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

# libraries
library(dplyr)
library(data.table)
library(lubridate)

source("some_functions.R")

set.seed(42)

# load links data
links <- readRDS("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/DatasetsAnalysis/Tour_prediction/fg/links.rds")

# put same_tour at the end of dataframe
links <- links %>% select(-same_tour, same_tour)

# sort by date
links <- links[order(links$date),] 

min_date <- min(links$date); min_date # 16072
as.Date(min_date, origin="1970-01-01") # "2014-01-02"
max_date <- max(links$date); max_date # 18260
as.Date(max_date, origin="1970-01-01") # "2019-12-24"

# adding month variable
links$month = as.factor(month(as.Date(links$date, origin="1970-01-01")))

dates <- seq(from = min_date, to = max_date, by = 1)

# sample 80% of the dates for the training set,  
# the remaining dates will be for testing 
draw_train <- sample(dates,  round(length(dates)*0.8)) 
draw_test  <- outersect(dates,draw_train)

train_set <- links[which(links$date %in% draw_train),]
test_set <- links[which(links$date %in% draw_test),]

# check if we have enough same_tour in test!
table(test_set$same_tour)
table(train_set$same_tour)

# balance train set - maybe not necessary
train_set <- drop_for_bal(train_set, 0.95, "same_tour", 0) #0.977
table(train_set$same_tour)


