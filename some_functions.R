
# some helper functions

# partition function which do a train/test split, returning a list of two dataframes
train_test_split <- function(dataset, train_size){
  set.seed(7)
  n <- dim(dataset)[1]
  train_ind <- runif(n) < train_size
  test_set <- dataset[!train_ind, ]
  train_set <- dataset[train_ind, ]
  return(list("test" = test_set, "train" = train_set))
}

# drop function to create a balanced dataset
# example: balanced_dataset <- drop_for_bal(pig_transports, 0.9, "c_name", 'Mastjager')
# example: balanced = drop_for_bal(train_set,0.95,"same_tour",0)
drop_for_bal <- function(dataset, amount, variable, value){
  index_type = which(dataset[[variable]] == value)
  set.seed(42)
  index_to_remove = sample(index_type, (length(index_type)*amount))
  return(dataset[-index_to_remove, ])
}

# helper function in partitioning dataset procedure using dates 
outersect <- function(x, y) {
  sort(c(x[!x%in%y],
         y[!y%in%x]))
}


