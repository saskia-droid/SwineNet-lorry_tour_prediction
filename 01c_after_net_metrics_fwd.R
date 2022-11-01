
list.of.packages <- c("dplyr","rio")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

# libraries
library(dplyr)
library(rio)

setwd("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/DatasetsAnalysis/Tour_prediction/fg/SwineNet-lorry_tour_prediction-files")
files <- list.files(path="link_files", pattern="links*", full.names=TRUE, recursive=FALSE)
list  <- lapply(files, function(x) {
  t <- import(x) # load file
})
links_2014_2019  <- do.call(rbind, list)
saveRDS(links_2014_2019, file = "links.rds")
links_2014_2019 <- NULL
