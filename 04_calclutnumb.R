
# create linear model for defining number of clusters for each day

# install packages if not already install
list.of.packages <- c("dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

# libraries
library(dplyr)

# import TVD that matched with traders data
TVD <- read.csv("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/DatasetsAnalysis/Tour_prediction/fg/data_exploration/train_tvd_data_gps_pigcat_suisagtransports_agis_traders.csv", sep=",")
TVD <- as.data.frame(TVD)

# select columns of interest
TVD <- subset(TVD, select = c(event_date, tourid))

# date variable - change factor to date
TVD$event_date <- as.Date(TVD$event_date, format = "%Y-%m-%d")
TVD <- arrange(TVD, event_date)

TVD %>% count(event_date, tourid) %>% summary() # max: 19

res <- TVD %>% group_by(event_date) %>% summarise(n_T = n_distinct(tourid), n_transport = n())

plot(res$n_transport, res$n_T, xlab = "number of transports", ylab = "number of tours")
lm01 <- lm(n_T ~ n_transport, data = res)
abline(lm01, col = "blue")

summary(lm01)

saveRDS(lm01, file = "./calclutnumb.rds")
