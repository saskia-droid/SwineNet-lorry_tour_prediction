
# create the comparative links between transports of same day
# Saskia Perret-Gentil

# install packages if not already install
list.of.packages <- c("dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library(dplyr)

## some functions

# same function
same <- function(x, y){
  ans = (x == y)

  if (any(is.na(ans))){
    ans <- FALSE
  }

  return (ans)
}

relDiff <- function(val1, val2){
  if (val1 == 0 & val2 == 0){
    return (0)
  }
  else{
    return ((abs(val1 - val2))/max(abs(val1), abs(val2)))
  }
}

pointDistance <- function(p1, p2){
  # p: c(x, y)
  return (sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2))
}

#TVD <- read.csv("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/DatasetsAnalysis/Tour_prediction/fg/data_exploration/train_tvd_data_gps_pigcat_suisagtransports_agis_traders.csv")
# import TVD that matched with traders data
TVD <- read.csv("train_tvd_data_gps_pigcat_suisagtransports_agis_traders.csv", sep=",")
TVD <- as.data.frame(TVD)

# keep only transports of 2017
#TVD <- TVD[TVD$year == 2017, ]

# transports of 2018 and 2019
#TVD <- TVD[(TVD$year == 2017 | TVD$year == 2018 | TVD$year == 2019), ]

# select columns of interest
TVD <- subset(TVD, select = c(v1, # id specific of the transport
                              id_anh_source,
			                        id_anh_dest,
			                        event_date,
                              tourid,
                              combined_trader_id,
                              pig_type,
                              n_pigs,
                              previous_occurrence,
                              longis_est,
                              latis_est,
                              longid_est,
                              latid_est,
                              dist_km,
			                        notif_type,
			                        farm_type_source,
			                        farm_type_dest))

# date variable - change factor to date
TVD$event_date <- as.Date(TVD$event_date, format = "%Y-%m-%d")

# create t_freq from previous_occurence
TVD <- mutate(TVD, t_freq = 1/previous_occurrence)
TVD$t_freq[is.na(TVD$t_freq)] <- 0

col_names <- c("date",
               "v1_i",
               "v1_j",
               "pig_type_i",
               "pig_type_j",
               "farm_type_source_i",
               "farm_type_source_j",
               "farm_type_dest_i",
               "farm_type_dest_j",
               "n_pigs_i",
               "n_pigs_j",
               "dist_km_i",
               "dist_km_j",
               "t_freq_i",
               "t_freq_j",

               "same_trader_id",
               #"same_pig_type",
               "same_tour",
               #"same_notif_type",

               "relDiff_n_pigs",
               "relDiff_t_freq",
               "relDiff_dist_km",

               "dist_ss",
               "dist_sd",
               "dist_ds",
               "dist_dd"
)

# create empty dataframe for links
links <- data.frame(matrix(ncol = length(col_names), nrow = 1))

colnames(links) <- col_names

TVD <- arrange(TVD, event_date)

# take only the first 500 rows for testing
#TVD <- TVD[1:100,]

dates <- seq(min(TVD$event_date), max(TVD$event_date), by = 1)

# todo: ask F.: we go from 14 to 14 jours?

n = Sys.getenv('SLURM_ARRAY_TASK_ID')
n = as.numeric(n)
m = n+13
file_name = paste0('links_', n, '.csv')

dates  <- seq(dates[n],dates[m], by = 1)

for (d in dates){
  same_date_indices <- which(TVD$event_date == d)
  n_t_d = length(same_date_indices) # number of transport on the day
  if (n_t_d > 1){ # to have at least 2 transports to compare
    for(i in 1:(n_t_d-1)){
      start = i+1
      for (j in start:n_t_d){
        row1 = TVD[same_date_indices[i], ]
        row2 = TVD[same_date_indices[j], ]
        new_row = list(d,
                       row1$v1,
                       row2$v1,
                       row1$pig_type,
                       row2$pig_type,
                       row1$farm_type_source,
                       row2$farm_type_source,
                       row1$farm_type_dest,
                       row2$farm_type_dest,
                       row1$n_pigs,
                       row2$n_pigs,
                       row1$dist_km,
                       row2$dist_km,
                       row1$t_freq,
                       row2$t_freq,

                       same(row1$combined_trader_id, row2$combined_trader_id),
                       #same(row1$pig_type, row2$pig_type),
                       same(row1$tourid, row2$tourid),
                       #same(row1$notif_type, row2$notif_type),

                       relDiff(row1$n_pigs, row2$n_pigs),
                       relDiff(row1$t_freq, row2$t_freq),
                       relDiff(row1$dist_km, row2$dist_km),

                       pointDistance(c(row1$longis_est, row1$latis_est),
                                     c(row2$longis_est, row2$latis_est)),

                       pointDistance(c(row1$longis_est, row1$latis_est),
                                     c(row2$longid_est, row2$latid_est)),

                       pointDistance(c(row1$longid_est, row1$latid_est),
                                     c(row2$longis_est, row2$latis_est)),

                       pointDistance(c(row1$longid_est, row1$latid_est),
                                     c(row2$longid_est, row2$latid_est))
        )
        links <- rbind(links, unlist(new_row))
      }
    }
  }

  #links$route_similarity[links$route_similarity == 0] <- Inf
}

# remove NA
links <- links[complete.cases(links),]

write.csv(links, file = file_name, row.names = FALSE)