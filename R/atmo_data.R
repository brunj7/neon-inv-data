library(neonUtilities)
# library(sf)
library(tidyverse)


## Constants ----
options(stringsAsFactors = FALSE)
<<<<<<< HEAD
dir.create(path_out, showWarnings = FALSE)
=======
path_out <- "Data_merged"
dir.create(path_out, showWarnings = FALSE)
file_out_charact <- "soil_charact_chem_phys.csv"
>>>>>>> f152fc0b2bac989ba3f0433668bf170dffac2f6e

neon_sites <- c("SRER", "ONAQ", "MOAB", "JORN")



## Products of interest ----
<<<<<<< HEAD
# some of these take a while
# Bulk precipitation
parameters <- c(precip = "DP1.00006.001", # bulk precitpitation
                pressure =  "DP1.00004.001", # barometric pressure
                wind = "DP1.00001.001",  #2D wind speed and direction
                air_temp = "DP1.00002.001", # single aspirated air temp
                rel_hum = "DP1.00098.001", # relative humidity
                biological_temp = "DP1.00005.001", #IR Biological temperature
                sum_stats = "DP4.00001.001",  # Summary weather statistics
                bulk_eddy = "DP4.00200.001" #eddy covariance bundle
)

# .... too much data
for(i in 1:length(parameters)){
 loadByProduct(dpID = parameters[i], site = neon_sites, 
                check.size = F) %>%
    saveRDS(paste0("data/", names(parameters[i]), "_object.RDS"))
  gc()
  
}
=======
# Bulk precipitation
Parameters <- c(precip <- "DP1.00006.001", # bulk precitpitation
                pressure <-  "DP1.00004.001", # barometric pressure
                wind <- "DP1.00001.001",  #2D wind speed and direction
                air_temp <- "DP1.00002.001", # single aspirated air temp
                rel_hum <- "DP1.00098.001", # relative humidity
                biological_temp <- "DP1.00005.001", #IR Biological temperature
                sum_stats <- "DP4.00001.001",  # Summary weather statistics
                bulk_eddy <- "DP4.00200.001" #eddy covariance bundle
)
                
                
                  

bundled <- loadByProduct(dpID = "DP4.00200.001", site = neon_sites, 
                                   check.size = F)

>>>>>>> f152fc0b2bac989ba3f0433668bf170dffac2f6e

