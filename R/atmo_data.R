library(neonUtilities)
# library(sf)
library(tidyverse)


## Constants ----
options(stringsAsFactors = FALSE)
dir.create(path_out, showWarnings = FALSE)
path_out <- "Data_merged"
dir.create(path_out, showWarnings = FALSE)
file_out_charact <- "soil_charact_chem_phys.csv"

neon_sites <- c("SRER", "ONAQ", "MOAB", "JORN")



## Products of interest ----
# some of these take a while

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


