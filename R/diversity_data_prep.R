library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)
# devtools::install("admahood/neondiversity")
library(neondiversity) 
options(stringsAsFactors = FALSE)

####################
# downloading data #
####################
sites <- c("SRER", "ONAQ", "MOAB", "JORN")
lut_sites <- c("SRER" = "Santa Rita",
               "ONAQ" = "Onaqui",
               "MOAB" = "Moab",
               "JORN" = "Jornada")
# if statement helps avoid downloading over and over
if(!file.exists("data/diversity.RDS")){
  loadByProduct(dpID = "DP1.10058.001", 
                site = sites, 
                check.size = F) -> x
  saveRDS(x, "data/diversity.RDS")}else{
x<-readRDS("data/diversity.RDS")}

#######################
# using neondiversity #
#######################

plot_level <- get_diversity_info(neon_div_object = x, scale = "plot")
sp_level_1 <- get_diversity_info(x, "1m")
sp_level_10 <- get_diversity_info(x, "10m")
sp_level_100 <- get_diversity_info(x, "100m")
all_scales <- rbind(plot_level, sp_level_1, sp_level_10, sp_level_100) %>%
  mutate(site_name = lut_sites[site])


