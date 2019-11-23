library(neonUtilities)
# library(sf)
library(tidyverse)
library(ggpubr)
library(vegan)



## Constants ----
path_out <- "Data_merged"
file_out_charact <- "soil_charact_chem_phys.csv"

neon_sites <- c("SRER", "ONAQ", "MOAB")

## Data import ----

# soil chemical properties (Distributed initial characterization)
soil_chem_charact <- loadByProduct(dpID = "DP1.10008.001", site = neon_sites, 
              check.size = F)

# Soil physical properties (Distributed initial characterization)
soil_physical <- loadByProduct(dpID = "DP1.10047.001", site = neon_sites, 
                               check.size = F)

# Soil chemical properties (Distributed periodic)
soil_chem_periodic <- loadByProduct(dpID = "DP1.10078.001", site = neon_sites, 
                                check.size = F)

# Soil inorganic nitrogen pools and transformations
soil_chem_nitro <- loadByProduct(dpID = "DP1.10080.001", site = neon_sites, 
                                    check.size = F)
  

# Soil physical properties (Distributed periodic)
soil_physical_periodic <- loadByProduct(dpID = "DP1.10086.001", site = neon_sites, 
                               check.size = F)



## check content we got back from the API -----

# names(soil_chem_charact)
# # [1] "spc_biogeochem" "validation"     "variables"  
# 
# names(soil_chem_periodic)
# # [1] "sls_soilChemistry" "validation"        "variables"  
# 
# names(soil_physical)
# # [1] "spc_bulkdensity"  "spc_particlesize" "spc_perhorizon"   "spc_perplot"      "validation"       "variables"   
# 
# names(soil_physical_periodic)
# # [1] "sls_bgcSubsampling"      "sls_metagenomicsPooling" "sls_soilCoreCollection"  "sls_soilMoisture"        "sls_soilpH"              "validation"             
# # [7] "variables"



# # Looking at the spatial extent
# plot_sf <- soil_physical$spc_perplot %>% st_as_sf(., coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
# plot(st_geometry(plot_sf), axes = TRUE)



## Joining characterization data ----

soil_charact_chem_phys <- soil_chem_charact$spc_biogeochem %>% 
  full_join(soil_physical$spc_bulkdensity, 
            by=c("namedLocation","domainID","siteID", "plotID", "horizonID", "horizonName")) %>%
  full_join(soil_physical$spc_particlesize, 
            by=c("namedLocation","domainID","siteID", "plotID", "horizonID", "horizonName"))

#looks like single dates of samples at each horizon
ggplot(soil_chem_charact$spc_biogeochem, aes(x=collectDate, y=ctonRatio,
                                             color = horizonName)) +
  geom_point() +
  facet_wrap(~plotID, scales="free_y")

# maybe taking only the top layer is a good idea?
ggplot(soil_physical$spc_particlesize %>% filter(biogeoTopDepth == 0),
       aes(x=collectDate, y=sandTotal,
                                           color = horizonName)) +
  geom_point() +
  facet_wrap(~plotID)

# Write the merged data to disk
dir.create(path_out, showWarnings = FALSE)
write_csv(soil_charact_chem_phys, file.path(path_out, file_out_charact))

## nitrogen and soil moisture data ----
# basically mineral N and soil moisture

nitro <- soil_chem_nitro$ntr_externalLab %>%
  dplyr::select(plotID, collectDate, kclAmmoniumNConc, 
                kclNitrateNitriteNConc) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date(),
         kclAmmoniumNConc = ifelse(is.na(kclAmmoniumNConc), 0,kclAmmoniumNConc),
         kclNitrateNitriteNConc = ifelse(is.na(kclNitrateNitriteNConc), 0,
                                         kclNitrateNitriteNConc),
         mineral_n_total = kclNitrateNitriteNConc +kclAmmoniumNConc) %>%
  dplyr::select(plotID, collectDate, mineral_n_total)

sms <- soil_chem_nitro$sls_soilMoisture %>%
  dplyr::select(plotID, collectDate, 
                soilMoisture) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date())

ggplot() +
  geom_point(data = nitro %>% filter(plotID != ""), 
             aes(x = collectDate, y=mineral_n_total), 
             color = "red", shape = 21) +
  geom_point(data = sms, aes(x=collectDate, y=soilMoisture),
             color = "blue", shape = 22) +
  facet_wrap(~plotID, scales = "free_y")

## soil physical periodic ----
# seems like soil moisture and ph are the main pieces of info here

soil_moisture <- soil_physical_periodic$sls_soilMoisture %>%
  dplyr::select(soilMoisture, collectDate, plotID) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date());glimpse(soil_moisture)

soil_ph <- soil_physical_periodic$sls_soilpH %>%
  dplyr::select(plotID, collectDate, soilInWaterpH, soilInCaClpH) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date());glimpse(soil_ph)


ggplot(soil_moisture, aes(x=collectDate, y=soilMoisture)) +
  geom_boxplot(aes(group = collectDate)) +
  facet_wrap(~plotID, scales = "free_y")

ggplot(soil_ph, aes(x=collectDate)) +
  geom_boxplot(aes(y= soilInWaterpH,group = collectDate)) +
  facet_wrap(~plotID)

ggplot(soil_ph, aes(x=collectDate)) +
  geom_boxplot(aes(y= soilInCaClpH,group = collectDate)) +
  facet_wrap(~plotID)

ggplot(soil_ph, aes(x=soilInWaterpH, y=soilInCaClpH)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  annotate("text", label = "1:1 line", x=9.2,y=9)


## soil chem periodic ---
# they did a great job of messing up this table luckily it's not too hard to fix

soil_n <- soil_chem_periodic$sls_soilChemistry %>%
  dplyr::select(plotID, collectDate, nitrogenPercent, cnSampleID) %>%
  na.omit() %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date())

soil_c <- soil_chem_periodic$sls_soilChemistry %>%
  dplyr::select(plotID, collectDate, organicCPercent,cnSampleID) %>%
  na.omit()%>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date())

soil_cn <- left_join(soil_n, soil_c, 
                     by = c("plotID", "collectDate", "cnSampleID")) %>%
  mutate(soil_cn = organicCPercent/nitrogenPercent)

ggplot(soil_cn) +
  geom_point(aes(x=collectDate, y=soil_cn),
             color = "red", shape = 21)+
  facet_wrap(~plotID)

ggplot(soil_cn) +
  geom_point(aes(x=collectDate, y=nitrogenPercent)) 

ggplot(soil_cn) +
  geom_point(aes(x=collectDate, y=organicCPercent))
# note, 
## Joining perodic data ----


# => To be figured out




