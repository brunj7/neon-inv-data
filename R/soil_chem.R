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

# Write the merged data to disk
dir.create(path_out, showWarnings = FALSE)
write_csv(soil_charact_chem_phys, file.path(path_out, file_out_charact))

## nitrogen and soil moisture data ----

nitro_sms <- soil_chem_nitro$ntr_externalLab %>%
  dplyr::select(plotID, date_n = collectDate, kclAmmoniumNConc, 
                kclNitrateNitriteNConc) %>%
  left_join(soil_chem_nitro$sls_soilMoisture %>%
              dplyr::select(plotID, date_sms = collectDate, 
                            soilMoisture, dryMassFraction)) %>%
  mutate(date_n = str_sub(date_n,1,10) %>% as.Date(),
         date_sms = str_sub(date_sms,1,10) %>% as.Date());glimpse(nitro_sms)

ggplot(nitro_sms, aes(x=date_sms, y=soilMoisture)) +
  geom_point() +
  facet_wrap(~plotID)

soil_moisture <- soil_physical_periodic$sls_soilMoisture %>%
  dplyr::select(soilMoisture, collectDate, plotID) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date());glimpse(soil_moisture)

ggplot(soil_moisture, aes(x=collectDate, y=soilMoisture)) +
  geom_point() +
  facet_wrap(~plotID)
# note, 
## Joining perodic data ----


# => To be figured out




