library(neonUtilities)
# library(sf)
library(tidyverse)
library(ggpubr)
#library(vegan)

## adam here - I added a bunch of stuff and also data visualization just to kind
## of understand what the data looks like as we work through the initial 
## stages of data wrangling.


## Constants ----
options(stringsAsFactors = FALSE)
path_out <- "Data_merged"
dir.create(path_out, showWarnings = FALSE)
file_out_charact <- "soil_charact_chem_phys.csv"

neon_sites <- c("SRER", "ONAQ", "MOAB", "JORN")

## Data import ----

rdss<- c("data/soil_chem_charact.RDS",
         "data/soil_physical.RDS",
         "data/soil_chem_periodic.RDS",
         "data/soil_chem_nitro.RDS",
         "data/soil_physical_periodic.RDS")

# soil chemical properties (Distributed initial characterization)
if(!file.exists(rdss[1])){
  soil_chem_charact <- loadByProduct(dpID = "DP1.10008.001", site = neon_sites, 
                check.size = F)
  saveRDS(soil_chem_charact, rdss[1])
  }else{soil_chem_charact<-readRDS(rdss[1])}

# Soil physical properties (Distributed initial characterization)
if(!file.exists(rdss[2])){
  soil_physical <- loadByProduct(dpID = "DP1.10047.001", site = neon_sites, 
                                 check.size = F)
  saveRDS(soil_physical, rdss[2])
}else{soil_physical <- readRDS(rdss[2])}

# Soil chemical properties (Distributed periodic)
if(!file.exists(rdss[3])){
  soil_chem_periodic <- loadByProduct(dpID = "DP1.10078.001", site = neon_sites, 
                                  check.size = F)
  saveRDS(soil_chem_periodic, rdss[3])
}else{soil_chem_periodic<-readRDS(rdss[3])}

# Soil inorganic nitrogen pools and transformations
if(!file.exists(rdss[4])){
  soil_chem_nitro <- loadByProduct(dpID = "DP1.10080.001", site = neon_sites, 
                                      check.size = F)
  saveRDS(soil_chem_nitro, rdss[4])
}else{soil_chem_nitro <- readRDS(rdss[4])}

# Soil physical properties (Distributed periodic)
if(!file.exists(rdss[5])){
  soil_physical_periodic <- loadByProduct(dpID = "DP1.10086.001", site = neon_sites, 
                                 check.size = F)
  saveRDS(soil_physical_periodic, rdss[5])
}else{soil_physical_periodic <- readRDS(rdss[5])}


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
# I put these in long form -  the sample dates were different, so joining by 
# collection date was probably resulting in some NAs, and also there were
# different numbers of rows for each thing.
# this format works well with that type of issue, and we can simplify things
# as appropriate later. since these are mostly one-off measurements, we can 
# probably lump everything into static characterizations, but there might be 
# a few things, e.g. nitrogen measurements, that might be good to join in to 
# other data frames

biogeochem <- soil_chem_charact$spc_biogeochem %>%
  dplyr::select(-uid, -namedLocation, -domainID,-siteID, -nrcsDescriptionID,
                -horizonID, -analysisStartDate, -biogeoIDnrcs,-laboratoryName,
                -dataQF, -processingRemarks,-biogeoSampleType, -publicationDate) %>%
  mutate(collectDate = as.Date(collectDate))

biogeochem_l <- biogeochem %>%
  pivot_longer(cols = names(.)[4:ncol(.)],
               names_to = "variable",
               values_to = "value")

bulk_density <- soil_physical$spc_bulkdensity %>%
  dplyr::select(-uid, -namedLocation, -domainID,-siteID, -nrcsDescriptionID,
                -horizonID,-laboratoryName, -bulkDensProcessedDate, 
                -bulkDensMethod,-dataQF, -bulkDensIDnrcs, -remarks,
                -bulkDensMethodPub, -bulkDensSampleType, -publicationDate) %>%
  mutate(collectDate = as.Date(collectDate))

bulk_density_l <- bulk_density %>% 
  pivot_longer(cols = names(.)[4:ncol(.)],
               names_to = "variable",
               values_to = "value")

particle_size <- soil_physical$spc_particlesize %>%
  dplyr::select(-uid, -namedLocation, -domainID,-siteID, -nrcsDescriptionID,
                -horizonID,-laboratoryName,-biogeoIDnrcs, -dataQF, 
                -particleSizeDistMethodPub, -particleSizeDistProcessedDate,
                -particleSizeDistMethod,-biogeoSampleType, -publicationDate)%>%
  mutate(collectDate = as.Date(collectDate))

particle_size_l <- particle_size %>%
  pivot_longer(cols = names(.)[4:ncol(.)],
               names_to = "variable",
               values_to = "value")

# although, after looking at the next three ggplots, maybe these are best kept
#separate?
soil_charact_chem_phys_l <- rbind(biogeochem_l, bulk_density_l, particle_size_l)
write_csv(soil_charact_chem_phys_l, file.path(path_out, 
                                              "soil_charact_chem_phys_l.csv"))

# checking to see if it's a good idea to join in long form
# looks like biogeochem and  particle size are all good, bd is not done for SRER

# biogeochem$plotID%>%unique %>% sort
# particle_size$plotID%>%unique%>% sort
# bulk_density$plotID%>%unique%>% sort
# biogeochem$collectDate == particle_size$collectDate # all equal
# biogeochem$biogeoTopDepth[1:3] == bulk_density$bulkDensTopDepth[1:3] #not equal

# joining
soil_charact_chem_phys_w <- left_join(biogeochem, 
                                      particle_size, 
                                      by = c("plotID", "collectDate",
                                             "horizonName", "biogeoTopDepth",
                                             "biogeoBottomDepth", 
                                             "biogeoCenterDepth")) %>%
  left_join(bulk_density, by=c("plotID", "collectDate",
            "horizonName"))

write_csv(soil_charact_chem_phys_w, file.path(path_out, "soil_charact_chem_phys_w.csv"))

# full_join(soil_physical$spc_bulkdensity, 
  #           by=c("namedLocation","domainID","siteID", "plotID", "horizonID", 
  #                "horizonName")) %>%
  # full_join(soil_physical$spc_particlesize, 
  #           by=c("namedLocation","domainID","siteID", "plotID", "horizonID", 
  #                "horizonName", "biogeoBottomDepth"))

# 
# #looks like single dates of samples at each horizon
# ggplot(soil_chem_charact$spc_biogeochem, aes(x=collectDate, y=ctonRatio,
#                                              color = horizonName)) +
#   geom_point() +
#   facet_wrap(~plotID, scales="free_y")
# 
# # maybe taking only the top layer is a good idea?
# ggplot(soil_physical$spc_particlesize %>% filter(biogeoTopDepth == 0),
#        aes(x=collectDate, y=sandTotal, color = horizonName)) +
#   geom_point() +
#   facet_wrap(~plotID)

#bulk density is very sparsely measured... also, it's not clear to me which 
#column in the bulk density data frame is the final value of bulk density
# ggplot(soil_physical$spc_bulkdensity,
#        aes(x=collectDate, y=bulkDensCenterDepth, color = horizonName)) +
#   geom_point() +
#   facet_wrap(~plotID)

# Write the merged data to disk

## nitrogen and soil moisture data ----
# basically mineral N and soil moisture

nitro <- soil_chem_nitro$ntr_externalLab %>%
  dplyr::select(plotID, collectDate, kclAmmoniumNConc, 
                kclNitrateNitriteNConc, sampleID) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date(),
         kclAmmoniumNConc = ifelse(is.na(kclAmmoniumNConc), 0,kclAmmoniumNConc),
         kclNitrateNitriteNConc = ifelse(is.na(kclNitrateNitriteNConc), 0,
                                         kclNitrateNitriteNConc),
         mineral_n_total = kclNitrateNitriteNConc +kclAmmoniumNConc) %>%
  dplyr::select(plotID, collectDate, mineral_n_total, sampleID)%>% 
  filter(plotID != "")

sms <- soil_chem_nitro$sls_soilMoisture %>%
  dplyr::select(plotID, collectDate, 
                soilMoisture, sampleID) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date())

# checking to see if the nitrogen and soil moisture are all th esame plots 
# in the same order
# nitro$plotID %>% unique == sms$plotID %>% unique
# 
# ggplot() +
#   geom_point(data = nitro %>% filter(plotID != ""), 
#              aes(x = collectDate, y=mineral_n_total), 
#              color = "red", shape = 21) +
#   geom_point(data = sms, aes(x=collectDate, y=soilMoisture),
#              color = "blue", shape = 22) +
#   facet_wrap(~plotID, scales = "free_y")

soil_nitro <- left_join(nitro, sms, by = c("plotID", "collectDate", "sampleID"))
write_csv(soil_nitro, file.path(path_out, "soil_nitro_w.csv"))


## soil physical periodic ----
# seems like soil moisture and ph are the main pieces of info here, and they
# were collected in triplicate

soil_moisture <- soil_physical_periodic$sls_soilMoisture %>%
  dplyr::select(soilMoisture, collectDate, plotID, sampleID) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date());glimpse(soil_moisture)

soil_ph <- soil_physical_periodic$sls_soilpH %>%
  dplyr::select(plotID, collectDate, soilInWaterpH, soilInCaClpH,sampleID) %>%
  mutate(collectDate = str_sub(collectDate,1,10) %>% as.Date());glimpse(soil_ph)

# # visualizing 
# ggplot(soil_moisture, aes(x=collectDate, y=soilMoisture)) +
#   geom_boxplot(aes(group = collectDate)) +
#   facet_wrap(~plotID)
# 
# ggplot(soil_ph, aes(x=collectDate)) +
#   geom_boxplot(aes(y= soilInWaterpH,group = collectDate)) +
#   facet_wrap(~plotID)
# 
# ggplot(soil_ph, aes(x=collectDate)) +
#   geom_boxplot(aes(y= soilInCaClpH,group = collectDate)) +
#   facet_wrap(~plotID)
# 
# ggplot(soil_ph, aes(x=soilInWaterpH, y=soilInCaClpH)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0) +
#   annotate("text", label = "1:1 line", x=9.2,y=9)

soil_phys <- left_join(soil_moisture, soil_ph, by = c("plotID", "collectDate", "sampleID"))
write_csv(soil_phys, file.path(path_out, "soil_phys_periodic_w.csv"))
# unique(soil_phys$collectDate)

## soil chem periodic --- # it's mainly total n and organic C. 
# This one comes out of the box a little goofed up bit it's easy to fix

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
write_csv(soil_cn, file.path(path_out, "soil_cn_periodic_w.csv"))
# 
# ggplot(soil_cn) +
#   geom_point(aes(x=collectDate, y=soil_cn),
#              color = "red", shape = 21)+
#   facet_wrap(~plotID)
# 
# ggplot(soil_cn) +
#   geom_point(aes(x=organicCPercent, y=nitrogenPercent)) 
# 
# # looks like the collection dates are pretty well coordinated
# # problem - nothing has been collected since 2017
# ggplot(soil_cn) +
#   geom_point(aes(x=collectDate, y=organicCPercent)) +
#   facet_wrap(~str_sub(plotID,1,4), ncol=1)
# unique(soil_cn$collectDate) %>% sort

# note, 
## Joining perodic data ----


# => To be figured out




