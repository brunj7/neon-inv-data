# diversity data at different scales
library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)

options(stringsAsFactors = FALSE)

# avoiding downloading over and over
if(!file.exists("data/diversity.RDS")){
  loadByProduct(dpID = "DP1.10058.001", 
                site = c("SRER", "ONAQ", "MOAB", "JORN"), 
                check.size = F) -> x
  saveRDS(x, "data/diversity.RDS")}else{
    x<-readRDS("data/diversity.RDS")}

# first, each 1m2 + 10m2 (each 1m2 is nested with a 10m2 around it)
# 8 means there are 8 subplots
cover8 <- x$div_1m2Data %>% 
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(divDataType == "plantSpecies") %>%
  mutate(year = str_c(str_sub(endDate,1,4)))%>%
  group_by(plotID, subplotID, taxonID, year) %>%
  # dealing with the multiple bout issue by first getting the mean cover
  # per sampling effort, without aggregating, then later we'll aggregate.
  # that way, a fall-bloomer that isn't visible in spring, for example, 
  # will be given its full cover value for fall, but then a species 
  # that is there for both seasons will be averaged, if that makes sense
  summarise(cover = mean(percentCover, na.rm=TRUE),
            nativeStatusCode = first(nativeStatusCode),
            scientificName = first(scientificName),
            endDate = first(endDate),
            family = first(family)) %>%
  ungroup()  %>%
  filter(taxonID != "") %>%
  mutate(subplotID = str_sub(subplotID, 1, 4))

# 10m2,100m2 are given 0.5 (we can change later)
# unique(x$div_10m2Data100m2Data$subplotID) # there are 12 subplots

traces8 <- x$div_10m2Data100m2Data %>%
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(targetTaxaPresent == "Y") %>%
  mutate(year = str_c(str_sub(endDate,1,4)))%>%
  group_by(plotID, subplotID, taxonID, year) %>%
  summarise(cover = 0.5,
            endDate = first(endDate),
            scientificName = first(scientificName),
            nativeStatusCode = first(nativeStatusCode),
            family = first(family)) %>%
  ungroup() %>% 
  filter(taxonID != "", 
         subplotID != "31", # these are the 100m2 subplots under which two 1m2 and 10m2 pairs are nested
         subplotID != "32",
         subplotID != "40",
         subplotID != "41")  %>%
  mutate(subplotID = str_sub(subplotID, 1, 4))

traces100s <- x$div_10m2Data100m2Data %>%
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(targetTaxaPresent == "Y") %>%
  mutate(year = str_c(str_sub(endDate,1,4)))%>%
  group_by(plotID, subplotID, taxonID, year) %>%
  summarise(cover = 0.5,
            endDate = first(endDate),
            scientificName = first(scientificName),
            nativeStatusCode = first(nativeStatusCode),
            family = first(family)) %>%
  ungroup() %>% 
  mutate(site = str_sub(plotID, 1,4)) %>%
  filter(taxonID != "", 
         subplotID == "31"| # these are the 100m2 subplots under which two 1m2 and 10m2 pairs are nested
         subplotID == "32"|
         subplotID == "40"|
         subplotID == "41")

# aggregating at different scales ----------------------------------------------
cover8_1m2 <- cover8 %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  unk_fixer()

cover8_1m2_10m2 <- rbind(cover8, traces8) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  unk_fixer()

cover4 <- cover8_1m2_10m2 %>%
  mutate(subplotID = str_sub(subplotID, 1,2)) %>%
  rbind(traces100s) %>% # adding in the 100m2 subplots
  group_by(plotID, subplotID, year, taxonID) %>%
  summarise(cover = max(cover), #max, mean? up for debate
            endDate = first(endDate), # might be better to change to min or max upstream
            scientificName = first(scientificName),
            nativeStatusCode = first(nativeStatusCode),
            family = first(family),
            site = first(site)) %>%
  ungroup() %>%
  unk_fixer()

