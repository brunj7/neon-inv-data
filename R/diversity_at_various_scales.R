# diversity data at different scales
library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)

options(stringsAsFactors = FALSE)
source("R/unk_investigation.R")
# avoiding downloading over and over
if(!file.exists("data/diversity.RDS")){
  loadByProduct(dpID = "DP1.10058.001", 
                site = c("SRER", "ONAQ", "MOAB", "JORN"), 
                check.size = F) -> x
  saveRDS(x, "data/diversity.RDS")}else{
    x<-readRDS("data/diversity.RDS")}


# data wrangling section =======================================================

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
  summarise(cover = max(percentCover),
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

cover_plot <- x$div_1m2Data %>% 
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(divDataType == "plantSpecies") %>%
  mutate(year = str_c(str_sub(endDate,1,4)))%>% 
  group_by(plotID, subplotID, taxonID, year) %>%
  # dealing with the multiple bout issue by first getting the max cover
  # per sampling effort
  summarise(cover = max(percentCover),
            nativeStatusCode = first(nativeStatusCode),
            scientificName = first(scientificName),
            endDate = first(endDate),
            family = first(family)) %>%
  ungroup()  %>% 
  filter(taxonID != "") %>%
  group_by(plotID, taxonID, year) %>%
  summarise(cover = sum(cover, na.rm=TRUE)/8,
            nativeStatusCode = first(nativeStatusCode),
            scientificName = first(scientificName),
            endDate = first(endDate),
            family = first(family)) %>%
  ungroup()

traces_plot <- x$div_10m2Data100m2Data %>%
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
  filter(taxonID != "") %>%
  group_by(plotID, taxonID, year) %>%
  summarise(cover = sum(cover, na.rm=TRUE)/12,
            nativeStatusCode = first(nativeStatusCode),
            scientificName = first(scientificName),
            endDate = first(endDate),
            family = first(family)) %>%
  ungroup()



# aggregating at different scales ----------------------------------------------
cover8_1m2 <- cover8 %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  unk_fixer() #%>% get_diversity_info()

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

cover_plot <- rbind(cover_plot, traces_plot) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  unk_fixer()

# turningthem vegan friendly

vegify <- function(df) {
  return(
    df %>%
      mutate(p_sp_y = paste(plotID, subplotID, year, sep = "_")) %>%
      dplyr::select(p_sp_y, taxonID, cover) %>%
      na.omit() %>% # not sure how, but there are some NA's where they shouldn't be
      pivot_wider(id_cols = p_sp_y, 
                  names_from = taxonID, 
                  values_from = cover,
                  values_fill = list(cover=0)) %>%
      tibble::column_to_rownames("p_sp_y") %>%
      as.data.frame()
  )
}

# beta diversity calculations --------------------------------------------------
sites <- unique(cover4$site)

covers<- list(cover8_1m2, cover8_1m2_10m2, cover4)
s<-list()
sor <- list()
jac <- list()
bd_vectors<- list()
bdmods<-list()
for(ss in 1:length(sites)) {
  sps <- cover4 %>%
    filter(site == sites[ss]) %>%
    vegify()
  plot_ids <- str_sub(rownames(sps),1,8)
  years <- str_sub(rownames(sps), 13,16)

  
  # metaMDS takes a REALLY long time with huge data like this... not sure how to proceed
  # comm.k.mds <- metaMDS(comm, distance = "kul", trace = 0)
  
  # takes ~5min
  t0 <- Sys.time()
  bdmod_sim <- betadisper(betadiver(sps, 1), paste(plot_ids,years, sep="_"))
  
  grps <- paste(plot_ids, years, sep="_") %>% unique()
  jacbd <- list()
  for(i in 1:length(grps)){
    jacbd[[i]]<-sps %>%
      mutate(group = paste(plot_ids, years, sep = "_")) %>%
      filter(group == grps[i]) %>%
      dplyr::select(-group) %>%
      nestedbetajac() %>%
      as_tibble(rownames = "variable") %>%
      mutate(group = grps[i]) %>%
      pivot_wider(names_from = variable, values_from = value, id_cols=group)
  }
  bdjac <- do.call("rbind", jacbd)
  bdmods[[ss]] <- bdmod_sim
  # # quick viz
  # plot(bdmods[[4]], label = F)
  # # begs the question - should each site be done separately?
  centroids <- bdmod_sim$centroids[,1:2] %>%
    as_tibble(rownames = "group") %>%
    rename(centroid_x = PCoA1, centroid_y = PCoA2)
  
  s[[ss]] <- data.frame(bdmod_sim$distances, 
                        bdmod_sim$group,
                        bdmod_sim$vectors[,1:2]) %>%
    dplyr::rename(bd = bdmod_sim.distances, group = bdmod_sim.group) %>%
    mutate(year = str_sub(group,10,13) %>% as.numeric,
           site = str_sub(group,1,4),
           plotID = str_sub(group, 1,8),
           subplotID = str_split(rownames(sps), "_",simplify = T)[,3]) %>%
    left_join(centroids, by = "group") %>%
    left_join(bdjac, by = "group") %>%
    dplyr::select(-group)
  
  print(Sys.time() - t0)
  
}
full_bd<- do.call("rbind",s)

ggplot(full_bd, aes(color = as.factor(year))) +
  geom_line(aes(x = PCoA1, y = PCoA2, group=paste(plotID, year)),
            alpha = 0.5)+
  geom_point(aes(x = centroid_x, y = centroid_y)) +
  facet_wrap(~site) +
  theme_pubr()

ggplot(full_bd, aes(x=year, y=jaccard, color = site)) +
  geom_smooth() +
  geom_line(aes(group = plotID), alpha = 0.25)+
  facet_wrap(~site) +
  theme_pubr() 
