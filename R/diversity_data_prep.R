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

# data comes in two separate components - 1m2 subplots with cover estimates
# and 10 and 100m2 subplots with presence only

# checking out unknown species. I (Adam)
# figured out a fair amount so far... made an unk fixer function 
source("R/unk_investigation.R")

# Initial ingest ---------------------------------------------------------------
# 1m2 cover is aggregated by sampling bout and plot
cover <- x$div_1m2Data %>% 
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
  group_by(plotID, taxonID, year) %>%
  summarise(cover = sum(cover, na.rm=TRUE)/8,
            nativeStatusCode = first(nativeStatusCode),
            scientificName = first(scientificName),
            endDate = first(endDate),
            family = first(family)) %>%
  ungroup()

traces <- x$div_10m2Data100m2Data %>%
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

full_on_cover <- rbind(cover, traces) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  unk_fixer()


# # side tangent - to look at remaining unks do something like this
# unks <- full_on_cover %>% 
#   filter(nativeStatusCode == "UNK") %>% 
#   select(taxonID, plotID, family, scientificName) %>%
#   mutate(site = str_sub(plotID, 1,4)) %>%
#   group_by(site, taxonID) %>%
#   summarise(family = first(family),
#             scientificName = paste(unique(scientificName))) %>%
#   ungroup()


# calculating various indexes at plot level at each timestep -------------------
# native vs invasive cover and relative cover
# note to self do relative grass cover too, relative, invasive grass cover,
# relative legume (fabaceae) cover

# gonna make this whole thing into a giant function
get_diversity_info <- function(full_on_cover) { # note to self, gotta change the name of that arguement

  if(!"subplotID" %in% colnames(full_on_cover)) full_on_cover$subplotID <- "plot"
  
  n_i <- full_on_cover%>%
    group_by(plotID, subplotID, year) %>%
    mutate(total_cover = sum(cover))%>%
    ungroup() %>%
    group_by(plotID, subplotID,year, nativeStatusCode) %>%
    summarise(cover = sum(cover),
              total_cover = first(total_cover),
              endDate = first(endDate)) %>% # note, endDate might as well be removed
    ungroup() %>%
    mutate(rel_cover = cover/total_cover) %>%
    #filter(nativeStatusCode == "N" | nativeStatusCode == "I")%>%
    ungroup() %>%
    filter(nativeStatusCode != "")
    
  n_i_cover <- n_i %>%
    filter(nativeStatusCode != "" &
             nativeStatusCode != "A" &
             nativeStatusCode != "NI") %>%
    dplyr::select(plotID, subplotID,year, endDate, nativeStatusCode, cover) %>%
    pivot_wider(names_from = nativeStatusCode,
                values_from = cover,
                values_fill = list(cover = 0)) %>%
    rename(cover_native = N,
           cover_exotic = I,
           cover_unk = UNK)
  
  n_i_rel_cover <- n_i %>%
    filter(nativeStatusCode != ""&
             nativeStatusCode != "A" &
             nativeStatusCode != "NI") %>%
    dplyr::select(plotID, subplotID,year, endDate, nativeStatusCode, rel_cover) %>%
    pivot_wider(names_from = nativeStatusCode,
                values_from = rel_cover,
                values_fill = list(rel_cover = 0))%>%
    rename(rel_cover_native = N,
           rel_cover_exotic = I,
           rel_cover_unk = UNK)
  
  
  byfam <- full_on_cover%>%
    group_by(plotID, subplotID, year) %>%
    mutate(total_cover = sum(cover))%>%
    ungroup() %>%
    group_by(plotID, subplotID,year, family) %>%
    summarise(cover = sum(cover),
              total_cover = first(total_cover),
              endDate = first(endDate)) %>% # note, endDate might as well be removed
    ungroup() %>%
    mutate(rel_cover = cover/total_cover) %>%
    ungroup() %>%
    filter(family != "")
  
  rcf<- byfam%>%
    dplyr::select(plotID, subplotID,year, endDate, family, rel_cover) %>%
    filter(family == "Poaceae" | family == "Fabaceae") %>%
    pivot_wider(names_from = family,
                names_prefix = "rc_",
                values_from = (rel_cover),
                values_fill = list(rel_cover = 0))
  
  cf<- byfam%>%
    dplyr::select(plotID, subplotID,year, endDate, family, cover) %>%
    filter(family == "Poaceae" | family == "Fabaceae") %>%
    pivot_wider(names_from = family,
                names_prefix = "cover_",
                values_from = (cover),
                values_fill = list(cover = 0))
  
  exotic_grass <- full_on_cover%>%
    group_by(plotID, subplotID, year) %>%
    mutate(total_cover = sum(cover))%>%
    ungroup() %>%
    group_by(plotID, subplotID,year, family, nativeStatusCode) %>%
    summarise(cover = sum(cover),
              total_cover = first(total_cover),
              endDate = first(endDate)) %>% # note, endDate might as well be removed
    ungroup() %>%
    mutate(rel_cover = cover/total_cover) %>%
    ungroup() %>%
    filter(family != "")
  
  # what about having exotic forbs as well?
  # maybe that's just exotic cover - exotic grass cover
  rc_ig<- exotic_grass%>%
    dplyr::select(plotID, subplotID,year, endDate, family, nativeStatusCode,rel_cover) %>%
    filter(nativeStatusCode == "I") %>%
    filter(family == "Poaceae") %>%
    pivot_wider(names_from = family,
                names_prefix = "rc_exotic_",
                values_from = (rel_cover),
                values_fill = list(rel_cover = 0)) %>%
    dplyr::select(-nativeStatusCode)
  
  rc_ng<- exotic_grass%>%
    dplyr::select(plotID, subplotID,year, endDate, family, nativeStatusCode,rel_cover) %>%
    filter(nativeStatusCode == "N") %>%
    filter(family == "Poaceae") %>%
    pivot_wider(names_from = family,
                names_prefix = "rc_native_",
                values_from = (rel_cover),
                values_fill = list(rel_cover = 0)) %>%
    dplyr::select(-nativeStatusCode)
  
  c_ig<- exotic_grass%>%
    dplyr::select(plotID, subplotID,year, endDate, family, nativeStatusCode,cover) %>%
    filter(nativeStatusCode == "I") %>%
    filter(family == "Poaceae") %>%
    pivot_wider(names_from = family,
                names_prefix = "cover_exotic_",
                values_from = (cover),
                values_fill = list(cover = 0)) %>%
    dplyr::select(-nativeStatusCode)
  
  #diversity indexes splitting between native status
  vegan_friendly_div <- full_on_cover %>%
    group_by(plotID, subplotID,taxonID, year, endDate, nativeStatusCode) %>%
    summarise(cover = sum(cover)) %>%
    ungroup() %>%
    mutate(taxonID = as.character(taxonID),
           plotID = as.character(plotID),
           nativeStatusCode = as.character(nativeStatusCode)) %>%
    filter(nchar(as.character(taxonID))>0,
           nativeStatusCode != "",
           nativeStatusCode != "A",
           nativeStatusCode != "NI") %>%
    group_by(plotID, subplotID, year, nativeStatusCode) %>%
    spread(taxonID, cover, fill=0) %>%
    ungroup()
  
  # note to self - hard code! gotta fix it
  vegan_friendly_div$shannon = diversity(vegan_friendly_div[6:ncol(vegan_friendly_div)]) 
  vegan_friendly_div$nspp = specnumber(vegan_friendly_div[6:ncol(vegan_friendly_div)])
  
  nspp <- vegan_friendly_div %>%
    dplyr::select(plotID, subplotID,year, endDate, nativeStatusCode, nspp) %>%
    pivot_wider(names_from = nativeStatusCode,
                values_from = nspp,
                values_fill = list(nspp=0)) %>%
    rename(nspp_native = N, nspp_exotic=I, nspp_unk = UNK)
  
  shannon <- vegan_friendly_div %>%
    dplyr::select(plotID, subplotID,year, endDate, nativeStatusCode, shannon) %>%
    pivot_wider(names_from = nativeStatusCode,
                values_from = shannon,
                values_fill = list(shannon=0)) %>%
    rename(shannon_native = N, shannon_exotic=I, shannon_unk = UNK)
  
  # total diversity - not splitting between native status
  vegan_friendly_div_total <- full_on_cover %>%
    group_by(plotID, subplotID, taxonID, year, endDate) %>%
    summarise(cover = sum(cover)) %>%
    ungroup() %>%
    mutate(taxonID = as.character(taxonID),
           plotID = as.character(plotID)) %>%
    filter(nchar(as.character(taxonID))>0) %>%
    group_by(plotID, subplotID,year) %>%
    spread(taxonID, cover, fill=0) %>%
    ungroup()
  
  # note to self - fix that hard-coding!
  div_total = dplyr::select(vegan_friendly_div_total, plotID, subplotID,year, endDate)
  div_total$shannon_total = diversity(vegan_friendly_div_total[5:ncol(vegan_friendly_div_total)])
  div_total$nspp_total = specnumber(vegan_friendly_div_total[5:ncol(vegan_friendly_div_total)])
  
  
  
  # joining and writing out ------------------------------------------------------
  plot_level <- left_join(nspp, shannon, by = c("plotID", "subplotID", "year", "endDate")) %>%
    left_join(n_i_cover, by = c("plotID", "subplotID","year", "endDate")) %>%
    left_join(n_i_rel_cover, by = c("plotID", "subplotID", "year", "endDate")) %>%
    left_join(div_total, by = c("plotID", "subplotID", "year", "endDate"))%>%
    left_join(rcf, by = c("plotID", "subplotID", "year", "endDate")) %>%
    left_join(rc_ig, by = c("plotID", "subplotID", "year", "endDate"))%>%
    left_join(c_ig, by = c("plotID", "subplotID", "year", "endDate"))%>%
    left_join(cf, by = c("plotID", "subplotID", "year", "endDate")) %>%
    left_join(rc_ng, by = c("plotID", "subplotID", "year", "endDate")) %>%
    mutate(site = str_sub(plotID, 1,4))
    
  
  return(plot_level)
}

plot_level <- get_diversity_info(full_on_cover = full_on_cover)

write_csv(plot_level, "data/plot_level_diversity_stuff.csv")

