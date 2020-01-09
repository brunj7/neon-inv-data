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


# gonna make this whole thing into a giant function
get_diversity_info <- function(neon_div_object, 
                               scale, 
                               families = "Poaceae",
                               species = "Bromus tectorum") { 
  # scale options: "1", "10", "100", "plot"
    # 1 means aggregated by 1m2 subplots only,
    # 10 means 1m2 and 10m2 subplots are aggregated
    # 100 means each quadrant of the plot is aggregated...
      # i.e. 2 1m2 and 10m2 subplots plus the 100m2 subplot
    # plot means the whole plot is aggregated together
  # concatenate a list of family names for family cover, grass fam is default
  # concatenate a list of species names in similar fashion
  if(scale == "plot"){
    cover <- x$div_1m2Data %>% 
      mutate(endDate = as.Date(endDate)) %>%
      dplyr::filter(divDataType == "plantSpecies") %>%
      mutate(year = str_c(str_sub(endDate,1,4)))%>% 
      group_by(plotID, subplotID, taxonID, year) %>%
      # dealing with the multiple bout issue by first getting the max cover
      # per sampling effort
      summarise(cover = max(percentCover),
                nativeStatusCode = first(nativeStatusCode),
                scientificName = first(scientificName),
                family = first(family)) %>%
      ungroup()  %>% 
      filter(taxonID != "") %>%
      group_by(plotID, taxonID, year) %>%
      summarise(cover = sum(cover, na.rm=TRUE)/8,
                nativeStatusCode = first(nativeStatusCode),
                scientificName = first(scientificName),
                family = first(family)) %>%
      ungroup()
    
    traces <- x$div_10m2Data100m2Data %>%
      mutate(endDate = as.Date(endDate)) %>%
      dplyr::filter(targetTaxaPresent == "Y") %>%
      mutate(year = str_c(str_sub(endDate,1,4)))%>%
      group_by(plotID, subplotID, taxonID, year) %>%
      summarise(cover = 0.5,
                scientificName = first(scientificName),
                nativeStatusCode = first(nativeStatusCode),
                family = first(family)) %>%
      ungroup() %>% 
      filter(taxonID != "") %>%
      group_by(plotID, taxonID, year) %>%
      summarise(cover = sum(cover, na.rm=TRUE)/12,
                nativeStatusCode = first(nativeStatusCode),
                scientificName = first(scientificName),
                family = first(family)) %>%
      ungroup()
    
    full_on_cover <- rbind(cover, traces) %>%
      mutate(site = str_sub(plotID, 1,4)) %>%
      unk_fixer()
  }
  
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
    unk_fixer() #%>% get_diversity_info()
  
  cover8_1m2_10m2 <- rbind(cover8, traces8) %>%
    mutate(site = str_sub(plotID, 1,4)) %>%
    unk_fixer()
  
  cover4 <- cover8_1m2_10m2 %>%
    mutate(subplotID = str_sub(subplotID, 1,2)) %>%
    rbind(traces100s) %>% # adding in the 100m2 subplots
    group_by(plotID, subplotID, year, taxonID) %>%
    summarise(cover = max(cover), #max, mean? up for debate
              scientificName = first(scientificName),
              nativeStatusCode = first(nativeStatusCode),
              family = first(family),
              site = first(site)) %>%
    ungroup() %>%
    unk_fixer()
  
  
  if(scale == "1") full_on_cover <- cover8_1m2
  if(scale == "10") full_on_cover <- cover8_1m2_10m2
  if(scale == "100") full_on_cover <- cover4
  
  
  
  if(!"subplotID" %in% colnames(full_on_cover)) full_on_cover$subplotID <- "plot"
  
  n_i <- full_on_cover%>%
    group_by(plotID, subplotID, year) %>%
    mutate(total_cover = sum(cover))%>%
    ungroup() %>%
    group_by(plotID, subplotID,year, nativeStatusCode) %>%
    summarise(cover = sum(cover),
              total_cover = first(total_cover)) %>% 
    ungroup() %>%
    mutate(rel_cover = cover/total_cover) %>%
    #filter(nativeStatusCode == "N" | nativeStatusCode == "I")%>%
    ungroup() %>%
    filter(nativeStatusCode != "")
    
  n_i_cover <- n_i %>%
    filter(nativeStatusCode != "" &
             nativeStatusCode != "A" &
             nativeStatusCode != "NI") %>%
    dplyr::select(plotID, subplotID,year, nativeStatusCode, cover) %>%
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
    dplyr::select(plotID, subplotID,year, nativeStatusCode, rel_cover) %>%
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
              total_cover = first(total_cover)) %>%
    ungroup() %>%
    mutate(rel_cover = cover/total_cover) %>%
    ungroup() %>%
    filter(family != "")
  
  rcf<- byfam%>%
    dplyr::select(plotID, subplotID,year, family, rel_cover) %>%
    filter(family %in% families)%>%
    pivot_wider(names_from = family,
                names_prefix = "rc_",
                values_from = (rel_cover),
                values_fill = list(rel_cover = 0))
  
  cf<- byfam%>%
    dplyr::select(plotID, subplotID,year, family, cover) %>%
    filter(family %in% families)%>%
    pivot_wider(names_from = family,
                names_prefix = "cover_",
                values_from = (cover),
                values_fill = list(cover = 0))
  
  bysp <- full_on_cover%>%
    group_by(plotID, subplotID, year) %>%
    mutate(total_cover = sum(cover))%>%
    ungroup() %>%
    group_by(plotID, subplotID,year, scientificName) %>%
    summarise(cover = sum(cover),
              total_cover = first(total_cover)) %>% 
    ungroup() %>%
    mutate(rel_cover = cover/total_cover) %>%
    ungroup() %>%
     mutate(genus = str_split(scientificName,
                      pattern = " ",
                      simplify = TRUE)[,1],
            species = str_split(scientificName,
                              pattern = " ",
                              simplify = TRUE)[,2],
            gen_sp = str_c(genus, " ", species))
  
  rc_sp <- bysp%>%
    dplyr::select(plotID, subplotID,year, gen_sp, rel_cover) %>%
    filter(gen_sp %in% species)%>%
    mutate(gen_sp = str_replace(gen_sp, " ","_")) %>%
    pivot_wider(names_from = gen_sp,
                names_prefix = "rc_",
                values_from = (rel_cover),
                values_fill = list(rel_cover = 0))
  
  c_sp<- bysp%>%
    dplyr::select(plotID, subplotID,year, gen_sp, cover) %>%
    filter(gen_sp %in% species)%>%
    mutate(gen_sp = str_replace(gen_sp, " ","_")) %>%
    pivot_wider(names_from = gen_sp,
                names_prefix = "cover_",
                values_from = (cover),
                values_fill = list(cover = 0))
  
  exotic_grass <- full_on_cover%>%
    group_by(plotID, subplotID, year) %>%
    mutate(total_cover = sum(cover))%>%
    ungroup() %>%
    group_by(plotID, subplotID,year, family, nativeStatusCode) %>%
    summarise(cover = sum(cover),
              total_cover = first(total_cover)) %>% 
    ungroup() %>%
    mutate(rel_cover = cover/total_cover) %>%
    ungroup() %>%
    filter(family != "")
  
  # what about having exotic forbs as well?
  # maybe that's just exotic cover - exotic grass cover
  rc_ig<- exotic_grass%>%
    dplyr::select(plotID, subplotID,year, family, nativeStatusCode,rel_cover) %>%
    filter(nativeStatusCode == "I") %>%
    filter(family %in% families)%>%
    # filter(family == "Poaceae") %>%
    pivot_wider(names_from = family,
                names_prefix = "rc_exotic_",
                values_from = (rel_cover),
                values_fill = list(rel_cover = 0)) %>%
    dplyr::select(-nativeStatusCode)
  
  rc_ng<- exotic_grass%>%
    dplyr::select(plotID, subplotID,year, family, nativeStatusCode,rel_cover) %>%
    filter(nativeStatusCode == "N") %>%
    filter(family %in% families)%>%
    # filter(family == "Poaceae") %>%
    pivot_wider(names_from = family,
                names_prefix = "rc_native_",
                values_from = (rel_cover),
                values_fill = list(rel_cover = 0)) %>%
    dplyr::select(-nativeStatusCode)
  
  c_ig<- exotic_grass%>%
    dplyr::select(plotID, subplotID,year, family, nativeStatusCode,cover) %>%
    filter(nativeStatusCode == "I") %>%
    # filter(family == "Poaceae") %>%
    filter(family %in% families)%>%
    pivot_wider(names_from = family,
                names_prefix = "cover_exotic_",
                values_from = (cover),
                values_fill = list(cover = 0)) %>%
    dplyr::select(-nativeStatusCode)
  
  #diversity indexes splitting between native status
  vegan_friendly_div <- full_on_cover %>%
    group_by(plotID, subplotID,taxonID, year, nativeStatusCode) %>%
    summarise(cover = sum(cover, na.rm = TRUE)) %>% 
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
  
  # summary(vegan_friendly_div)
  
  # note to self - hard code! gotta fix it
  vegan_friendly_div$shannon = diversity(vegan_friendly_div[5:ncol(vegan_friendly_div)]) 
  vegan_friendly_div$nspp = specnumber(vegan_friendly_div[5:ncol(vegan_friendly_div)])
  
  nspp <- vegan_friendly_div %>%
    dplyr::select(plotID, subplotID,year, nativeStatusCode, nspp) %>%
    pivot_wider(names_from = nativeStatusCode,
                values_from = nspp,
                values_fill = list(nspp=0)) %>%
    rename(nspp_native = N, nspp_exotic=I, nspp_unk = UNK)
  
  shannon <- vegan_friendly_div %>%
    dplyr::select(plotID, subplotID,year, nativeStatusCode, shannon) %>%
    pivot_wider(names_from = nativeStatusCode,
                values_from = shannon,
                values_fill = list(shannon=0)) %>%
    rename(shannon_native = N, shannon_exotic=I, shannon_unk = UNK)
  
  # total diversity - not splitting between native status
  vegan_friendly_div_total <- full_on_cover %>%
    group_by(plotID, subplotID, taxonID, year) %>%
    summarise(cover = sum(cover)) %>%
    ungroup() %>%
    mutate(taxonID = as.character(taxonID),
           plotID = as.character(plotID)) %>%
    filter(nchar(as.character(taxonID))>0) %>%
    group_by(plotID, subplotID,year) %>%
    spread(taxonID, cover, fill=0) %>%
    ungroup()
  
  # note to self - fix that hard-coding!
  div_total = dplyr::select(vegan_friendly_div_total, plotID, subplotID,year)
  div_total$shannon_total = diversity(vegan_friendly_div_total[4:ncol(vegan_friendly_div_total)])
  div_total$nspp_total = specnumber(vegan_friendly_div_total[4:ncol(vegan_friendly_div_total)])
  
  # joining and writing out ------------------------------------------------------
  final_table <- left_join(nspp, shannon, by = c("plotID", "subplotID", "year")) %>%
    left_join(n_i_cover, by = c("plotID", "subplotID","year")) %>%
    left_join(n_i_rel_cover, by = c("plotID", "subplotID", "year")) %>%
    left_join(div_total, by = c("plotID", "subplotID", "year"))%>%
    left_join(rcf, by = c("plotID", "subplotID", "year")) %>%
    left_join(rc_ig, by = c("plotID", "subplotID", "year"))%>%
    left_join(c_ig, by = c("plotID", "subplotID", "year"))%>%
    left_join(cf, by = c("plotID", "subplotID", "year")) %>%
    left_join(rc_ng, by = c("plotID", "subplotID", "year")) %>%
    left_join(rc_sp, by = c("plotID", "subplotID", "year")) %>%
    left_join(c_sp, by = c("plotID", "subplotID", "year")) %>%
    mutate(site = str_sub(plotID, 1,4))
  
  # seems crazy, i know...
  final_table[is.na(final_table)] <- 0
    
  return(final_table)
}

plot_level <- get_diversity_info(neon_div_object = x, scale = "plot")
sp_level_1 <- get_diversity_info(x, "1")
sp_level_10 <- get_diversity_info(x, "10")
sp_level_100 <- get_diversity_info(x, "100")

write_csv(plot_level, "data/plot_level_diversity_stuff.csv")
write_csv(sp_level_1, "data/subplot_level_diversity_1.csv")
write_csv(sp_level_10, "data/subplot_level_diversity_10.csv")
write_csv(sp_level_100, "data/subplot_level_diversity_100.csv")


