library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)



loadByProduct(dpID = "DP1.10058.001", site = c("SRER", "ONAQ", "MOAB", "JORN"), 
              check.size = F) -> x

cover <- x$div_1m2Data %>%
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(divDataType == "plantSpecies") %>%
  mutate(bout_year = str_c(str_sub(endDate,1,4),"_", boutNumber))%>%
  group_by(plotID, taxonID, bout_year) %>%
  summarise(cover = sum(percentCover, na.rm=TRUE)/8,
            nativeStatusCode = first(nativeStatusCode),
            endDate = first(endDate)) %>%
  ungroup() 

traces <- x$div_10m2Data100m2Data %>%
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(targetTaxaPresent == "Y") %>%
  mutate(bout_year = str_c(str_sub(endDate,1,4),"_", boutNumber))%>%
  group_by(plotID, taxonID, bout_year) %>%
  summarise(cover = 0.5,
            endDate = first(endDate),
            nativeStatusCode = first(nativeStatusCode)) %>%
  ungroup() 

n_i <- rbind(cover, traces)%>%
  group_by(plotID, endDate) %>%
  mutate(total_cover = sum(cover))%>%
  ungroup() %>%
  group_by(plotID, endDate, nativeStatusCode) %>%
  summarise(cover = sum(cover),
            total_cover = first(total_cover)) %>%
  ungroup() %>%
  mutate(rel_cover = cover/total_cover) %>%
  #filter(nativeStatusCode == "N" | nativeStatusCode == "I")%>%
  ungroup()

vegan_friendly_div <- rbind(cover, traces) %>%
  group_by(plotID, taxonID, bout_year, endDate, nativeStatusCode) %>%
  summarise(cover = sum(cover)) %>%
  ungroup() %>%
  mutate(taxonID = as.character(taxonID),
         plotID = as.character(plotID),
         nativeStatusCode = as.character(nativeStatusCode)) %>%
  filter(nchar(as.character(taxonID))>0,
         nativeStatusCode != "",
         nativeStatusCode != "A",
         nativeStatusCode != "NI") %>%
  # dplyr::select(-endDate) %>%
  group_by(plotID, bout_year, nativeStatusCode) %>%
  #filter(bout_year == "2017_1") %>%
  #dplyr::select(-endDate, -bout_year,-nativeStatusCode) %>%
  #mutate(year = str_sub(bout_year,1,4)) %>%
  spread(taxonID, cover, fill=0) %>%
  ungroup()

vegan_friendly_div$shannon = diversity(vegan_friendly_div[5:ncol(vegan_friendly_div)])
vegan_friendly_div$nspp = specnumber(vegan_friendly_div[5:ncol(vegan_friendly_div)])

nspp <- vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, nspp) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = nspp,
              values_fill = list(nspp=0)) %>%
  rename(nspp_native = N, nspp_exotic=I, nspp_unk = UNK)

shannon <- vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon,
              values_fill = list(shannon=0)) %>%
  rename(shannon_native = N, shannon_exotic=I, shannon_unk = UNK)

#still need total diverstiy
plot_level <- left_join(nspp, shannon, by = c("plotID", "bout_year", "endDate"))
