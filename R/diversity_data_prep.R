library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)

options(stringsAsFactors = FALSE)


loadByProduct(dpID = "DP1.10058.001", 
              site = c("SRER", "ONAQ", "MOAB", "JORN"), 
              check.size = F) -> x

# data comes in two separate components - 1m2 subplots with cover estimates
# and 10 and 100m2 subplots with presence only

# Initial ingest ---------------------------------------------------------------
# 1m2 cover is aggregated by sampling bout and plot
cover <- x$div_1m2Data %>% 
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(divDataType == "plantSpecies") %>%
  mutate(bout_year = str_c(str_sub(endDate,1,4),"_", boutNumber))%>%
  group_by(plotID, taxonID, bout_year) %>%
  summarise(cover = sum(percentCover, na.rm=TRUE)/8,
            nativeStatusCode = first(nativeStatusCode),
            endDate = first(endDate),
            family = first(family)) %>%
  ungroup()  %>%
  filter(taxonID != "")



# 10m2,100m2 are given 0.5 (we can change later)
traces <- x$div_10m2Data100m2Data %>%
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(targetTaxaPresent == "Y") %>%
  mutate(bout_year = str_c(str_sub(endDate,1,4),"_", boutNumber))%>%
  group_by(plotID, taxonID, bout_year) %>%
  summarise(cover = 0.5,
            endDate = first(endDate),
            nativeStatusCode = first(nativeStatusCode),
            family = first(family)) %>%
  ungroup() 

unks <- rbind(cover, traces) %>% filter(nativeStatusCode == "UNK") %>% select(taxonID) %>% as.vector() %>% unique()


# calculating various indexes at plot level at each timestep -------------------
# native vs invasive cover and relative cover
n_i <- rbind(cover, traces)%>%
  group_by(plotID, endDate) %>%
  mutate(total_cover = sum(cover))%>%
  ungroup() %>%
  group_by(plotID, bout_year, nativeStatusCode) %>%
  summarise(cover = sum(cover),
            total_cover = first(total_cover),
            endDate = first(endDate)) %>%
  ungroup() %>%
  mutate(rel_cover = cover/total_cover) %>%
  #filter(nativeStatusCode == "N" | nativeStatusCode == "I")%>%
  ungroup() %>%
  filter(nativeStatusCode != "")
  
n_i_cover <- n_i %>%
  filter(nativeStatusCode != "" &
           nativeStatusCode != "A" &
           nativeStatusCode != "NI") %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, cover) %>%
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
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, rel_cover) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = rel_cover,
              values_fill = list(rel_cover = 0))%>%
  rename(rel_cover_native = N,
         rel_cover_exotic = I,
         rel_cover_unk = UNK)

#diversity indexes splitting between native status
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

# total diversity - not splitting between native status
vegan_friendly_div_total <- rbind(cover, traces) %>%
  group_by(plotID, taxonID, bout_year, endDate) %>%
  summarise(cover = sum(cover)) %>%
  ungroup() %>%
  mutate(taxonID = as.character(taxonID),
         plotID = as.character(plotID)) %>%
  filter(nchar(as.character(taxonID))>0) %>%
  # dplyr::select(-endDate) %>%
  group_by(plotID, bout_year) %>%
  #filter(bout_year == "2017_1") %>%
  #dplyr::select(-endDate, -bout_year,-nativeStatusCode) %>%
  #mutate(year = str_sub(bout_year,1,4)) %>%
  spread(taxonID, cover, fill=0) %>%
  ungroup()

div_total = dplyr::select(vegan_friendly_div_total, plotID, bout_year, endDate)
div_total$shannon_total = diversity(vegan_friendly_div_total[4:ncol(vegan_friendly_div_total)])
div_total$nspp_total = specnumber(vegan_friendly_div_total[4:ncol(vegan_friendly_div_total)])



# joining and writing out ------------------------------------------------------
plot_level <- left_join(nspp, shannon, by = c("plotID", "bout_year", "endDate")) %>%
  left_join(n_i_cover, by = c("plotID", "bout_year", "endDate")) %>%
  left_join(n_i_rel_cover, by = c("plotID", "bout_year", "endDate")) %>%
  left_join(div_total, by = c("plotID", "bout_year", "endDate"))
write_csv(plot_level, "data/plot_level_diversity_stuff.csv")
