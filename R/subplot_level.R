
library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)

x <- loadByProduct(dpID = "DP1.10058.001", site = c("SRER", "ONAQ", "MOAB", "JORN"), 
              check.size = F)

cover_subplots <- x$div_1m2Data %>%
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(divDataType == "plantSpecies") %>%
  mutate(bout_year = str_c(str_sub(endDate,1,4),"_", boutNumber))%>%
  group_by(plotID, subplotID, taxonID, bout_year) %>%
  summarise(cover = sum(percentCover, na.rm=TRUE)/8,
            nativeStatusCode = first(nativeStatusCode),
            endDate = first(endDate)) %>%
  ungroup()  %>%
  mutate(taxonID = as.character(taxonID),
         plotID = as.character(plotID),
         subplotID = as.character(subplotID),
         nativeStatusCode = as.character(nativeStatusCode)) %>%
  filter(nchar(as.character(taxonID))>0,
         nativeStatusCode != "",
         nativeStatusCode != "A",
         nativeStatusCode != "NI") %>%
  # dplyr::select(-endDate) %>%
  group_by(plotID, subplotID, bout_year, nativeStatusCode) %>%
  #filter(bout_year == "2017_1") %>%
  #dplyr::select(-endDate, -bout_year,-nativeStatusCode) %>%
  #mutate(year = str_sub(bout_year,1,4)) %>%
  spread(taxonID, cover, fill=0) %>%
  ungroup()

cover_subplots$shannon = diversity(cover_subplots[6:ncol(cover_subplots)])
cover_subplots$nspp = specnumber(cover_subplots[6:ncol(cover_subplots)])

cover_subplots %>%
  dplyr::select(plotID, subplotID, bout_year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x=N, y=I, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()
