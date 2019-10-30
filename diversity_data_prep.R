library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)



loadByProduct(dpID = "DP1.10058.001", site = c("SRER", "ONAQ", "MOAB"), 
              check.size = F) -> x

# idea - full sacs per site, then per plot through time (each plot, all years)

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

n_v_i_div <- vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, nspp) %>%
  pivot_wider(names_from = nativeStatusCode,
               values_from = nspp) %>%
  mutate(site = str_sub(plotID, 1,4))

ggplot(n_v_i_div, aes(x=N, y=I, color = site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Native vs. Exotic Species Richness")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()# +
 # facet_wrap(~site, scales = "free") 
 
glm(I ~ N, data = filter(n_v_i_div, site == "SRER"), family = "poisson") %>% summary

for(i in vegan_friendly_div$bout_year){
plot(specaccum(comm = vegan_friendly_div %>%
                 filter(nativeStatusCode == "N", bout_year == i) %>%
                 dplyr::select(-bout_year, -endDate, -nativeStatusCode,-plotID)
                  ), add=T, col = substr())
}

ggplot(vegan_friendly_div,
       aes(x = endDate, y=shannon, color = nativeStatusCode)) +
  geom_line() +
  #theme(legend.position = "none") +
  facet_wrap(~plotID) +
  theme_pubr() +
  ggsave("/home/a/Desktop/quickplot.png")

ggplot(n_i,
       aes(x = endDate, y=rel_cover, color = nativeStatusCode)) +
  geom_line() +
  #theme(legend.position = "none") +
  facet_wrap(~plotID) +
  theme_pubr() +
  ggsave("/home/a/Desktop/quickplot.png")


sact <- list()
plot_ids <- vegan_friendly_div$plotID

for(i in 1:nrow(vegan_friendly_div)){
  vegan_friendly_div %>% 
    filter(plotID == plot_ids[i]) %>% 
    dplyr::select(-plotID,-bout_year, -endDate, -nativeStatusCode) %>% 
    specaccum(method = "exact") -> zero
  sact[[i]] <- data.frame(sites = zero$sites, 
                        richness = zero$richness, 
                        sd = zero$sd,
                        plot = plot_ids[i],
                        site = vegan_friendly_div$plotID[i] %>% str_sub(1,4),
                        nativity = vegan_friendly_div$nativeStatusCode[i])
}
sact <- do.call("rbind", sact)

pd <- position_dodge(0.1)

ggplot(sact, aes(x=sites, y=richness, color = nativity, group = plot)) + 
  geom_errorbar(aes(ymin=richness-sd, ymax=richness+sd), width =0, position = pd) +
  #geom_ribbon(aes(ymin=richness-sd, ymax=richness+sd, fill = fire_frequency),alpha = 0.2) +
  geom_line(aes(color = nativity), position=pd) +
  scale_color_discrete(name="native status") +
  xlab("Sites") +
  ylab("Richness") +
  theme_pubr()+
  facet_wrap(~site)+
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))

