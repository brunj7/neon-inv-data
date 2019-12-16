## modelling native vs exotic diversity
## 

source("R/diversity_data_prep.R")
library(lmerTest)

## plotting number of species then doing a glmmm ----

vegan_friendly_div %>%
  dplyr::select(plotID, year, endDate, nativeStatusCode, nspp) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = nspp) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x=N, y=I, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr() +
# facet_wrap(~site, scales = "free") 
  ggsave("draft_figures/n_vs_e_nspp.png")

vegan_friendly_div %>%
  dplyr::select(plotID, year, endDate, nativeStatusCode, nspp) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = nspp) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  lme4::glmer(I ~ N + (1|site), data = ., family = "poisson") %>% 
  summary

## plotting shannon diversity then doing an lmm -----
vegan_friendly_div %>%
  dplyr::select(plotID, year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x=N, y=I, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Shannon Diversity")+
  xlab("Native Diversity") +
  ylab("Exotic Diversity") +
  theme_pubr() +
  ggsave("draft_figures/n_vs_e_shannon.png")

vegan_friendly_div %>%
  dplyr::select(plotID, year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  lmerTest::lmer(I~N + (1|site), data=.) %>%
  summary

# trying to do what someone wrote in the google doc -----
# rescaling needed
vegan_friendly_div %>%
  dplyr::select(plotID, year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  lmerTest::lmer(I~N*site*endDate + (1|plotID), data=.) %>%
  summary


# basic AF relating it to env variables

source("R/soil_chem.R")

soil_chem_byplot<- soil_cn %>%
  group_by(plotID) %>%
  summarise(nitrogen = mean(nitrogenPercent),
            carbon = mean(organicCPercent),
            CN = mean(soil_cn))

vegan_friendly_div %>%
  dplyr::select(plotID, year, endDate, nativeStatusCode, shannon) %>%
  # pivot_wider(names_from = nativeStatusCode,
  #             values_from = shannon) %>%
  left_join(soil_chem_byplot )%>%
  left_join(n_i_rel_cover) %>%
  mutate(site = str_sub(plotID, 1,4))  %>%
  ggplot(aes(x=endDate, y=shannon)) +
  geom_point(aes(color = nativeStatusCode)) +
  geom_line(alpha = 0.2,aes(color = nativeStatusCode,group=paste(plotID, nativeStatusCode)))+
  facet_wrap(~site, scales="free_y")+
  geom_smooth(show.legend = F, aes(color = nativeStatusCode), se=F) +
  theme_pubr()

# so clearly there can't be 200% relative cover so something's messed up with 
# relative exotic cover
vegan_friendly_div %>%
  dplyr::select(plotID, year, endDate, nativeStatusCode, shannon,nspp) %>%
  # pivot_wider(names_from = nativeStatusCode,
  #             values_from = shannon) %>%
  left_join(soil_chem_byplot )%>%
  left_join(n_i_rel_cover) %>%
  mutate(site = str_sub(plotID, 1,4))  %>%
  ggplot(aes(x=rel_cover_exotic, y=nspp)) +
  geom_point(aes(color = nativeStatusCode)) +
  #geom_line(alpha = 0.2,aes(color = nativeStatusCode,group=paste(plotID, nativeStatusCode)))+
  #facet_wrap(~site, scales="free")+
  geom_smooth(show.legend = F, aes(color = nativeStatusCode), se=T ,method="lm") +
  theme_pubr()

# significant
vegan_friendly_div %>%
  dplyr::select(plotID, year, endDate, nativeStatusCode, shannon,nspp) %>%
  # pivot_wider(names_from = nativeStatusCode,
  #             values_from = shannon) %>%
  left_join(soil_chem_byplot )%>%
  left_join(n_i_rel_cover) %>%
  mutate(site = str_sub(plotID, 1,4))  %>%
  glmer(nspp~rel_cover_exotic*nativeStatusCode+(1|plotID),data= ., family = "poisson") %>%
  summary()

plot_level %>%
  mutate(site = str_sub(plotID, 1,4))  %>%
  filter(rel_cover_exotic <1)%>%
  ggplot(aes(x=rel_cover_exotic, y=shannon_native, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free")+
  theme_pubr()+
  geom_smooth(se=F)
