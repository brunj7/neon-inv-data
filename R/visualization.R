## modelling native vs exotic diversity
## 

source("R/diversity_data_prep.R")
library(lmerTest)

## plotting number of species then doing a glmmm ----

plot_level %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x=nspp_native, y=nspp_exotic, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness") +
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr() +
# facet_wrap(~site, scales = "free") 
  ggsave("draft_figures/n_vs_e_nspp.png")

## plotting shannon diversity then doing an lmm -----
plot_level %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x=shannon_exotic, y=shannon_native, color = site)) +
  geom_point() +
  facet_wrap(~year, scales = "free") +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Shannon Diversity")+
  ylab("Native Diversity") +
  xlab("Exotic Diversity") +
  theme_pubr() +
  ggsave("draft_figures/n_vs_e_shannon.png")

# basic AF relating it to env variables

source("R/soil_chem.R")

soil_chem_byplot<- soil_cn %>%
  group_by(plotID) %>%
  summarise(nitrogen = mean(nitrogenPercent),
            carbon = mean(organicCPercent),
            CN = mean(soil_cn))

plot_level%>%
  left_join(soil_chem_byplot)%>%
  mutate(site = str_sub(plotID, 1,4))  %>%
  ggplot(aes(x=rel_cover_exotic, y=CN)) +
  geom_point(aes(color = site)) +
  geom_smooth(show.legend = F) +
  ylab("Soil C:N")+
  theme_pubr()


plot_level%>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x = rel_cover_exotic)) +
  geom_point(aes(y = shannon_native), color = "darkgreen") +
  geom_point(aes(y = shannon_exotic), color = "red") +
  geom_point(aes(y=shannon_total), color = "blue") +
  ylab("shannon diversity") +
  # facet_wrap(~year, scales="free")+
  # facet_wrap(~site, scales="free")+
  geom_smooth(show.legend = F, aes(y = shannon_total),color = "blue", se=T ,method="lm")+
  geom_smooth(show.legend = F, aes(y = shannon_native),color = "darkgreen", se=T ,method="lm") +
  geom_smooth(show.legend = F, aes(y = shannon_exotic),color = "red", se=T ,method="lm") +
  theme_pubr()

plot_level%>%
  ggplot(aes(x = rc_exotic_Poaceae)) +
  geom_point(aes(y = shannon_native), color = "darkgreen") +
  geom_point(aes(y = shannon_exotic), color = "red") +
  ylab("shannon diversity")+
  geom_point(aes(y=shannon_total), color = "blue") +
  # facet_wrap(~year, scales="free")+
  # facet_wrap(~site, scales="free")+
  geom_smooth(show.legend = F, aes(y = shannon_total),color = "blue", se=T ,method="lm")+
  geom_smooth(show.legend = F, aes(y = shannon_native),color = "darkgreen", se=T ,method="lm") +
  geom_smooth(show.legend = F, aes(y = shannon_exotic),color = "red", se=T ,method="lm") +
  theme_pubr()

plot_level %>%
  mutate(site = str_sub(plotID, 1,4))  %>%
  filter(rel_cover_exotic <1)%>%
  ggplot(aes(x=rel_cover_exotic, y=shannon_native, color = site)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  geom_smooth(se=F)

plot_level %>%
  ggplot(aes(x=rel_cover_exotic, y=shannon_total, color = site)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  geom_smooth(se=F)

plot_level %>%
  ggplot(aes(x=rc_exotic_Poaceae, y=shannon_total, color = site)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  geom_smooth(se=F)

plot_level %>%
  ggplot(aes(x=rc_exotic_Poaceae, y=shannon_native, color = site)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  geom_smooth(se=F)

plot_level %>%
  filter(site != "JORN")%>%
  ggplot(aes(x=rc_exotic_Poaceae)) +
  geom_point(alpha = 0.25,aes(y=shannon_exotic), color="red") +
  geom_point(alpha = 0.25,aes(y=shannon_native), color = "darkgreen") +
  geom_point(alpha = 0.25,aes(y=shannon_total),color = "blue") +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  ylab("shannon diversity")+
  geom_smooth(se=F,aes(y=shannon_exotic), color = "red")+
  geom_smooth(se=F,aes(y=shannon_native), color = "darkgreen")+
  geom_smooth(se=F,aes(y=shannon_total), color = "blue")

plot_level %>%
  filter(site != "JORN")%>%
  ggplot(aes(x=rc_native_Poaceae)) +
  geom_point(alpha = 0.25,aes(y=shannon_exotic), color="red") +
  geom_point(alpha = 0.25,aes(y=shannon_native), color = "darkgreen") +
  geom_point(alpha = 0.25,aes(y=shannon_total),color = "blue") +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  ylab("shannon diversity")+
  geom_smooth(se=F,aes(y=shannon_exotic), color = "red")+
  geom_smooth(se=F,aes(y=shannon_native), color = "darkgreen")+
  geom_smooth(se=F,aes(y=shannon_total), color = "blue")

# plot_level %>%
# get_diversity_info(cover4) %>%
get_diversity_info(cover8_1m2_10m2) %>%
  filter(site != "JORN")%>%
  ggplot(aes(x=rc_Poaceae)) +
  geom_point(alpha = 0.25,aes(y=shannon_exotic), color="red") +
  geom_point(alpha = 0.25,aes(y=shannon_native), color = "darkgreen") +
  geom_point(alpha = 0.25,aes(y=shannon_total),color = "blue") +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  ylab("shannon diversity")+
  geom_smooth(se=F,aes(y=shannon_exotic), color = "red")+
  geom_smooth(se=F,aes(y=shannon_native), color = "darkgreen")+
  geom_smooth(se=F,aes(y=shannon_total), color = "blue")


plot_level %>%
  filter(site != "JORN")%>%
  ggplot(aes(x=rc_Poaceae, y = rc_exotic_Poaceae)) +
  geom_point() +
  facet_wrap(~site)
