## modelling native vs exotic diversity
## 

source("R/diversity_data_prep.R")
source("R/soil_chem.R")
library(lmerTest)

## plotting number of species then doing a glmmm ----

all_scales %>%
  ggplot(aes(x=nspp_exotic, y=nspp_native, color = scale)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = "poisson"),
              show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness, (Poisson glm)") +
  ylab("Native Species") +
  xlab("Exotic Species") +
  theme_pubr() +
  # facet_wrap(~site) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1)) +
  ggsave("draft_figures/n_vs_e_nspp.png")

all_scales %>%
  # filter(scale == "1m") %>%
  glm(nspp_native ~ nspp_exotic*scale, family = "poisson",
              data = .) %>% 
  summary

all_scales %>%
  # filter(scale == "1m") %>%
  lme4::glmer(nspp_native ~ nspp_exotic*scale + (1|site), family = "poisson",
      data = .) %>% 
  summary

## plotting shannon diversity then doing an lmm -----
all_scales %>%
  ggplot(aes(x=shannon_exotic, y=shannon_native, color = scale)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Shannon Diversity")+
  ylab("Native Diversity") +
  xlab("Exotic Diversity") +
  theme_pubr() +
  ggsave("draft_figures/n_vs_e_shannon.png")

# basic AF relating it to env variables


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

all_scales%>%
  ggplot(aes(x = rc_exotic_Poaceae)) +
  geom_point(aes(y = shannon_native), color = "darkgreen") +
  geom_point(aes(y = shannon_exotic), color = "red") +
  ylab("shannon diversity")+
  geom_point(aes(y=shannon_total), color = "blue") +
  facet_wrap(~scale, scales="free")+
  # facet_wrap(~site, scales="free")+
  geom_smooth(show.legend = F, aes(y = shannon_total),color = "blue", se=T ,method="loess")+
  geom_smooth(show.legend = F, aes(y = shannon_native),color = "darkgreen", se=T ,method="loess") +
  geom_smooth(show.legend = F, aes(y = shannon_exotic),color = "red", se=T ,method="loess") +
  theme_pubr()

plot_level %>%
  mutate(site = str_sub(plotID, 1,4))  %>%
  filter(rel_cover_exotic <1)%>%
  ggplot(aes(x=rel_cover_exotic, y=shannon_native, color = site)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  geom_smooth(se=F)

# rc vs shannon ================================================================

ggarrange(
  all_scales %>%
    filter(site!="JORN") %>%
    ggplot(aes(x=rel_cover_exotic, y=shannon_total, color = scale)) +
    geom_point(alpha = 0.25) +
    facet_wrap(~site, scales = "free_y")+
    theme_pubr()+
    geom_smooth(se=F) +
    xlab("Relative Cover of All Exotic Species")+
    ylab("Shannon Diversity")
  ,
  all_scales %>%
    filter(site!="JORN") %>%
    ggplot(aes(x=rc_exotic_Poaceae, y=shannon_total, color = scale)) +
    geom_point(alpha = 0.25) +
    facet_wrap(~site)+
    theme_pubr()+
    geom_smooth(se=F)+
    ylab("Shannon Diversity") +
    xlab("Relative Cover of Exotic Grasses")
  , common.legend = TRUE, nrow=2) +
  ggsave('draft_figures/faceted_shannon_vs_ex_p_smoothers.png')
# poisson rc vs nspp ===========================================================
ggarrange(
all_scales %>%
  filter(site!="JORN") %>%
  ggplot(aes(x=rel_cover_exotic, y=nspp_total, color = scale)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  geom_smooth(se = F)
,
all_scales %>%
  filter(site!="JORN") %>%
  ggplot(aes(x=rc_exotic_Poaceae, y=nspp_total, color = scale)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  geom_smooth(se = F)
, common.legend = TRUE, nrow=2)

# more all scale stuff ========================================================
plot_level %>%
  ggplot(aes(x=rc_exotic_Poaceae, y=shannon_native, color = site)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~site, scales = "free_y")+
  theme_pubr()+
  geom_smooth(se=F)

all_scales %>%
  filter(site != "JORN")%>%
  ggplot(aes(x=rc_native_Poaceae)) +
  geom_point(alpha = 0.25,aes(y=nspp_exotic), color="red") +
  geom_point(alpha = 0.25,aes(y=nspp_native), color = "darkgreen") +
  geom_point(alpha = 0.25,aes(y=nspp_total),color = "blue") +
  facet_wrap(~scale, scales = "free_y")+
  theme_pubr()+
  ylab("shannon diversity")+
  geom_smooth(se=F,aes(y=nspp_exotic), color = "red")+
  geom_smooth(se=F,aes(y=nspp_native), color = "darkgreen")+
  geom_smooth(se=F,aes(y=nspp_total), color = "blue")

all_scales %>%
  filter(site != "JORN")%>%
  ggplot(aes(x=rc_native_Poaceae)) +
  geom_point(alpha = 0.25,aes(y=shannon_exotic), color="red") +
  geom_point(alpha = 0.25,aes(y=shannon_native), color = "darkgreen") +
  geom_point(alpha = 0.25,aes(y=shannon_total),color = "blue") +
  facet_wrap(~scale, scales = "free_y")+
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

# time_series =================================================================

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x=year, y=nspp_exotic/nspp_total)) +
    facet_wrap(~site, scales="free_y") +
    geom_point() +
    geom_smooth()+
    geom_line(aes(group = plotID)) +
    theme_classic()

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x=year, y=rel_cover_exotic)) +
  facet_wrap(~site) +
  geom_point() +
  geom_smooth()+
  geom_line(aes(group = plotID)) +
  theme_classic()

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x=year, y=rc_exotic_Poaceae)) +
  facet_wrap(~site) +
  geom_point() +
  geom_smooth()+
  geom_line(aes(group = plotID)) +
  theme_classic()
