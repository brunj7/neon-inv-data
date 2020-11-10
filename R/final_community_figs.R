# final diversity/scale figures and models
source("R/diversity_data_prep.R")
library(lme4)
library(ggpubr)
library(ggthemes)
library(car)
library(ggsci)

# data mongering ===============================================================

uninvaded_sites <- all_scales %>% 
  mutate(year = as.numeric(year),
         uniqueid = paste0(year+1,plotID,scale,subplotID, site)) %>%
  filter(nspp_exotic == 0) %>% 
  dplyr::select(year, plotID, scale, subplotID, site,uniqueid, 
                shannon_total, nspp_total, shannon_native, nspp_native)

uniqueids <- uninvaded_sites$uniqueid

next_year<-all_scales %>% 
  mutate(year = as.numeric(year),
         uniqueid = paste0(year,plotID,scale,subplotID, site))%>%
  filter(uniqueid %in% uniqueids) %>%
  dplyr::select(uniqueid, next_shannon_total=shannon_total, 
                next_nspp_total=nspp_total, 
                next_nspp_exotic = nspp_exotic,
                next_shannon_native=shannon_native, 
                next_nspp_native = nspp_native,
                next_shannon_exotic = shannon_exotic) %>%
  mutate(invaded = ifelse(next_nspp_exotic > 0, 1, 0))

prev_year_div <- left_join(next_year, uninvaded_sites)

# binomial models ==============================================================

ma0<-all_scales %>%
  mutate(invaded = ifelse(invaded=="invaded",1,0)) %>%
  lme4::glmer(invaded ~ nspp_native * scale+ (1|site), 
              data = ., family = "binomial")

# this is the ggplot model, basically... not sure how to compare glm and glmer
ma1<-all_scales %>%
  mutate(invaded = ifelse(invaded=="invaded",1,0)) %>%
  glm(invaded ~ nspp_native * scale, 
              data = ., family = "binomial")
#not sure what to think about this stuff
vif(m0)
vif(m1)
AIC(m0,m1)
anova(m0,m1)

mc1<- prev_year_div %>%
  lme4::glmer(invaded ~ nspp_native*scale +(1|site), 
              data = ., family = "binomial")

mc0<-prev_year_div%>%
  mutate(nspp_native = scale(nspp_native)) %>%
  glm(invaded ~ nspp_native*scale, 
              data = ., family = "binomial")

# inv_mod<- lme4::glmer(invaded ~ nspp_native*scale +(1|site), 
#               data = prev_year_div, family = "binomial")
# 
# library(effects)
# plot(allEffects(inv_mod))

# count models =================================================================

# no convergence
# mb0<-all_scales %>%
#   lme4::glmer(nspp_exotic ~ nspp_native * scale+ (1|site), 
#               data = ., family = "poisson")

mb0<-all_scales %>%
  glm(nspp_exotic ~ nspp_native * scale, 
      data = ., family = "quasipoisson")
#not sure what to think about this stuff
summary(mb0)
md0<-prev_year_div%>%
  glm(next_nspp_exotic ~ nspp_native*scale, 
      data = ., family = "quasipoisson")
summary(md0)
Anova(md0)


# money plots ==================================================================
p1<-ggplot(all_scales %>% 
             mutate(invaded = ifelse(invaded=="invaded", 1,0)), 
           aes(x = nspp_native, y=invaded, color = scale)) +
  # geom_point(alpha=0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))+
  scale_color_viridis_d(option = "B") +
  xlab("Native Species Richness") +
  ylab("P(Invaded)") +
  ggsave("draft_figures/p_invaded.png");p1

# glm(nspp_native ~ nspp_exotic*scale, dat=all_scales, family = "quasipoisson") %>% summary()
# nb_mod<-MASS::glm.nb(nspp_native ~ nspp_exotic*scale, dat=all_scales)

p2<-all_scales %>%
  ggplot(aes(x=nspp_native, y=nspp_exotic, color = scale)) +
  # geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "quasipoisson"),
              show.legend = F) +
  # ggtitle("Native vs. Exotic Species Richness, (Quasipoisson glm)") +
  ylab("Exotic Species Richness") +
  xlab("Native Species Richness") +
  theme_pubr() +
  scale_color_viridis_d(option = "B") +
  # facet_wrap(~site) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1)) +
  ggsave("draft_figures/n_vs_e_nspp.png")


p3<-ggplot(prev_year_div, 
           aes(x = nspp_native, y=invaded, color = scale)) +
  # geom_point(alpha=0.5)+
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()+
  theme(legend.position = "none")+
  #geom_line(aes(y=predict(inv_mod,type="response", re.form=NA)))+
  scale_color_viridis_d(option = "B") +
  xlab("Native species Richness (Uninvaded Sites, Year 1)")+
  ylab("P(Invaded); Year 2")

p4<-ggplot(prev_year_div, 
           aes(x = nspp_native, y=next_nspp_exotic, color = scale)) +
  # geom_point(alpha=0.5)+
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_viridis_d(option = "B") +
  geom_hline(yintercept = 1, lty=2, color = "grey80")+
  scale_y_continuous(breaks = c(0,1,2,4,6,8))+
  xlab("Native Species Richness (Uninvaded Sites, Year 1)")+
  ylab("Exotic Species Richness; Year 2")




panel<- ggarrange(p2,p1,p4,p3, labels="auto", label.x = 0.15) +
  ggsave("draft_figures/scale_invaded.png", height = 8.5, width =8.5, bg="white")


# poisson glm of nspp ==========================================================


# nspp grasses
pp2<-all_scales %>%
  ggplot(aes(x=nspp_Poaceae_I, y=nspp_native, color = scale)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"),
              show.legend = F) +
  ggtitle("Native vs. Exotic Grass Species Richness") +
  ylab("Native Species") +
  xlab("Exotic Grass Species") +
  theme_pubr() +
  # facet_wrap(~site) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1)) +
  ggsave("draft_figures/n_vs_e_grass_nspp.png")

pp_panel <- ggarrange(pp1,pp2, common.legend = TRUE) +
  ggsave("draft_figures/nspp_panel.png", height=5, width=10)

## exotic grasses monitoring ===================================================
first_year_invaded <- get_longform_cover(x) %>%
  filter(family == "Poaceae") %>%
  filter(nativeStatusCode == "I") %>% 
  mutate(year = as.numeric(year)) %>%
  group_by(site, plotID) %>%
  summarise(first_year = min(year),
            number_of_years_invaded = length(unique(year)),
            number_of_invasive_spp = length(unique(taxonID))) %>%
  ungroup() %>%
  left_join(plot_level %>% mutate(year = as.numeric(year)),
            by = c("plotID", "site"))

fyp <- first_year_invaded %>%
  group_by(site, plotID, nspp_native) %>%
  summarise(first_year = first(first_year)) %>%
  ungroup() %>%
  dplyr::rename(nspp_thatyear = nspp_native)

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year, site) %>%
  mutate(median_nspp = median(nspp_native)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y=nspp_native)) +
  facet_wrap(~site) +
  geom_line(aes(group = plotID), alpha = 0.5) +
  geom_line(aes(y=median_nspp,group = plotID), lwd = 1) +
  geom_point(data =fyp, aes(x=first_year, y=nspp_thatyear), color="red")+
  theme_classic() +
  ylab("Native Species Richness") +
  xlab("Year") +
  ggtitle("Grass Invasion Detections", 
          "Red dots indicate the first year an exotic grass was found at a plot") +
  ggsave("draft_figures/invasion_detection.png", height = 5.5, width = 7)

## two species ==============================================================
erle <- get_longform_cover(readRDS("data/diversity.RDS")) %>%
  filter(taxonID == "ERLE") %>%
  mutate(year = as.numeric(year)) %>%
  group_by(site, plotID) %>%
  summarise(first_year = min(year),
            number_of_years_invaded = length(unique(year)),
            number_of_invasive_spp = length(unique(taxonID))) %>%
  ungroup() %>%
  left_join(plot_level %>% mutate(year = as.numeric(year)),
            by = c("plotID", "site"))%>%
  group_by(site, plotID, nspp_native) %>%
  summarise(first_year = first(first_year)) %>%
  ungroup() %>%
  dplyr::rename(nspp_thatyear = nspp_native)

brte <- get_longform_cover(readRDS("data/diversity.RDS")) %>%
  filter(taxonID == "BRTE") %>%
  mutate(year = as.numeric(year)) %>%
  group_by(site, plotID) %>%
  summarise(first_year = min(year),
            number_of_years_invaded = length(unique(year)),
            number_of_invasive_spp = length(unique(taxonID))) %>%
  ungroup() %>%
  left_join(plot_level %>% mutate(year = as.numeric(year)),
            by = c("plotID", "site"))%>%
  group_by(site, plotID, nspp_native) %>%
  summarise(first_year = first(first_year)) %>%
  ungroup() %>%
  dplyr::rename(nspp_thatyear = nspp_native)

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year, site) %>%
  mutate(median_nspp = median(nspp_native)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y=nspp_native)) +
  facet_wrap(~site) +
  geom_line(aes(group = plotID), alpha = 0.5) +
  geom_line(aes(y=median_nspp,group = plotID), lwd = 1) +
  geom_point(data =brte, aes(x=first_year, y=nspp_thatyear), color="red")+
  geom_point(data =erle, aes(x=first_year, y=nspp_thatyear), color="blue")+
  theme_classic() +
  ylab("Native Species Richness") +
  xlab("Year") +
  ggtitle("Species Invasion Detections", 
          paste0("Red dots indicate the first year cheatgrass was found at a plot\n",
                 "Blue dots indicate the first year Lehman's lovegrass was found at a plot")) +
  ggsave("draft_figures/invasion_detection_BRTE_ERLE.png", height = 5.5, width = 7)
