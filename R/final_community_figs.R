# final diversity/scale figures and models
source("R/diversity_data_prep.R")
library(lme4)
library(ggpubr)
library(car)

# data mongering ===============================================================

prev_year_div <- all_scales %>% 
  mutate(year = as.numeric(year)) %>%
  filter(nspp_exotic == 0) %>%
  dplyr::select(year, plotID, scale, subplotID, site,
                prev_shannon_total=shannon_total, 
                prev_nspp_total=nspp_total, 
                prev_shannon_native=shannon_native, 
                prev_nspp_native = nspp_native) %>%
  mutate(year = year+1) %>%
  left_join(all_scales%>% 
              mutate(year = as.numeric(year)),
            by = c("plotID", "subplotID", "year", "scale", "site"))

# models =======================================================================

m0<-all_scales %>%
  mutate(invaded = ifelse(invaded=="invaded",1,0)) %>%
  lme4::glmer(invaded ~ nspp_native * scale+ (1|site), 
              data = ., family = "binomial")

# this is the ggplot model, basically... not sure how to compare glm and glmer
m1<-all_scales %>%
  mutate(invaded = ifelse(invaded=="invaded",1,0)) %>%
  glm(invaded ~ nspp_native * scale, 
              data = ., family = "binomial")
#not sure what to think about this stuff
vif(m0)
vif(m1)
AIC(m0,m1)
anova(m0,m1)

prev_year_div%>%
  mutate(invaded = ifelse(invaded=="invaded",1,0), 
         prev_nspp_native = scale(prev_nspp_native)) %>%
  lme4::glmer(invaded ~ prev_nspp_native+scale +(1|site), 
              data = ., family = "binomial")%>%
  summary

prev_year_div%>%
  mutate(invaded = ifelse(invaded=="invaded",1,0), 
         prev_nspp_native = scale(prev_nspp_native)) %>%
  glm(invaded ~ prev_nspp_native+scale, 
              data = ., family = "binomial")%>%
  summary


# money plots ==================================================================
p2<-ggplot(all_scales %>% filter(invaded != 0) %>%
             mutate(invaded = ifelse(invaded=="invaded", 1,0)), 
           aes(x = nspp_native, y=invaded, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic() +
  xlab("Native species present") +
  ylab("P(Invaded)")


p1<-ggplot(prev_year_div %>% filter(invaded != 0) %>%
             mutate(invaded = ifelse(invaded=="invaded", 1,0)), 
           aes(x = prev_nspp_native, y=invaded, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()+
  xlab("Native species present in the previous year")+
  ylab("P(Invaded)")


panel<- ggarrange(p1,p2, common.legend = T, legend = "top")+
  ggsave("draft_figures/scale_invaded.png", height = 4, width =7)


# poisson glm of nspp ==========================================================

pp1<-all_scales %>%
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
