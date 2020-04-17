# just looking at grass through time

source("R/diversity_data_prep.R")
ggplot2::theme_set(theme_classic())

core_sites<- read_csv("data/core-field-sites.csv")

just_sites<- core_sites%>%
  filter(`Site Type` == "Core Terrestrial") %>%
  dplyr::select(`Site ID`) %>%
  pull()

# if statement helps avoid downloading over and over
if(!file.exists("data/diversity_all.RDS")){
  loadByProduct(dpID = "DP1.10058.001", 
                site = just_sites, 
                check.size = F) -> x
  saveRDS(x, "data/diversity_all.RDS")}else{
    x<-readRDS("data/diversity_all.RDS")}

plot_level <- get_diversity_info(x,scale = "plot",families = "Poaceae")

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x=year, y=rc_Poaceae)) +
  #geom_point(aes(color=site, group=plotID)) +
  geom_line(aes(color=site, group=plotID), alpha=0.35)+
  geom_smooth(aes(group=site, color = site)) +
  facet_wrap(~site, scales="free") +
  theme(legend.position = "none")

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x=year, y=rc_exotic_Poaceae)) +
  #geom_point(aes(color=site, group=plotID)) +
  geom_line(aes(color=site, group=plotID), alpha=0.35)+
  geom_smooth(aes(group=site, color = site)) +
  facet_wrap(~site, scales="free") +
  theme(legend.position = "none")

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x=year, y=cover_Poaceae)) +
  #geom_point(aes(color=site, group=plotID)) +
  geom_line(aes(color=site, group=plotID), alpha=0.35)+
  geom_smooth(aes(group=site, color = site)) +
  facet_wrap(~site, scales="free") +
  theme(legend.position = "none")

plot_level %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x=year, y=cover_exotic_Poaceae)) +
  #geom_point(aes(color=site, group=plotID)) +
  geom_line(aes(color=site, group=plotID), alpha=0.35)+
  geom_smooth(aes(group=site, color = site)) +
  facet_wrap(~site, scales="free") +
  theme(legend.position = "none")
