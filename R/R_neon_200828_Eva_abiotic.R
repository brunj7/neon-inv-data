# Eva Stricker code for Neon Invasion Vulnerability working group 191128

library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(car)
library(emmeans)
library(vegan)
detach(package:plyr)
library(httr)
library(RCurl)
library(dplyr)
library(ggthemes)
library(viridis) 

source("R/diversity_data_prep.R")

### Figure of site by grass cover through time- rc is boring so keep with absolute value. 
# plants <- read.csv(text = getURL("https://raw.githubusercontent.com/brunj7/neon-inv-data/master/data/plot_level_diversity_stuff.csv"))


# plants$site <- factor(substr(plants$site, 1, 4), levels = c("ONAQ", "MOAB", "SRER", "JORN"))

ggplot(plot_level %>%
         mutate(site_name = factor(lut_sites[site],
                                   levels = c("Onaqui", "Moab","Santa Rita",
                                              "Jornada"))), aes(x=year, 
                   y = cover_exotic, group = plotID, color = site_name))+
  geom_line(color="gray", size = .01)+
  geom_point()+
  geom_line(alpha=0.5)+
  facet_grid(site_name~.)+
  theme_bw() + 
  theme(axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(vjust=0.35) ,
        axis.title = element_text(size = 12),
        legend.position = "none",
        legend.justification = c(0,0),
        legend.title=element_text(size=11),
        axis.text = element_text(size = 10),
        legend.text=element_text(size=9),
        axis.ticks.x=element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank()  )+
  scale_color_viridis(discrete = TRUE, option = "D")+
  labs(y= "Exotic Cover (%)", x = "Year")+
  ylim(0,35) +
  ggsave("draft_figures/exotic_bysite.png", width=7.5, height=5)



##### N deposition

Ndepnh4 <- read.csv("data/ndep.csv")
# head(Ndepnh4)



soilN<- read.csv(text = getURL("https://raw.githubusercontent.com/brunj7/neon-inv-data/master/Data_merged/soil_cn_periodic_w.csv"))
# head(soilN)
# sort(soilN$nitrogenPercent)
soilN$Year<-format(as.Date(soilN$collectDate, format = "%Y-%m-%d"), "%Y")
soilN$year <- as.numeric(soilN$Year)
SN <- soilN %>% group_by(plotID, year) %>%
  summarize(meanNper = mean(nitrogenPercent, na.rm=T)) 

plot_level <- plot_level %>%
  mutate(year = as.numeric(year)) %>%
  left_join(SN, by = c('plotID' = 'plotID', 'year'= 'year'))
# head(plot_level)


# ggplot(filter(plot_level, year == "2016"), aes(x=year, y=meanNper))+
#   geom_point()+
#   facet_grid(.~site)
# hist(asin(sqrt(plot_level$cover_exotic)))

sub<- filter(plot_level, year == "2016")

mod1<-lm(log(cover_exotic_Poaceae+1)~meanNper*site , data = sub)
# car::Anova(mod1, type = 3)

# emtrends(mod1, var = "meanNper")

# pairs(emtrends(mod1, var = "meanNper", "site"), adjust = "fdr")

plot_level <- plot_level %>%
  mutate(site_name = factor(lut_sites[site],
                            levels = c("Onaqui", "Moab","Santa Rita",
                                       "Jornada")))

# hist(resid(mod1))
p1<-ggplot(filter(plot_level, year == "2016"),
       aes(x = meanNper, y = cover_exotic, group = site_name))+
  geom_point(aes(color = site_name))+
  theme_bw() + 
  # facet_wrap(~site_name, scales = "free", nrow=1) +
  theme(axis.title.x = element_text(vjust=-0.35),
        axis.title.y = element_text(vjust=0.35) ,
        axis.title = element_text(size = 12),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title=element_blank(),
        axis.text = element_text(size = 10),
        legend.text=element_text(size=9),
        axis.ticks.x=element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank())+
  geom_smooth(method = "lm", aes(color = site_name), se=F)+
  scale_x_continuous(n.breaks = 4)+
  # ggtitle("2016")+
  scale_color_viridis(discrete = TRUE, option = "D")+
  labs(x="soil N (%), 2016", y = "Exotic cover (%)")
  

  
  site <- plot_level %>% group_by(site, year) %>%
    summarize(cover_exotic = mean(cover_exotic, na.rm=T))
  
  # head(Ndepnh4)
  site_level <- left_join(site, Ndepnh4, by = c('site' = 'seas', 'year'= 'yr'))%>%
  mutate(site = factor(site, 
                          levels = c("ONAQ", "MOAB", "SRER", "JORN")))%>%
  mutate(site_name = factor(lut_sites[site],
                            levels = c("Onaqui", "Moab","Santa Rita",
                                       "Jornada")))


# hist(log(site_level$cover_exotic+1))
# unique(site_level$year)
mod1 <- lmerTest::lmer(log(cover_exotic+1)~ site*totalN +(1|year),
                       data = site_level)
# anova(mod1)
# no effect on exotic cover of N deposition; small scale matters more.


site_level$site <- factor(substr(site_level$site, 1, 4),
                          levels = c("ONAQ", "MOAB", "SRER", "JORN"))

p2<-ggplot(site_level,aes(x = totalN, y = cover_exotic, group = site))+
  geom_point(aes(color = site_name))+
  theme_bw() + 
  # facet_wrap(~site_name, scales = "free", nrow=1) +
  theme(  axis.title.x = element_text(vjust=-0.35),
          axis.title.y = element_text(vjust=0.35) ,
          axis.title = element_text(size = 12),
          legend.position = "none",
          legend.title=element_blank(),
          axis.text = element_text(size = 10),
          legend.text=element_text(size=9),
          axis.ticks.x=element_blank(),
          legend.key = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_blank()  )+
  geom_smooth(method = "lm", aes(color = site_name), se=F)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  # scale_color_colorblind()+
  # ggtitle("2014-2018")+
  labs(x=expression(paste ("N deposition (kg m"^2," y"^-1,")", ", 2014-2018")), 
       y = "Exotic cover (%)")

                                          
ggpubr::ggarrange(p1, p2, nrow=2, ncol=1) +
  ggsave("draft_figures/n_vs_exotics.png", 
         height=5, width=7, bg="white")

# expression('Mean annual Q,  m'^"3"*' s'^"-1")
