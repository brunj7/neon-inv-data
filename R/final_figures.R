# all figures

# setup ========================================================================

library(neonUtilities)
library(tidyverse)
library(sf)
library(ggpubr)
library(car)
library(emmeans)
library(vegan)
library(httr)
library(RCurl)
library(ggthemes)
library(viridis) 
library(lme4)
library(car)

if(!exists("prepped")) source("R/diversity_data_prep.R"); prepped = TRUE

# figure 1 =====================================================================
plot_level %>%
  mutate(site_name = factor(lut_sites[site],
                            levels = c("Moab","Onaqui","Jornada", "Santa Rita"))) %>%
  
       ggplot(aes(x=year, 
           y = cover_exotic, group = plotID, color = site_name))+
  geom_line(color="gray", size = .01)+
  geom_point()+
  geom_line(alpha=0.5)+
  facet_grid(site_name~., )+
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
  labs(y= "Non-Native Plant Cover (%)", x = "Year")+
  ylim(0,35) +
  ggsave("draft_figures/exotic_bysite.png", width=7.5, height=5) +
  ggsave("final_figures/figure_1_exotic_x_site.pdf", width = 4.5, height =5)

# figure 2 =====================================================================

# data mongering 

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

# plots
p1<-ggplot(all_scales %>% 
             mutate(invaded = ifelse(invaded=="invaded", 1,0)), 
           aes(x = nspp_native, y=invaded, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        panel.border = element_rect(fill = NA, size=0.75),
        legend.background = element_rect(fill="transparent"))+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1)) +
  scale_color_viridis_d(option = "B") +
  xlab("Native Species Richness") +
  ylab("P(Invaded)") +
  ggsave("draft_figures/p_invaded.png");p1

p2<-all_scales %>%
  ggplot(aes(x=nspp_native, y=nspp_exotic, color = scale)) +
  geom_smooth(method = "glm", 
              method.args = list(family = "quasipoisson"),
              show.legend = F) +
  ylab("Non-Native Species Richness") +
  xlab("Native Species Richness") +
  theme_classic() +
  scale_color_viridis_d(option = "B") +
  theme(legend.position = c(1,1),
        panel.border = element_rect(fill = NA, size=0.75),
        legend.justification = c(1,1)) +
  ggsave("draft_figures/n_vs_e_nspp.png")


p3<-ggplot(prev_year_div, 
           aes(x = nspp_native, y=invaded, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()+
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA,size=0.75),)+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1)) +
  scale_color_viridis_d(option = "B") +
  xlab("Native species Richness\n(Uninvaded Sites, Year 1)")+
  ylab("P(Invaded), Year 2")

p4<-ggplot(prev_year_div, 
           aes(x = nspp_native, y=next_nspp_exotic, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson")) +
  theme_classic()+
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, size=0.75))+
  scale_color_viridis_d(option = "B") +
  geom_hline(yintercept = 1, lty=2, color = "grey80")+
  scale_y_continuous(breaks = c(0,1,2,4,6,8))+
  xlab("Native Species Richness\n(Uninvaded Sites, Year 1)")+
  ylab("Non-Native Species Richness, Year 2")

panel<- ggarrange(p2,p1,p4,p3, labels="auto", label.x = 0.15, label.y = 0.97) +
  ggsave("draft_figures/scale_invaded.png", height = 7, width =6.5, bg="white") +
  ggsave("final_figures/figure_2_scale.pdf", height = 7, width = 6.5, bg="white")

# figure 3 =====================================================================

### set crs for all data layers: Albers Equal Area
crs1b <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'

# reading in the locations
# x is the neon diversity object from the data prep script
cover_sf2016 <- x$div_1m2Data %>%
  dplyr::select(decimalLongitude, 
                decimalLatitude,
                endDate, plotID,
                siteID) %>%
  mutate(year = str_sub(endDate, 1,4))%>%
  group_by(decimalLongitude, 
           decimalLatitude,
           plotID,
           siteID, year) %>%
  summarise()%>%
  ungroup()%>%
  st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = 4326) %>%
  st_transform(crs1b)%>%
  filter(year == "2016")

# split by site to get the distance to road info
cover_sf2016onaq <- cover_sf2016 %>%
  filter(siteID == "ONAQ")
cover_sf2016moab <- cover_sf2016 %>%
  filter(siteID == "MOAB")
cover_sf2016srer <- cover_sf2016 %>%
  filter(siteID == "SRER")
cover_sf2016jorn <- cover_sf2016 %>%
  filter(siteID == "JORN")

# bring in roads data
unzip(zipfile = "data/NEON_roads_data-20200130T165308Z-001.zip",
      exdir = "data")
road_path <- "data/NEON_roads_data/"

onaq_roads_shp <- st_read(file.path(road_path), layer = '15_C_ONAQ_Roads') %>%
  st_transform(.,crs1b)
moab_roads_shp <- st_read(file.path(road_path), layer = '13_R_MOAB_Roads') %>%
  st_transform(.,crs1b) 
srer_roads_shp <- st_read(file.path(road_path), layer = '14_C_SRER_Roads') %>%
  st_transform(.,crs1b)
jorn_roads_shp <- st_read(file.path(road_path), layer = '14_R_JORN_Roads') %>%
  st_transform(.,crs1b)

# calculating distances

cover_sf2016onaq <- cover_sf2016onaq %>%
  mutate(dist_to_road = sf::st_distance(cover_sf2016onaq, 
                                        onaq_roads_shp) %>%
           apply(1, min))
cover_sf2016jorn <- cover_sf2016jorn %>%
  mutate(dist_to_road = sf::st_distance(cover_sf2016jorn, 
                                        jorn_roads_shp) %>%
           apply(1, min))
cover_sf2016moab <- cover_sf2016moab %>%
  mutate(dist_to_road = sf::st_distance(cover_sf2016moab, 
                                        moab_roads_shp) %>%
           apply(1, min))
cover_sf2016srer <- cover_sf2016srer %>%
  mutate(dist_to_road = sf::st_distance(cover_sf2016srer, 
                                        srer_roads_shp) %>%
           apply(1, min))

# SANITY CHECKS
# plot(onaq_roads_shp[0])
# plot(cover_sf2016onaq["dist_to_road"], add=T)
# plot(srer_roads_shp[0])
# plot(cover_sf2016srer["dist_to_road"], add=T)
# plot(moab_roads_shp[0])
# plot(cover_sf2016moab["dist_to_road"], add=T)
# plot(jorn_roads_shp[0])
# plot(cover_sf2016jorn["dist_to_road"], add=T)

dist_allsites <- bind_rows(cover_sf2016onaq, cover_sf2016moab, 
                           cover_sf2016srer, cover_sf2016jorn) %>%
  st_set_geometry(NULL) %>%
  dplyr::select(plotID, dist_to_road) 

coverdist_allsites <- left_join(plot_level, dist_allsites, by = "plotID") %>%
  filter(year == 2016) %>%
  mutate(site = factor(site, levels = c("MOAB","ONAQ", "JORN",  "SRER")),
         site_name = factor(lut_sites[site],levels = c("Moab","Onaqui", "Jornada", 
                                                       "Santa Rita")))
# the function to make the colors transparent
pp1<-ggplot(coverdist_allsites, aes(x=dist_to_road, y=nspp_exotic, color = site_name)) +
  geom_point(size = 2, alpha=0.7) +
  geom_rug(sides = "b", 
           alpha = 0.7, 
           position = "jitter", length = unit(0.02, "npc")) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se=TRUE, aes(color = site_name))+
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_classic() +
  labs(x ="Distance to Nearest Road (m)",
       y = "Richness of Non-Native Species",
       color = "Sites")+
  theme(
    axis.title.x = element_text(vjust=-0.35),
    axis.title.y = element_text(vjust=0.35) ,
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA)
  ) 

pp3<-ggplot(coverdist_allsites, aes(x=dist_to_road, y=cover_exotic, color = site_name)) +
  geom_point(size = 2, alpha=0.7) +
  geom_rug(sides = "b", 
           alpha = 0.7, 
           position = "jitter", length = unit(0.02, "npc")) +
  geom_smooth(method = "lm", 
              # method.args = list(family = "poisson"), 
              se=TRUE,
              aes(color = site_name))+
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_classic() +
  labs(x ="Distance to Nearest Road (m)", 
       y = "Cover of Non-Native Species",
       color = "Sites")+
  ylim(0, NA)+
  theme(
    axis.title.x = element_text(vjust=-0.35),
    axis.title.y = element_text(vjust=0.35) ,
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill=NA)
  )

ggarrange(pp1, pp3, common.legend = T, labels = "auto", 
          label.x = 0.91, label.y = 0.95, nrow=1)+
  ggsave("draft_figures/dist_road_by_site_2pan.png",
         width=7.5, height=3.5,bg="white")+
  ggsave("final_figures/figure_3_roads.pdf",
         width=7.5, height=3.5,bg="white")

# figure 4 =====================================================================
# Eva Stricker code for Neon Invasion Vulnerability working group 191128

Ndepnh4 <- read.csv("data/ndep.csv")

SN<- read_csv("Data_merged/soil_cn_periodic_w.csv")%>%
  mutate(year=format(as.Date(collectDate, format = "%Y-%m-%d"), "%Y")%>%
           as.numeric)%>% 
  group_by(plotID, year) %>%
  dplyr::summarize(meanNper = mean(nitrogenPercent, na.rm=T)) 

plot_level <- plot_level %>%
  mutate(year = as.numeric(year)) %>%
  left_join(SN, by = c('plotID','year'))%>%
  mutate(site_name = factor(lut_sites[site],
                            levels = c("Moab","Onaqui","Santa Rita","Jornada")))

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
  labs(x="soil N (%), 2016", y = "Non-Native cover (%)")



site <- plot_level %>% group_by(site, year) %>%
  summarize(cover_exotic = mean(cover_exotic, na.rm=T))

site_level <- left_join(site, Ndepnh4, by = c('site' = 'seas', 'year'= 'yr'))%>%
  mutate(site = factor(site, 
                       levels =  c("MOAB","ONAQ", "JORN",  "SRER")))%>%
  mutate(site_name = factor(lut_sites[site],
                            levels = c("Moab","Onaqui",
                                       "Jornada", "Santa Rita"))) 

site_level$site <- factor(substr(site_level$site, 1, 4),
                          levels = c("MOAB","ONAQ", "JORN",  "SRER"))

p2<-ggplot(site_level,aes(x = totalN, y = cover_exotic, group = site))+
  geom_point(aes(color = site_name))+
  theme_bw() + 
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
  labs(x=expression(paste ("N deposition (kg m"^2," y"^-1,")", ", 2014-2018")), 
       y = "Non-Native cover (%)")


ggpubr::ggarrange(p1, p2, nrow=2, ncol=1) +
  ggsave("draft_figures/n_vs_exotics.png", 
         height=5, width=7, bg="white") +
  ggsave("final_figures/figure_4_n.pdf", height=5, width =7, bg = "white" )

# figure 5 =====================================================================

leaves <- read.csv("data/magnitude_phenometrics_data.csv")%>%
  filter(magnitude_phenometrics_data$Phenophase_Description=='Leaves (grasses)')%>%
  arrange(Start_Date)

# load the time series data but replace the csv filename with whatever you downloaded
df_sh <- read.table("data/NEON.D15.ONAQ.DP1.00042_SH_1001_3day.csv", header = TRUE, sep = ",")

# read in the transition date file
td_sh <- read.table("data/NEON.D15.ONAQ.DP1.00042_SH_1001_3day_transition_dates.csv",
                    header = TRUE,
                    sep = ",")
#read in the cheatgrass timeseries
df_gr <- read.table("data/NEON.D15.ONAQ.DP1.00042_GR_1000_3day.csv", header = TRUE, sep = ",")

# read in the transition date file
td_gr <- read.table("data/NEON.D15.ONAQ.DP1.00042_GR_1000_3day_transition_dates.csv",
                    header = TRUE,
                    sep = ",")

spring_sh <- td_sh[td_sh$direction == "rising" & td_sh$gcc_value == "gcc_90",]
fall_sh <- td_sh[td_sh$direction == "falling" & td_sh$gcc_value == "gcc_90",]
spring_gr <- td_gr[td_gr$direction == "rising" & td_gr$gcc_value == "gcc_90",]
fall_gr <- td_gr[td_gr$direction == "falling" & td_gr$gcc_value == "gcc_75",]

lvs_p<- leaves %>%
  dplyr::select(Start_Date, Proportion_Yes_Records) %>%
  mutate(date = as.Date(Start_Date),
         var = "NEON TOS") %>%
  filter(Proportion_Yes_Records >0) 

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

d<- rbind(dplyr::select(df_sh,date, smooth_gcc_90)%>% mutate(var="PhenoCam: Shrubs"),
          dplyr::select(df_gr,date, smooth_gcc_90)%>% mutate(var="PhenoCam: Cheatgrass"))
rmax<-max(d$smooth_gcc_90) %>% round(2)
rmin<-min(d$smooth_gcc_90) %>% round(2)
rmed<- round(rmax - ((rmax-rmin)/2), 3)


d %>% 
  mutate(date = as.Date(date),
         smooth_gcc_90 = range01(smooth_gcc_90)) %>%
  filter(date < max(lvs_p$date)+60) %>%
  ggplot(aes(x=date, y=smooth_gcc_90,
             color=var)) + 
  geom_bar(data = lvs_p, aes(x=date,y=Proportion_Yes_Records),
           stat="identity", fill = "#FFC845") +
  geom_line(key_glyph = "rect") +
  geom_point(data = fall_gr %>%
               mutate(var = "Cheatgrass 25% Greenness Threshold\n(senescence)"), 
             aes(x=as.Date(transition_25), y=threshold_25), key_glyph="rect")+
  scale_y_continuous(name = "PhenoCam Relative Greenness", breaks = c(0,.5,1),labels = c(rmin,rmed, rmax), 
                     sec.axis = dup_axis(name = "Proportion Yes Records",breaks = c(0,1),labels = c(0,1)))+ 
  scale_x_date(name = "Date",
               date_breaks = "6 months",
               date_labels = "%B %Y") +
  theme_classic() +
  theme(legend.title = element_blank(),
        panel.border = element_rect(fill=NA, size=.75),
        legend.position =c(0,1),
        legend.justification = c(0,1),
        legend.background = element_rect(fill="transparent"),
        legend.direction = "vertical")+
  scale_color_manual(values = c("black", NA,"#007DBA", "firebrick")) +
  ggsave("draft_figures/gcc_leaf_obs7.png", height=4, width = 7.5) +
  ggsave("final_figures/figure_5_gcc_leaf.pdf", height =4, width=9.5)

