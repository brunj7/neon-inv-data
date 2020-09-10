# installing packages - if necessary
# devtools::install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
# install.packages(c("rhdf5", "doBy", "sf")) 

# load packages
library(tidyverse)
library(neonUtilities)
library(geoNEON)
library(raster)
library(rhdf5)
library(httr)
library(jsonlite)
library(downloader)
library(scales)
library(gridExtra)
library(doBy)
library(sf)
library(sp)
library(lme4)
library(bbmle)
library(viridis)
library(devtools)


###
#set crs for all data layers: Albers Equal Area
#crs1 <- 'ESRI:102003'
crs1b <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
###

#sites we want are ONAQ, MOAB, SRER, and JORN

# ###
# #bring in example of cover data
# coverO <- loadByProduct (dpID = "DP1.10058.001", site = 'ONAQ', check.size= FALSE)
# coverM <- loadByProduct (dpID = "DP1.10058.001", site = 'MOAB', check.size= FALSE)
# coverS <- loadByProduct (dpID = "DP1.10058.001", site = 'SRER', check.size= FALSE)
# coverJ <- loadByProduct (dpID = "DP1.10058.001", site = 'JORN', check.size= FALSE)
# 
# #select the 1 m2 subplots (this is where cover is measured)
# coverDivO <- coverO[[2]]
# coverDivM <- coverM[[2]]
# coverDivS <- coverS[[2]]
# coverDivJ <- coverJ[[2]]
# 
# unique(coverDivO$divDataType)
# 
# cover2O <- coverDivO %>%
#   dplyr::filter(divDataType=="plantSpecies")
# 
# cover2M <- coverDivM %>%
#   dplyr::filter(divDataType=="plantSpecies")
# 
# cover2S <- coverDivS %>%
#   dplyr::filter(divDataType=="plantSpecies")
# 
# cover2J <- coverDivJ %>%
#   dplyr::filter(divDataType=="plantSpecies")
# 
# covera <- rbind (cover2O, cover2M)
# coverb <- rbind (covera, cover2S)
# cover2 <- rbind (coverb, cover2J)
# 
# unique(cover2$nativeStatusCode)
# 
# sumCoverDiv <- cover2 %>%
#   group_by(plotID, nativeStatusCode) %>%
#   summarise(mean = mean(percentCover))

source("R/diversity_data_prep.R")
# 
# inv<- plot_level %>%
#   dplyr::select(plotID, cover_exotic, year)

cover2<- x$div_1m2Data %>%
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
  ungroup()

# inv <- cover2 %>%
#   filter(nativeStatusCode=="I")

##transform to sf object
cover_sf  <-  st_as_sf(cover2, 
                       coords = c('decimalLongitude', 'decimalLatitude'), 
                       crs = 4326) %>%
  st_transform(crs1b)

# st_crs(cover_sf)
###


#filter for 2016 cover only
#unique(cover_sf$endDate)

cover_sf2016 <- cover_sf %>%
  filter(year == "2016")




#split by site, but will be joined back later
cover_sf2016onaq <- cover_sf2016 %>%
  filter(siteID == "ONAQ")

cover_sf2016moab <- cover_sf2016 %>%
  filter(siteID == "MOAB")

cover_sf2016srer <- cover_sf2016 %>%
  filter(siteID == "SRER")

cover_sf2016jorn <- cover_sf2016 %>%
  filter(siteID == "JORN")



###
#bring in roads data
#setwd("D:/Arquivos_pessoais/?rea de Trabalho/NEON")

unzip(zipfile = "data/NEON_roads_data-20200130T165308Z-001.zip",
      exdir = "data")

road_path <- "data/NEON_roads_data/"

onaq_roads_shp <- st_read(file.path(road_path), layer = '15_C_ONAQ_Roads') %>%
  st_transform(.,crs1b) %>%
  dplyr::select()
# each file has different column names... we don't care about any of them
# st_crs(onaq_roads_shp)

moab_roads_shp <- st_read(file.path(road_path), layer = '13_R_MOAB_Roads') %>%
  st_transform(.,crs1b) %>%
  dplyr::select()

srer_roads_shp <- st_read(file.path(road_path), layer = '14_C_SRER_Roads') %>%
  st_transform(.,crs1b) %>%
  dplyr::select()

jorn_roads_shp <- st_read(file.path(road_path), layer = '14_R_JORN_Roads') %>%
  st_transform(.,crs1b) %>%
  dplyr::select()



###################
#join roads data...but they all have different fields...how to do this?
#roads2 <- st_union(onaq_roads_shp, moab_roads_shp, by_feature = FALSE)
#roads2a <- st_join(onaq_roads_shp, moab_roads_shp, by = X1)
###################

#cper_roads_shp2 <- st_read(dsn = 'NEON_roads_data', layer = "10_C_CPER_Roads") %>%
#st_transform(., crs1b)

#now I need to intersect the point data (cover plots) with the vector data (roads)
#really this is finding the nearest feature/point to the cover plots




# ADAM CHANGING THINGS =========================================================
#method 1
cover_sf2016onaq$dist_to_road <- sf::st_nearest_feature(cover_sf2016onaq, 
                                                        onaq_roads_shp)
length(roads_intonaq)
#1484; same as original dataset

# adam here!
# looks like this isn't doing what we think it's doing. I think it's just 
# returning the row number of the nearest feature
plot(onaq_roads_shp[0])
plot(cover_sf2016onaq[6], add=T)

# new method - st_distance returns a matrix of all pairwise distances so we 
# need to do the apply thing at the end to get the minimum of those

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

plot(onaq_roads_shp[0])
plot(cover_sf2016onaq["dist_to_road"], add=T)
plot(srer_roads_shp[0])
plot(cover_sf2016srer["dist_to_road"], add=T)
plot(moab_roads_shp[0])
plot(cover_sf2016moab["dist_to_road"], add=T)
plot(jorn_roads_shp[0])
plot(cover_sf2016jorn["dist_to_road"], add=T)








dist_allsites <- bind_rows(cover_sf2016onaq, cover_sf2016moab, 
                           cover_sf2016srer, cover_sf2016jorn) %>%
  st_set_geometry(NULL) %>%
  dplyr::select(plotID, dist_to_road) #combine all sites data
# dist_allsites <- as.data.frame(dist_allsites) #convertes the data table into a dataframe (more managable)
# dist_allsites <- select(dist_allsites, plotID, dist_to_road) #select only the desired columns
# dist_allsites <- distinct(dist_allsites, plotID, .keep_all = TRUE) #select unique plot names (no suplots)

# importing the data from a table I found in our shared GitHub (it's from Adam)
#plot_level_diversity_stuff.txt

# cover_allsites <- read.table(file.path('D:/Arquivos_pessoais/?rea de Trabalho/NEON/plot_level_diversity_stuff.txt'), sep = ",", header = TRUE) 

coverdist_allsites <- left_join(plot_level, dist_allsites, by = "plotID") %>%
  filter(year == 2016) %>%
  mutate(site = factor(site, levels = c("ONAQ", "MOAB", "SRER", "JORN")))

###Exploratory data

#histograms response variables
par(mfrow = c(1,3))
hist(coverdist_allsites$nspp_exotic, col = "orange", breaks = 10)
#hist(coverdist_allsites$rel_cover_exotic, col = "purple", breaks = 10)
#hist(coverdist_allsites$rc_exotic_Poaceae, col = "darkgreen", breaks = 10)

####center and standardize the predictor variables
coverdist_allsites$dist_to_road.Z <- (coverdist_allsites$dist_to_road - mean(coverdist_allsites$dist_to_road))/ (2 * sd(coverdist_allsites$dist_to_road)) 
coverdist_allsites$nspp_native.Z <- (coverdist_allsites$nspp_native - mean(coverdist_allsites$nspp_native))/ (2 * sd(coverdist_allsites$nspp_native)) 

#summary statistics for distance to the road by site ### really cool package, function
library(psych)
describe.by(coverdist_allsites$dist_to_road, coverdist_allsites$site)

#### creating the models ##Poisson GLM
mdist <- coverdist_allsites %>%
  lme4::glmer.nb(nspp_exotic ~ dist_to_road.Z + (1|site), 
                 data = ., family = "poisson") 
summary(mdist)

mdist_nat <- coverdist_allsites %>%
  lme4::glmer(nspp_exotic ~ nspp_native.Z + dist_to_road.Z + (1|site), 
              data = ., family = "poisson") 
summary(mdist_nat)

mdistnat <- coverdist_allsites %>%
  lme4::glmer(nspp_exotic ~ nspp_native.Z*dist_to_road.Z + (1|site), 
              data = ., family = "poisson") 
summary(mdistnat)

mnat <- coverdist_allsites %>%
  lme4::glmer(nspp_exotic ~ nspp_native.Z + (1|site), 
              data = ., family = "poisson")
summary(mnat)

#Comparing models
# if including a model with native species only, without minimum distance, it ends up being the best model
AICtab(logLik(mdist), logLik(mdist_nat), logLik(mdistnat), logLik(mnat), base=TRUE, weights = TRUE)

#coefficients
coef(summary(mdist))
coef(mdist)

#creating the plot
my.alpha <- 100 # defines how transparent you want the color to be

# the function to make the colors transparent
ggplot(coverdist_allsites, aes(x=dist_to_road, y=nspp_exotic, color = site)) +
  geom_point(size = 2, alpha=0.7) +
  geom_rug(sides = "b", 
           alpha = 0.7, 
           position = "jitter", length = unit(0.02, "npc")) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se=TRUE, aes(color = site))+
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_classic() +
  labs(x ="Distance to nearest road (m)", y = "Richness of invasive species (2016)",
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
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill=NA)
  ) + 
  ggsave("draft_figures/dist_to_road_nspp_exotic.png", width = 7, height =4)

##without sitesas random effects##

summary(m1 <- glm(nspp_exotic ~ dist_to_road.Z, family="poisson", data=coverdist_allsites))
coef(m1)


p1<- ggplot(coverdist_allsites, aes(x=dist_to_road, y=nspp_exotic)) +
  geom_point(size = 2, alpha=0.7) +
  geom_rug(sides = "b", 
           alpha = 0.7, 
           position = "jitter", length = unit(0.02, "npc")) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se=TRUE)+
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_classic() +
  labs(x ="Distance to nearest road (m)", y = "Richness of invasive species (2016)",
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
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill=NA)
  )
p2<- ggplot(coverdist_allsites, aes(x=dist_to_road, y=nspp_native)) +
  geom_point(size = 2, alpha=0.7) +
  geom_rug(sides = "b", 
           alpha = 0.7, 
           position = "jitter", length = unit(0.02, "npc")) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se=TRUE)+
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_classic() +
  labs(x ="Distance to nearest road (m)", y = "Richness of invasive species (2016)",
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
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill=NA)
  )

p3<-ggplot(coverdist_allsites, aes(x=dist_to_road, y=cover_exotic)) +
  geom_point(size = 2, alpha=0.7) +
  geom_rug(sides = "b", 
           alpha = 0.7, 
           position = "jitter", length = unit(0.02, "npc")) +
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se=TRUE)+
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_classic() +
  labs(x ="Distance to nearest road (m)", y = "Cover of invasive species (2016)",
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
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill=NA)
  )

p4<-ggplot(coverdist_allsites, aes(x=dist_to_road, y=cover_native)) +
  geom_point(size = 2, alpha=0.7) +
  geom_rug(sides = "b", 
           alpha = 0.7, 
           position = "jitter", length = unit(0.02, "npc")) +
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se=TRUE)+
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme_classic() +
  labs(x ="Distance to nearest road (m)", y = "Cover of native species (2016)",
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
    legend.title = element_text(face = "bold"),
    panel.border = element_rect(colour = "black", fill=NA)
  )

ggsave(plot = p1,filename="draft_figures/dist_to_roads_v_exotics.png", width=7, height=4)

ggpubr::ggarrange(p1,p2,p3,p4, nrow=2, ncol=2, labels="auto",label.x=0.94, label.y=.98) +
ggsave(filename="draft_figures/dist_to_roads_v_e_n.png", width=10, height=8)


##END##

