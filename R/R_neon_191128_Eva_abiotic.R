# Eva Stricker code for Neon Invasion Vulnerability working group 191128

library(neonUtilities)
library(tidyverse)
library(ggpubr)
library(vegan)
detach(package:plyr)
library(httr)
library(RCurl)
library(dplyr)
library(viridis) 

x <- loadByProduct(dpID = "DP1.10058.001", site = c("SRER", "ONAQ", "MOAB", "JORN"), 
                   check.size = F)

#### code to get proportion of total cover that is invasive grass
cover_subplots <- x$div_1m2Data %>%
  mutate(endDate = as.Date(endDate)) %>%
  dplyr::filter(divDataType == "plantSpecies") %>%
  mutate(bout_year = str_c(str_sub(endDate,1,4),"_", boutNumber))%>%
  group_by(plotID, subplotID, taxonID, family, bout_year) %>%
  summarise(cover = sum(percentCover, na.rm=TRUE)/8,
            nativeStatusCode = first(nativeStatusCode),
            endDate = first(endDate)) %>%
  ungroup()  %>%
  mutate(taxonID = as.character(taxonID),
         plotID = as.character(plotID),
         subplotID = as.character(subplotID),
         nativeStatusCode = as.character(nativeStatusCode))

#NI = "native or invasive" category
cover_subplots$NI <- paste(cover_subplots$family, cover_subplots$nativeStatusCode, sep = "_")

# categorize everything NOT invasive grass as native
cover_subplots[which(cover_subplots$NI != "Poaceae_I"), "NI"]<-"N"
cover_subplots$NI <- as.factor(cover_subplots$NI)

# spread values so that I can calculate 
NI <- cover_subplots %>% select(NI, plotID, subplotID, bout_year, endDate, cover, NI)%>%
   group_by(NI, plotID, subplotID, bout_year, endDate)%>%
    summarise(cover_s = sum(cover)) %>% spread(key  = NI, value = cover_s)


NI[which(is.na(NI$Poaceae_I)), "Poaceae_I"]<- 0

NI[which(is.na(NI$N)), "N"]<- 0

NI$proportion <- NI$Poaceae_I/(NI$Poaceae_I+new$N)
hist(NI$proportion)

NI[which(is.na(NI$proportion)),"proportion"] <- 0

unique(NI$plotID)
### proportion invasive grass
head(NI)


NI$siteID <- factor(substr(NI$plotID, 1, 4), levels = c("ONAQ", "MOAB", "SRER", "JORN"))
### This is problematic - for some sites, and some times, the bout 1 is actually in the fall. so we should definitely define seasons based on month!
# make biologically sensible seasons
NI$Year<-format(as.Date(NI$endDate, format = "%Y-%m-%d"), "%Y")
NI$season<-as.character(format(as.Date(NI$endDate, format = "%Y-%m-%d"), "%m"))

xtabs(~season+bout_year+siteID,NI)
unique(NI$season)

NI[which((NI$season == "01"|NI$season == "03"|NI$season == "04"|
            NI$season == "05"|NI$season=="06"|NI$season=="07"|
            NI$season=="08")&(  NI$siteID == "ONAQ"|NI$siteID=="MOAB")),"season"]<-"spring"

NI[which((NI$season == "01"|NI$season == "03"|NI$season == "04"|
                       NI$season == "05"|NI$season=="06")&(  NI$siteID == "JORN"|NI$siteID=="SRER")),"season"]<-"spring"

NI[which(NI$season != "spring"),"season"]<-"fall"

NI$season<-NI[which((NI$season == "01"|NI$season == "03"|NI$season == "04"|
                       NI$season == "05"|NI$season=="06"|NI$season=="07"|
                       NI$season=="08")&(  NI$siteID == "ONAQ"|NI$siteID=="MOAB")),"season"]<-"spring"

colnames(NI)

### Calculate effect size of differences in proportion by year
NI_sp <- NI %>% 
  select(-c(subplotID, endDate, bout_year, N,Poaceae_I))%>%
  ungroup()%>%
  group_by(plotID, siteID, Year, season)%>%
  summarise( proportion = mean(proportion)) %>%
  filter(season == "spring")%>%
  spread(key = Year, proportion)

# calculate differences in subsequent years
NI_sp$d1516 <-(NI_sp$'2016' -  NI_sp$'2015')/(NI_sp$'2016' +  NI_sp$'2015')
NI_sp$d1617 <-NI_sp$'2017' -  NI_sp$'2016'/(NI_sp$'2017' +  NI_sp$'2016')
NI_sp$d1718 <-NI_sp$'2018' -  NI_sp$'2017'/(NI_sp$'2017' +  NI_sp$'2018')
NI_sp$d1819 <-NI_sp$'2019' -  NI_sp$'2018'/(NI_sp$'2018' +  NI_sp$'2019')
colnames(NI_sp)

NI_sm<- NI_sp %>% select (1,2,3,10,11,12,13) %>%
  gather(key = 'years', value = "proportion",4:7)

hist(NI_sm$proportion)

### Read in soils data
soil <- read.csv(text = getURL("https://raw.githubusercontent.com/brunj7/neon-inv-data/master/Data_merged/soil_charact_chem_phys_l.csv"))

#get just A horizon (is this the best idea? had a hard time figuring out what "ABk" and "Ak" were... )
soil_A<- soil %>% filter( horizonName == "A")
head(soil_A)
xtabs(~plotID+collectDate,soil_A)

NI_soila <- left_join(NI, soil_A, by = c('plotID'='plotID'))


NI_soila$Year <- substr(NI_soila$endDate, 1, 4)

xtabs(~Year+siteID,NI_soila)
# 2016 is the best year to do "initial" comparisons

### This is my 2008 version of subsetting hehehe... I *know* there are better ways but this works!
NI_soila_16sand <- NI_soila[which(NI_soila$Year == "2016"&NI_soila$variable=="sandTotal"),] %>% spread(key = "variable", value = "value")


# picked viridis because it's a good pallete for colorblind-friendlyness; since we'll have a lot of things to deal with visually, color will matter
ggplot(NI_soila_16sand,aes(x = sandTotal, y = proportion, group = siteID))+geom_point(aes(color = siteID, alpha = .2))+
  theme_bw() +  facet_grid(.~season)+
  theme(  axis.title.x = element_text(vjust=-0.35),
          axis.title.y = element_text(vjust=0.35) ,
          axis.title = element_text(size = 12),
          legend.position = "bottom",
          legend.title=element_text(size=11),
          axis.text = element_text(size = 10),
          legend.text=element_text(size=9),
          axis.ticks.x=element_blank(),
          legend.key = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_blank()  )+geom_smooth(method = "lm", aes(color = siteID))+scale_color_viridis(discrete = TRUE, option = "D")+
  xlab("Percent sand")+ylab ("Proportion invasive cover in 2016")

# color is SITE
# columns of facets are seasons (aka, bout year)
# rows of facets are years


#### Now try a figure 2 idea

soilpH <- read.csv(text = getURL("https://raw.githubusercontent.com/brunj7/neon-inv-data/master/Data_merged/soil_phys_periodic_w.csv"))

# match up with NI
soilpH$siteID <- factor(substr(soilpH$plotID, 1, 4), levels = c("ONAQ", "MOAB", "SRER", "JORN"))

soilpH$Year<-format(as.Date(soilpH$collectDate, format = "%Y-%m-%d"), "%Y")
soilpH$season<-as.character(format(as.Date(soilpH$collectDate, format = "%Y-%m-%d"), "%m"))

soilpH[which((soilpH$season == "01"|soilpH$season == "03"|soilpH$season == "04"|
            soilpH$season == "05"|soilpH$season=="06"|soilpH$season=="07"|
            soilpH$season=="08")&(  soilpH$siteID == "ONAQ"|soilpH$siteID=="MOAB")),"season"]<-"spring"

soilpH[which((soilpH$season == "01"|soilpH$season == "03"|soilpH$season == "04"|
            soilpH$season == "05"|soilpH$season=="06")&(  soilpH$siteID == "JORN"|soilpH$siteID=="SRER")),"season"]<-"spring"

soilpH[which(soilpH$season != "spring"),"season"]<-"fall"


# start with just spring and soil moisture
soil_sp_sm <- soilpH %>% 
  select(-c(sampleID, collectDate))%>%
  group_by(plotID, siteID, Year, season)%>%
  summarise( pH = mean(soilInWaterpH), SM = mean(soilMoisture )) %>%
    filter(season == "spring")%>%
    select(-c(pH))%>%
     spread(key = Year, SM)
colnames(soil_sp_sm)
# calculate differences in subsequent years
soil_sp_sm$d1516 <-soil_sp_sm$'2016' -  soil_sp_sm$'2015'
soil_sp_sm$d1617 <-soil_sp_sm$'2017' -  soil_sp_sm$'2016'
soil_sp_sm$d1718 <-soil_sp_sm$'2018' -  soil_sp_sm$'2017'
soil_sp_sm$d1819 <-soil_sp_sm$'2019' -  soil_sp_sm$'2018'

Soil_sm<-soil_sp_sm %>% select (1,2,3,10,11,12,13) %>%
  gather(key = 'years', value = "SM",4:7)
hist(Soil_sm$SM)


### Join native/invasive effect size with differences in soil moisture
NI_sm_d<-left_join(NI_sm, Soil_sm, by = c('plotID'='plotID', 'siteID'='siteID', 'season'='season')
)                   

ggplot(NI_sm_d, aes (x=SM, y = proportion))+geom_point(aes(color = siteID))+
  facet_grid(years.x~season)+theme_bw()+ theme(  axis.title.x = element_text(vjust=-0.35),
                                      axis.title.y = element_text(vjust=0.35) ,
                                      axis.title = element_text(size = 12),
                                      legend.position = "bottom",
                                      legend.title=element_text(size=11),
                                      axis.text = element_text(size = 10),
                                      legend.text=element_text(size=9),
                                      axis.ticks.x=element_blank(),
                                      legend.key = element_rect(fill = "white"),
                                      legend.background = element_rect(fill = "white"),
                                      panel.grid.major = element_line(colour = "white"),
                                      panel.grid.minor = element_blank()  )+
  geom_smooth(method = "lm", aes(color = siteID))+scale_color_viridis(discrete = TRUE, option = "D")+
  xlab("Change in SM over 1y")+ylab ("Effect size of proportion invasive cover")

