source("R/diversity_data_prep.R")
source("R/soil_chem.R")
library(lme4)
# basic relationships with diversity 
# get traits with BIEN?
# temporal stuff --  monitoring example
# e.g first detections --
# each site faceted, with lines for each plot, red dots for first detections
# get nspp for family


plot_level%>%
  lmerTest::lmer(shannon_native~shannon_exotic + (1|site), data=.) %>%
  summary

plot_level %>%
  lme4::glmer(nspp_native ~ nspp_exotic + (1|site), 
              data = ., family = "poisson") %>% 
  summary

# interaction with scale
all_scales %>%
  lme4::glmer(nspp_native ~ nspp_exotic +scale + (1|site), 
              data = ., family = "poisson") %>% 
  summary

all_scales %>%
  lmerTest::lmer(shannon_native~shannon_exotic+scale + (1|site), data=.) %>%
  summary

# trying to do what someone wrote in the google doc -----

plot_level%>%
  mutate(year = as.numeric(year)-2013) %>% # making the year easier for lmer to deal with
  lmerTest::lmer(shannon_native~rel_cover_exotic*site + (1|plotID), data=.) %>%
  summary

plot_level%>%
  mutate(year = as.numeric(year)-2013) %>% # making the year easier for lmer to deal with
  lmerTest::lmer(shannon_total~rel_cover_exotic*site*year + (1|plotID), data=.) %>%
  summary


# idea =========================================================================
# look at r2 values for individual species cover vs shannon diversity/evenness etc
# as a way to detect species having an impact
cover_ <- get_longform_cover(x) %>%
  filter(family != "") %>%
  mutate(genus = str_split(scientificName,
                           pattern = " ",
                           simplify = TRUE)[,1],
         species = str_split(scientificName,
                             pattern = " ",
                             simplify = TRUE)[,2],
         gen_sp = str_c(genus, " ", species))

result_list <- list()
spps <- unique(cover_$gen_sp) 

library(MuMIn)
library(doParallel)
library(foreach)
registerDoParallel(detectCores()-1)
t0<-Sys.time()
res <- foreach(ss = 1:length(spps), .combine = rbind)%dopar%{
  df <- get_diversity_info(neon_div_object = x,
                           scale = "plot",
                           species = spps[ss])
  # removing sites with zeros, probably a bad idea maybe not
  df <- df[df[,24]>0,]
  
  if(nrow(df)<20){
    return(data.frame("species" = spps[ss], "marginal" = NA, "conditional" = NA))
  }
  
  c_name <- spps[ss] %>% 
    str_replace(" ", "_") %>%
    str_c("cover_", .)
  
  f <- formula(paste0("shannon_total ~ `", 
                      c_name, 
                      "` + (1|site)"))
  mod <- lmerTest::lmer(f, data = df)

  r2 <-invisible(MuMIn::r.squaredGLMM(mod))
  
  # result_list[[ss]] <- data.frame("marginal" = r2[1], "conditional" = r2[2])
  msg <- paste("echo",round(ss/length(spps)*100,2), "%")
  system(msg)
  return(data.frame("species" = spps[ss], "marginal" = r2[1], "conditional" = r2[2]))
}
print(Sys.time()-t0) # 7 mins with 8 cores

res %>% arrange(desc(marginal))

# keystone woody species driving down diversity?
get_diversity_info(x, scale = "plot", species = "Artemisia tridentata") %>%
  filter(cover_Artemisia_tridentata >0) %>%
  ggplot(aes(x=cover_Artemisia_tridentata, y= shannon_native)) +
  geom_point() +
  geom_smooth(method = "loess", se=F)

# keystone woody species driving down diversity?
get_diversity_info(x, scale = "plot", species = "Prosopis glandulosa") %>%
  filter(cover_Prosopis_glandulosa >0) %>%
  ggplot(aes(x=cover_Prosopis_glandulosa, y= shannon_native)) +
  geom_point() +
  geom_smooth(method = "loess", se=F)

get_diversity_info(x, scale = "plot", species = "Vulpia octoflora") %>%
   filter(cover_Vulpia_octoflora >0) %>%
  ggplot(aes(x=rc_Vulpia_octoflora, y= shannon_exotic)) +
  geom_point() +
  geom_smooth(method = "loess", se=F)

# well, that didn't work out like i hoped...

# idea 2 ================
# detecting first occurrence of species
<<<<<<< HEAD
# does diversity affect that first invasion? 
=======
>>>>>>> f152fc0b2bac989ba3f0433668bf170dffac2f6e

first_year_bysp <- get_longform_cover(x) %>%
  filter(family != "") %>%
  filter(nativeStatusCode == "I") %>% 
  mutate(year = as.numeric(year)) %>%
  group_by(taxonID, site, plotID, scientificName, family) %>%
  summarise(first_year = min(year),
            number_of_years_detected = n()) %>%
  ungroup()

first_year_invaded <- get_longform_cover(x) %>%
  filter(family != "") %>%
  filter(nativeStatusCode == "I") %>% 
  mutate(year = as.numeric(year)) %>%
  group_by(site, plotID) %>%
  summarise(first_year = min(year),
<<<<<<< HEAD
            number_of_years_invaded = length(unique(year)),
            number_of_invasive_spp = length(unique(taxonID))) %>%
  ungroup() %>%
  left_join(plot_level %>% mutate(year = as.numeric(year)),
            by = c("plotID", "site"))



# boxplot(first_year_invaded$nspp_native, plot_level$nspp_native)
# boxplot(first_year_invaded$shannon_native, plot_level$shannon_native)

# getting year before diversity
prev_year_div <- all_scales %>% 
  mutate(year = as.numeric(year)) %>%
  filter(nspp_exotic == 0) %>%
  dplyr::select(year, plotID, scale, subplotID, site,
                prev_shannon_total=shannon_total, 
                prev_nspp_total=nspp_total, 
                prev_shannon_native=shannon_native, 
                prev_nspp_native = nspp_native) %>%
  mutate(year = year+1)

x<-  all_scales%>% 
              mutate(year = as.numeric(year)) %>%
  inner_join(prev_year_div,
            by = c("plotID", "subplotID", "year", "scale", "site"))

x%>%
  mutate(invaded = ifelse(invaded=="invaded",1,0), 
         prev_shannon_native = scale(prev_shannon_native)) %>%
  lme4::glmer(invaded ~ prev_shannon_native*scale +(1|site), 
              data = ., family = "binomial")%>%
  summary

x%>%
  mutate(invaded = ifelse(invaded=="invaded",1,0), 
         prev_nspp_native = scale(prev_nspp_native)) %>%
  lme4::glmer(invaded ~ prev_nspp_native*scale +(1|site), 
              data = ., family = "binomial")%>%
  summary

p1<-ggplot(prev_year_div %>% filter(invaded != 0) %>%
         mutate(invaded = ifelse(invaded=="invaded", 1,0)), 
       aes(x = prev_nspp_native, y=invaded, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()


plot_level %>%
  mutate(invaded = as.factor(invaded)) %>%
  lme4::glmer(invaded ~ shannon_total  +(1|site), 
              data = ., family = "binomial")%>%
  summary

# models having convergence troubles
all_scales %>%
  mutate(invaded = as.factor(invaded)) %>%
  lme4::glmer(invaded ~ shannon_total * scale +(1|site), 
              data = ., family = "binomial")%>%
  summary

all_scales %>%
  mutate(invaded = as.factor(invaded),
         nspp_native = scale(nspp_native)) %>%
  lme4::glmer(invaded ~ nspp_native *scale+ (1|site), 
              data = ., family = "binomial")%>%
  summary

# money plot
p2<-ggplot(all_scales %>% filter(invaded != 0) %>%
         mutate(invaded = ifelse(invaded=="invaded", 1,0)), 
       aes(x = nspp_native, y=invaded, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_classic()

library(ggpubr)
ggarrange(p1,p2)

# invaded locations vs uninvaded locations - temporal patterns
# invaded vs uninvaded hypothesis test - slope values from rate of change

ggplot(all_scales %>% filter(invaded != 0) %>%
         mutate(invaded = ifelse(invaded=="invaded", 1,0)), 
       aes(x = shannon_total, y=invaded, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

ggplot(all_scales %>% filter(invaded != 0) %>%
         mutate(invaded = ifelse(invaded=="invaded", 1,0)), 
       aes(x = nspp_total, y=invaded, color = scale)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))

=======
            number_of_years_invaded = n()) %>%
  ungroup()

ggplot(first_year_invaded, aes(x = first_year)) +
  geom_density() +
  facet_wrap(~site)
>>>>>>> f152fc0b2bac989ba3f0433668bf170dffac2f6e

ggplot(first_year_bysp, aes(x = first_year, color = scientificName)) +
  geom_density() +
  facet_wrap(~site) +
  theme(legend.position = "none")
<<<<<<< HEAD

# modelling soil chem vs div ===================================================
soil_cn %>%
  left_join(first_year_invaded) %>%
  lme4::glmer(number_of_years_invaded ~ 
                organicCPercent +
                shannon_native + 
                shannon_exotic +
                (1|site), 
              data=., 
              family="poisson") %>%
  summary

soil_cn %>%
  left_join(first_year_invaded) %>%
  lmerTest::lmer(shannon_native ~
                number_of_years_invaded +
                organicCPercent +
                shannon_exotic +
                (1|site), 
              data=.) %>%
  summary

soil_cn %>%
  left_join(first_year_invaded) %>%
  lme4::glmer(nspp_native ~
                   number_of_years_invaded +
                   organicCPercent +
                   nspp_exotic +
                   (1|site), 
                 data=., family="poisson") %>%
  summary

soil_cn %>%
  left_join(first_year_invaded) %>%
  lme4::glmer(number_of_invasive_spp ~ shannon_native + shannon_exotic +
                soil_cn +
                (1|site), 
              data=., family="poisson") %>%
  # car::vif()
  summary

soil_cn %>%
  left_join(first_year_invaded) %>%
  lme4::glmer(number_of_invasive_spp ~ nspp_native + nspp_exotic+
                soil_cn+
                (1|site), 
              data=., family="poisson") %>%
  # car::vif()
  summary
=======
>>>>>>> f152fc0b2bac989ba3f0433668bf170dffac2f6e
