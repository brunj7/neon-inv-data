

plot_level%>%
  lmerTest::lmer(shannon_native~shannon_exotic + (1|site), data=.) %>%
  summary

plot_level %>%
  lme4::glmer(nspp_native ~ nspp_exotic + (1|site), data = ., family = "poisson") %>% 
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
            number_of_years_invaded = n()) %>%
  ungroup()

ggplot(first_year_invaded, aes(x = first_year)) +
  geom_density() +
  facet_wrap(~site)

ggplot(first_year_bysp, aes(x = first_year, color = scientificName)) +
  geom_density() +
  facet_wrap(~site) +
  theme(legend.position = "none")
