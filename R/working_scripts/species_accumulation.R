source(diversity_data_prep.R)

# idea - full sacs per site, then per plot through time (each plot, all years)

for(i in vegan_friendly_div$bout_year){
  plot(specaccum(comm = vegan_friendly_div %>%
                   filter(nativeStatusCode == "N", bout_year == i) %>%
                   dplyr::select(-bout_year, -endDate, -nativeStatusCode,-plotID,
                                 -shannon, -nspp)
  ))
}

ggplot(vegan_friendly_div,
       aes(x = endDate, y=shannon, color = nativeStatusCode)) +
  geom_line() +
  #theme(legend.position = "none") +
  facet_wrap(~plotID) +
  theme_pubr() +
  ggsave("/home/a/Desktop/quickplot.png")

ggplot(n_i,
       aes(x = endDate, y=rel_cover, color = nativeStatusCode)) +
  geom_line() +
  #theme(legend.position = "none") +
  facet_wrap(~plotID) +
  theme_pubr() +
  ggsave("/home/a/Desktop/quickplot.png")


sact <- list()
plot_ids <- vegan_friendly_div$plotID
sites <- str_sub(vegan_friendly_div$plotID,1,4) %>% unique
vegan_friendly_div$site <- sites
for(i in 1:length(unique(sites))){
  vegan_friendly_div %>% 
    filter(site == sites[i]) %>% 
    dplyr::select(-plotID,-bout_year, -endDate, -nativeStatusCode,-shannon,-nspp,-site) %>%
    specaccum(method = "exact") -> zero
  sact[[i]] <- data.frame(sites = zero$sites, 
                          richness = zero$richness, 
                          sd = zero$sd,
                          # plot = plot_ids[i],
                          site = sites[i]#,
                          # nativity = vegan_friendly_div$nativeStatusCode[i]
  )
}
sact <- do.call("rbind", sact)

pd <- position_dodge(0.1)

ggplot(sact, aes(x=sites, y=richness#, 
                 #color = nativity, group = plot
)) + 
  geom_errorbar(aes(ymin=richness-sd, ymax=richness+sd), width =0, position = pd) +
  #geom_ribbon(aes(ymin=richness-sd, ymax=richness+sd, fill = fire_frequency),alpha = 0.2) +
  geom_line(#aes(color = nativity), 
    position=pd) +
  scale_color_discrete(name="native status") +
  xlab("Sites") +
  ylab("Richness") +
  theme_pubr()+
  facet_wrap(~site)+
  theme(legend.position = c(1,0),
        legend.justification = c(1,0))

