source(diversity_data_prep.R)



vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, nspp) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = nspp) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x=N, y=I, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Species Richness")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()# +
# facet_wrap(~site, scales = "free") 

glm(I ~ N*site, data = n_v_i_div, family = "poisson") %>% summary

vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x=N, y=I, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Shannon Diversity")+
  xlab("Native Species") +
  ylab("Exotic Species") +
  theme_pubr()