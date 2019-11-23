source("R/diversity_data_prep.R")
library(lmerTest)

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
  theme_pubr() +
# facet_wrap(~site, scales = "free") 
  ggsave("draft_figures/n_vs_e_nspp.png")

vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, nspp) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = nspp) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  lme4::glmer(I ~ N + (1|site), data = ., family = "poisson") %>% 
  summary

vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  ggplot(aes(x=N, y=I, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", show.legend = F) +
  ggtitle("Native vs. Exotic Shannon Diversity")+
  xlab("Native Diversity") +
  ylab("Exotic Diversity") +
  theme_pubr() +
  ggsave("draft_figures/n_vs_e_shannon.png")

# note: perhaps if we adjust for the total number of exotic species (i.e. the
# level of invasion), we wouldn't have to group the


vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  lmerTest::lmer(I~N + (1|site), data=.) %>%
  summary

# rescaling needed
vegan_friendly_div %>%
  dplyr::select(plotID, bout_year, endDate, nativeStatusCode, shannon) %>%
  pivot_wider(names_from = nativeStatusCode,
              values_from = shannon) %>%
  mutate(site = str_sub(plotID, 1,4)) %>%
  lmerTest::lmer(I~N*site*endDate + (1|plotID), data=.) %>%
  summary
