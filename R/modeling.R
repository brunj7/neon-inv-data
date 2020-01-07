
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
  lmerTest::lmer(shannon_total~rel_cover_exotic*site + (1|plotID), data=.) %>%
  summary
