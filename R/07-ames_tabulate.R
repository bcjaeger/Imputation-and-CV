
library(tidyverse)

ames_clean <- read_rds('results/06-ames_clean.rds')

ames_bv <- ames_clean %>% 
  summarize(
    mn_bias_icv_rf = mean(ext_rf - icv_rf), 
    mn_bias_cvi_rf = mean(ext_rf - cvi_rf),
    mn_bias_icv_lm = mean(ext_lm - icv_lm), 
    mn_bias_cvi_lm = mean(ext_lm - cvi_lm),
    sd_bias_icv_rf = sd(ext_rf - icv_rf), 
    sd_bias_cvi_rf = sd(ext_rf - cvi_rf),
    sd_bias_icv_lm = sd(ext_lm - icv_lm), 
    sd_bias_cvi_lm = sd(ext_lm - cvi_lm)
  ) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, into = c('metric', 'measure', 'type', 'model'))

external <- ames_clean %>% 
  pivot_longer(cols = matches('^ext|^icv|^cvi')) %>% 
  separate(name, into = c('type', 'model')) %>% 
  filter(type == 'ext') %>% 
  select(seed, impute, model, external = value)

external_smry <- external %>% 
  group_by(model) %>% 
  summarize(mn = mean(external), sd = sd(external))

cv_methods <- ames_clean %>% 
  pivot_longer(cols = matches('^ext|^icv|^cvi')) %>% 
  separate(name, into = c('type', 'model')) %>% 
  filter(type != 'ext') %>% 
  left_join(external)

tuned_rows <- cv_methods %>% 
  group_by(seed, type, model) %>%
  arrange(seed, type, model) %>% 
  summarise(external = external[which.max(value)]) %>%
  pivot_wider(names_from = type, values_from = external) %>% 
  group_by(model) %>% 
  summarise(diff = mean(cvi-icv))

tuned_rows %>% 
  pivot_wider(names_from = type, values_from = external) %>% 
  mutate(diff = cvi - icv) %>% 
  pull(diff) %>% 
  format(nsmall = 5)

ames_clean %>% 
  pivot_longer(cols = matches('^ext|^icv|^cvi')) %>% 
  separate(name, into = c('type', 'model')) %>% 
  ggplot() + 
  aes(x = impute, y = value, col = type) + 
  geom_smooth(se = FALSE) + 
  facet_wrap(~model)

