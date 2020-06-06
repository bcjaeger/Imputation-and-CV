
library(tidyverse)
library(tblStrings)

ames_init <- read_rds('results/06-ames_clean.rds')

ames_nsims <- nrow(ames_init)

ames_times <- (sum(ames_init$time_icv) + sum(ames_init$time_cvi)) / (60^2)

ames_time_icv <- mean(ames_init$time_icv) / 60
ames_time_cvi <- mean(ames_init$time_cvi) / 60

# ames_times <- ames_init %>% 
#   select(seed, starts_with('time')) %>% 
#   summarize(across(starts_with('time'), quantile, probs = probs))

ames <- list(
  times = ames_times, 
  times_cvi = ames_time_cvi,
  times_icv = ames_time_icv,
  times_ratio = ames_time_cvi / ames_time_icv,
  nsims = ames_nsims
)


ames_clean <- ames_init %>% 
  unnest(results)

external <- ames_clean %>% 
  pivot_longer(cols = matches('^ext|^icv|^cvi')) %>% 
  separate(name, into = c('type', 'model')) %>% 
  filter(type == 'ext') %>% 
  select(seed, impute, model, external = value)

ames_bias <- ames_clean %>% 
  drop_na() %>% 
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

ames_variance <- ames_clean %>% 
  drop_na() %>% 
  summarize(
    sd_icv_rf = sd(icv_rf), 
    sd_cvi_rf = sd(cvi_rf),
    sd_icv_lm = sd(icv_lm), 
    sd_cvi_lm = sd(cvi_lm)
  ) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, into = c('measure', 'type', 'model'))

bias_smry <- ames_bias %>% 
  pivot_wider(values_from = value, names_from = metric) %>% 
  transmute(measure, model, type, 
    tbv = tbl_string("{100*mn} ({100*sd})")) %>% 
  pivot_wider(values_from = tbv, names_from = type)

variance_smry <- ames_variance %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(across(c(icv, cvi), ~tbl_val(100*.x, max_decimals = 3)))

estimate_smry <- ames_clean %>%
  drop_na() %>% 
  pivot_longer(cols = matches('^ext|^icv|^cvi')) %>% 
  separate(name, into = c('type', 'model')) %>% 
  select(seed, type, impute, model, value) %>% 
  group_by(model, type) %>% 
  summarize(mn = mean(value), sd = sd(value)) %>% 
  ungroup() %>% 
  mutate(measure = 'estimate', 
    tbv = tbl_string("{100*mn} ({100*sd})")) %>%
  select(-mn, -sd) %>% 
  pivot_wider(values_from = tbv, names_from = type)

ames$tbl_data <- bind_rows(
  estimate_smry, 
  bias_smry, 
  variance_smry
) %>% 
  relocate(ext, .after = measure) %>% 
  arrange(model) %>% 
  mutate(
    model = recode(model, 
      lm = 'Linear model',
      rf = 'Random forest'),
    measure = recode(measure,
      estimate = '$R^2$',
      bias = 'Bias', 
      sd = 'Standard deviation')
  )

ames$meanimpute_smry <- ames_clean %>% 
  filter(impute == 0) %>% 
  summarize(across(c(ext_rf, ext_lm), list(mn = mean, sd = sd))) %>% 
  transmute(
    ext_lm = tbl_string("{ext_lm_mn} ({ext_lm_sd})"),
    ext_rf = tbl_string("{ext_rf_mn} ({ext_rf_sd})")
  )


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
  group_by(model)


ames$tuned_smry <- tuned_rows %>% 
  summarize(across(c(cvi, icv), 
    ~tbl_string("{mean(.x)} ({sd(.x)})", max_decimals = 3))) %>% 
  pivot_wider(names_from = model, values_from = c(cvi, icv))

ames$tuned_diffs <- tuned_rows %>% 
  summarise(diff = mean(cvi-icv)) %>% 
  deframe() %>% 
  as.list()

write_rds(ames, 'results/07-ames_tabulate.rds')

# ames_clean %>% 
#   pivot_longer(cols = matches('^ext|^icv|^cvi')) %>% 
#   separate(name, into = c('type', 'model')) %>% 
#   ggplot() + 
#   aes(x = impute, y = value, col = type) + 
#   geom_smooth(se = FALSE) + 
#   facet_wrap(~model)

