library(AmesHousing)
library(ipa)
library(tidyverse)
library(tidymodels)
library(janitor)
library(mice)
library(glue)
library(magrittr)

data_all <- make_ames() %>%
  drop_na() %>%
  clean_names(case = 'snake') %>%
  mutate(sale_price = log(sale_price))

patterns <- matrix(1, ncol = ncol(data_all), nrow = 4) %>%
  set_colnames(names(data_all)) %>%
  set_rownames(glue("pattern {1:4}"))

dnames <- names(data_all)

patterns[1, str_detect(dnames, '^lot|^garage')] <- 0
patterns[2, str_detect(dnames, '^longi|^lati')] <- 0
patterns[3, str_detect(dnames, '^bsmt|year')] <- 0
patterns[4, str_detect(dnames, '\\_qual|gr_liv_area')] <- 0

data_amp <- ampute(data_all, prop = 1, patterns = patterns, 
  mech = 'MCAR', bycases = TRUE) %>% 
  use_series('amp')

miss_vars <- data_amp %>% 
  select_if(~any(is.na(.x)))

data_analysis <- select(data_amp, sale_price) %>% 
  bind_cols(miss_vars) %>% 
  as_tibble()

mean(is.na(data_analysis))

data_split <- initial_split(data_analysis, prop = 3/4)

recipe_raw <- recipe(sale_price ~ ., data = training(data_split)) %>% 
  step_other(all_nominal(), threshold = 0.15)

recipe_prepped <- prep(recipe_raw)

data_train <- juice(recipe_prepped)
data_test <- bake(recipe_prepped, new_data = testing(data_split))

training_folds <- vfold_cv(data_train, v = 10)
testing_rows <- map(training_folds$splits, complement)

results <- data_train %>% 
  brew_nbrs(outcome = sale_price) %>%
  verbose_on(level = 1) %>%
  spice(with = spicer_nbrs(k_neighbors = 1:35)) %>%
  mash() %>% 
  stir() %>% 
  ferment(data_new = data_test) %>% 
  bottle(type = 'tibble') %>% 
  use_series('wort') %>% 
  as_tibble() %>% 
  select(-pars)

make_tt <- function(data, test_rows){
  list(
    training = map(test_rows, ~data[-.x, ]),
    testing = map(test_rows, ~data[.x, ])
  )
}

rforest <- function(train, test){
  rand_forest(trees = 500) %>% 
    set_mode('regression') %>% 
    set_engine('ranger') %>% 
    fit(sale_price ~ ., data = train) %>% 
    predict(new_data = test) %>% 
    bind_cols(.obs = test$sale_price) %>% 
    rsq(truth = .obs, estimate = .pred) %>% 
    pull(.estimate)
}

linmod <- function(train, test){
  linear_reg() %>% 
    set_engine('lm') %>% 
    fit(sale_price ~ ., data = train) %>% 
    predict(new_data = test) %>% 
    bind_cols(.obs = test$sale_price) %>% 
    rsq(truth = .obs, estimate = .pred) %>% 
    pull(.estimate)
}

icv <- results %>% 
  select(impute, training) %>% 
  transmute(impute, .folds = map(training, make_tt, rows_testing)) %>% 
  unnest_longer(.folds) %>% 
  unnest(.folds) %>% 
  group_by(impute, .folds_id) %>% 
  mutate(fold = 1:n()) %>% 
  pivot_wider(names_from = .folds_id, values_from = .folds) %>% 
  mutate(
    icv_rf = map2_dbl(training, testing, rforest),
    icv_lm = map2_dbl(training, testing, linmod)
  )

icv_estimates <- icv %>% 
  group_by(impute) %>% 
  summarize_at(vars(starts_with('icv')), mean)

results <- results %>% 
  mutate(
    ext_rf = map2_dbl(training, testing, rforest),
    ext_lm = map2_dbl(training, testing, linmod)
  ) %>% 
  left_join(icv_estimates)



##
