

main <- function(seed=1){
  
  library(AmesHousing)
  library(ipa)
  library(tidyverse)
  library(tidymodels)
  library(janitor)
  library(mice)
  library(glue)
  library(magrittr)
  
  set.seed(seed)
  
  # cleaned Ames data
  data_all <- make_ames() %>%
    drop_na() %>%
    clean_names(case = 'snake') %>%
    mutate(sale_price = log(sale_price))
  
  # create four missing data patterns
  patterns <- matrix(1, ncol = ncol(data_all), nrow = 4) %>%
    set_colnames(names(data_all)) %>%
    set_rownames(glue("pattern {1:4}"))
  
  dnames <- names(data_all)
  
  patterns[1, str_detect(dnames, '^lot|^garage')] <- 0
  patterns[2, str_detect(dnames, '^longi|^lati')] <- 0
  patterns[3, str_detect(dnames, '^bsmt|year')] <- 0
  patterns[4, str_detect(dnames, '\\_qual|gr_liv_area')] <- 0
  
  # ampute the cleaned Ames data
  data_amp <- ampute(
    data_all,
    prop = 1,
    patterns = patterns,
    mech = 'MCAR',
    bycases = TRUE
  ) %>%
    use_series('amp')
  
  # retain only the variables with missing data
  # to make the proportion of missing data higher
  miss_vars <- data_amp %>% 
    select_if(~any(is.na(.x)))
  
  data_analysis <- select(data_amp, sale_price) %>% 
    bind_cols(miss_vars) %>% 
    as_tibble()
  
  perc_miss_overall <- mean(is.na(data_analysis))
  
  # training / testing data
  data_split <- initial_split(data_analysis, prop = 3/4)
  
  # impute them using knn, separately
  start <- Sys.time()
  data_imputed_knn <- training(data_split) %>% 
    brew_nbrs(outcome = sale_price) %>%
    verbose_on(level = 1) %>%
    spice(with = spicer_nbrs(k_neighbors = 1:35)) %>%
    mash() %>% 
    stir() 
  stop <- Sys.time()
  
  cmp_time_icv <- as.numeric(stop-start, units = 'secs')
  
  data_imputed_knn <- data_imputed_knn %>% 
    ferment(data_new = testing(data_split)) %>% 
    bottle(type = 'tibble') %>% 
    use_series('wort') %>% 
    as_tibble() %>% 
    select(-pars)
  
  # this recipe gets used on the imputed data
  recipe_raw_no_impute <- 
    recipe(sale_price ~ ., data = training(data_split)) %>% 
    step_novel(all_nominal(), new_level = 'new_category') %>% 
    step_other(all_nominal(), threshold = 0.10) 
  
  # this recipe is used to do mean/mode imputes
  recipe_raw_mn_impute <- 
    recipe(sale_price ~ ., data = training(data_split)) %>% 
    step_meanimpute(all_numeric()) %>%
    step_modeimpute(all_nominal()) %>% 
    step_novel(all_nominal(), new_level = 'new_category') %>% 
    step_other(all_nominal(), threshold = 0.10)
  
  # get the train/test data ready for modeling
  data_knn_processed <- data_imputed_knn %>% 
    mutate(
      reci_prepped = map(training, ~prep(recipe_raw_no_impute, .x)),
      training = map(reci_prepped, juice),
      testing = map2(reci_prepped, testing, bake)
    )
  
  # get the mean mode imputed train/test data ready for modeling
  data_mn_processed <- tibble(impute = 1, 
    training = list(training(data_split)),
    testing = list(testing(data_split))
  ) %>% 
    mutate(
      reci_prepped = map(training, ~prep(recipe_raw_mn_impute, .x)),
      training = map(reci_prepped, juice),
      testing = map2(reci_prepped, testing, bake)
    )
  
  
  # modeling functions
  
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
  
  # model scores on external testing set if we use knn
  external_knn <- data_knn_processed %>% 
    mutate(
      ext_rf = map2_dbl(training, testing, rforest),
      ext_lm = map2_dbl(training, testing, linmod)
    )
  
  # model scores on external testing set if we use mean/mode
  external_mn <- data_mn_processed %>% 
    mutate(
      ext_rf = map2_dbl(training, testing, rforest),
      ext_lm = map2_dbl(training, testing, linmod)
    )
  
  # setting up 10-fold CV for the training set
  training_folds <- vfold_cv(training(data_split), v = 10)
  rows_testing <- map(training_folds$splits, complement)
  
  # make train/test data based on testing indices
  make_tt <- function(data, test_rows){
    list(
      training = map(test_rows, ~data[-.x, ]),
      testing = map(test_rows, ~data[.x, ])
    )
  }
  
  # impute and then run cross-validation
  icv <- data_imputed_knn %>% 
    transmute(impute, .folds = map(training, make_tt, rows_testing)) %>% 
    unnest_longer(.folds) %>% 
    unnest(.folds) %>% 
    group_by(impute, .folds_id) %>% 
    mutate(fold = 1:n()) %>% 
    pivot_wider(names_from = .folds_id, values_from = .folds) %>% 
    mutate(
      reci_prepped = map(training, ~prep(recipe_raw_no_impute, training = .x)),
      training = map(reci_prepped, juice),
      testing = map2(reci_prepped, testing, bake),
      icv_rf = map2_dbl(training, testing, rforest),
      icv_lm = map2_dbl(training, testing, linmod)
    )
  
  # aggregate the icv results. for each value of k-neighbors, 
  # take the mean of the internal rsq estimate for each model.
  # this averages one score over all 10 CV folds per neighbor.
  icv_estimates <- icv %>% 
    group_by(impute) %>% 
    summarize_at(vars(starts_with('icv')), mean)
  
  # run CV and impute during each replicate.
  start <- Sys.time()
  cvi <- training_folds$splits %>% 
    map(
      ~training(.x) %>% 
        brew_nbrs(outcome = sale_price) %>%
        verbose_on(level = 1) %>%
        spice(with = spicer_nbrs(k_neighbors = 1:35)) %>%
        mash() %>% 
        stir() %>% 
        ferment(data_new = testing(.x)) %>% 
        bottle(type = 'tibble') %>% 
        use_series('wort') %>% 
        as_tibble() %>% 
        select(-pars)
    )
  stop <- Sys.time()
  cmp_time_cvi <- as.numeric(stop-start, units = 'secs')
  
  # aggregate cvi estimates the same way as we aggregated icv estimates.
  cvi_estimates <- cvi %>%
    bind_rows(.id = 'fold') %>% 
    mutate(
      reci_prepped = map(training, ~prep(recipe_raw_no_impute, training = .x)),
      training = map(reci_prepped, juice),
      testing = map2(reci_prepped, testing, bake),
      cvi_rf = map2_dbl(training, testing, rforest),
      cvi_lm = map2_dbl(training, testing, linmod)
    ) %>% 
    group_by(impute) %>% 
    summarize_at(vars(starts_with('cvi')), mean)
  
  results <- external_knn %>%
    select(impute, starts_with("ext")) %>% 
    left_join(icv_estimates) %>% 
    left_join(cvi_estimates) %>% 
    add_row(impute = 0, 
      ext_lm = external_mn$ext_lm,
      ext_rf = external_mn$ext_rf) %>% 
    arrange(impute)
  
  tibble(
    seed = seed, 
    time_icv = cmp_time_icv,
    time_cvi = cmp_time_cvi,
    results = list(results)
  )

}


library(tidyverse)
library(rslurm)

start_iter <- 1
stop_iter <- 5000
mem_per_cpu <- 18000

today <- Sys.time() %>% 
  as.Date() %>% 
  str_replace_all(pattern = '\\-', replacement = '_')

jobname <- as.character(glue::glue(
  "ames_{today}"
))

pars <- data.frame(seed = seq(start_iter, stop_iter, by = 1))

sopt <- 
  list(
    partition='short',
    time='11:59:00',
    share=TRUE,
    'mem-per-cpu'= mem_per_cpu
  )

slurm_apply(
  main,
  pars,
  jobname = jobname,
  slurm_options = sopt,
  nodes = nrow(pars),
  cpus_per_node = 1,
  submit = TRUE
)



