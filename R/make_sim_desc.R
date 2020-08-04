##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_sim_desc <- function(sim_cmp_time, sim_data) {

  sim_time_total <- sim_cmp_time %>% 
    summarize(seconds = sum(time)) %>% 
    pluck('seconds') %>% 
    magrittr::divide_by(60 * 60) # seconds to hours 
  
  sim_time_miqr <- sim_cmp_time %>% 
    separate(key, into = c('scenario', 'miss_pattern')) %>% 
    #mutate(time = time / 60) %>% # minutes 
    group_by(action, cv_strat) %>% 
    summarize(
      time = quantile(time, probs = c(0.25, 0.50, 0.75)),
      probs = 100 * c(0.25, 0.50, 0.75),
      .groups = 'keep'
    ) %>%
    pivot_wider(
      names_from = probs, 
      values_from = time, 
      names_prefix = 'time_'
    ) %>% 
    transmute(time = tbl_string("{time_50} ({time_25} - {time_75})")) %>% 
    pivot_wider(names_from = cv_strat, values_from = time) %>% 
    ungroup() %>% 
    filter(action == 'make') %>% 
    select(-action)
  
  sim_time_ratio <- sim_cmp_time %>% 
    separate(key, into = c('scenario', 'miss_pattern')) %>% 
    #mutate(time = time / 60) %>% # minutes 
    group_by(scenario, miss_pattern, seeds, seed, ncov, nobs, action) %>% 
    summarize(
      time_ratio = time[cv_strat == 'cv_imp'] / time[cv_strat == 'imp_cv'],
      .groups = 'drop'
    ) %>%
    group_by(action) %>% 
    summarize(
      time_ratio = quantile(time_ratio, probs = c(0.25, 0.50, 0.75)),
      probs = 100 * c(0.25, 0.50, 0.75),
      .groups = 'keep'
    ) %>%
    pivot_wider(
      names_from = probs, 
      values_from = time_ratio, 
      names_prefix = 'time_'
    ) %>% 
    transmute(ratio = tbl_string("{time_50} ({time_25} - {time_75})")) %>% 
    ungroup() %>% 
    filter(action == 'make') %>% 
    select(-action)
  
  sim_time_compare <- as.list(bind_cols(sim_time_miqr, sim_time_ratio))
  
  sim_time_mdl <- sim_cmp_time %>% 
    filter(action == 'mdl', cv_strat == 'imp_cv') %>% 
    summarise(time = quantile(time, probs = c(0.25, 0.50, 0.75)),
              probs = 100 * c(0.25, 0.50, 0.75)) %>% 
    pivot_wider(
      names_from = probs, 
      values_from = time, 
      names_prefix = 'time_'
    ) %>% 
    transmute(value = tbl_string("{time_50} ({time_25} - {time_75})")) %>% 
    pull(value)
  
  sim_counts <- read.fst('data/sim_counts.fst')
  
  sim_prop_converged_perc <- 
    with(sim_counts, observed / expected) %>% 
    multiply_by(100) %>% 
    tbl_val(decimals = c(3,2,1))
  
  # The total number of settings in this simulation is 72
  # - 2 missing data patterns
  # - 4 sample sizes
  # - 3 covariate structures
  # - 3 data generation scenarios
  
  
  list(
    validation_size = 10000,
    nfolds = 10,
    total_scenarios = 72,
    expected = format(sim_counts$expected, big.mark = ','),
    observed = format(sim_counts$observed, big.mark = ','),
    converged = sim_prop_converged_perc,
    total_hours = sim_time_total,
    time_compare = sim_time_compare,
    time_mdl_fit = sim_time_mdl
  )

}
