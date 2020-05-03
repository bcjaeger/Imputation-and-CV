
# The total number of settings in this simulation is 72
# - 2 missing data patterns
# - 4 sample sizes
# - 3 covariate structures
# - 3 data generation scenarios

library(tidyverse)
library(tblStrings)

directories <- list.dirs(
  path = 'results/simulation', 
  full.names = TRUE,
  recursive = FALSE
)

#directories <- directories[1:3]

data_files <- directories %>% 
  str_remove('results/simulation/_rslurm_sim_x_') %>% 
  str_split('_x_') %>% 
  map(set_names, c('date','scenario','seeds')) %>% 
  enframe(name = 'directory') %>% 
  unnest_wider(value) %>% 
  mutate(directory = directories)

data_files_latest <- data_files %>% 
  group_by(scenario, seeds) %>% 
  arrange(desc(date)) %>% 
  slice(1) %>% 
  ungroup() 

data_full <- data_files_latest %>% 
  transmute(
    scenario,
    seeds,
    files = map(directory, list.files, 
      pattern = 'results_', full.names = TRUE)
  ) %>% 
  unnest(cols = files) %>% 
  mutate(files = map(files, ~read_rds(.x)[[1]]))

data_out <- data_full %>% 
  unnest(cols = files) %>% 
  arrange(scenario, miss_mech, seed) %>% 
  unite(nx, nz, col = 'ncov') %>% 
  mutate(
    scenario = recode(
      scenario, 
      'noGroups'         = 's1', 
      'noGroup'          = 's1',
      'wGroups_observed' = 's2', 
      'wGroups_latent'   = 's3'
    ),
    ncov = recode(
      ncov, 
      '10_10'  = '10 predictors, 10 junk', 
      '10_40'  = '10 predictors, 40 junk',
      '10_490' = '10 predictors, 490 junk'
    )
  ) %>% 
  unite(scenario, miss_mech, col = 'key') %>% 
  select(key, seeds, seed, ncov, nobs, compute_time, results)

sim_validation_size <- data_full$files[[1]]$n_val
sim_nfolds <- data_full$files[[1]]$nfold

sim_count_expected <- length(directories) * 6000
sim_count_observed <- nrow(data_out)

sim_count_failed <- sim_count_expected - sim_count_observed
sim_prop_converged <- sim_count_observed / sim_count_expected

sim_prop_converged_perc <- sim_prop_converged %>% 
  magrittr::multiply_by(100) %>% 
  round(digits = 2) %>% 
  paste0('%')

sim_total_scenarios <- data_out %>% 
  distinct(key, ncov, nobs) %>% 
  nrow()

sim_total_time <- data_out %>% 
  select(compute_time) %>% 
  unnest(compute_time) %>% 
  summarize(seconds = sum(time)) %>% 
  pluck('seconds') %>% 
  magrittr::divide_by(60 * 60) # seconds to hours 

sim_desc <- list(
  validation_size = sim_validation_size,
  nfolds = sim_nfolds,
  expected = format(sim_count_expected, big.mark = ','),
  observed = format(sim_count_observed, big.mark = ','),
  converged = sim_prop_converged_perc,
  total_scenarios = sim_total_scenarios,
  total_hours = sim_total_time
)


write_rds(sim_desc, 'results/02-sim_descriptives.rds')

write_rds(data_out, 'results/02-sim_clean.rds')




