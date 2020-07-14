
# This code deals with raw results from the simulation.
# The raw results are not pushed to github due to space limitations.
# => this is not reproducible unless you generate the raw data locally.

# Clean simulation data -----------------------------------------------------

library(tidyverse)
library(fst)
library(glue)

directories <- list.dirs(
  path = 'results_raw/simulation', 
  full.names = TRUE,
  recursive = FALSE
)

#directories <- directories[1:3]

data_files <- directories %>% 
  str_remove('results_raw/simulation/_rslurm_sim_x_') %>% 
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

nsims_expected <- nrow(data_files_latest) * 6000

data_full <- data_files_latest %>% 
  transmute(
    scenario,
    seeds,
    files = map(directory, list.files, 
      pattern = 'results_', full.names = TRUE)
  ) %>% 
  unnest(cols = files) %>% 
  mutate(files = map(files, ~read_rds(.x)[[1]]))

nsims_observed <- nrow(data_full)

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

compute_time <- data_out %>% 
  select(-results) %>% 
  unnest(compute_time)

sim_results <- data_out %>% 
  select(-compute_time) %>% 
  unnest(results) %>%
  group_by(key) %>% 
  nest() %>% 
  deframe()


tibble(expected = nsims_expected, observed = nsims_observed) %>% 
  write.fst('data/sim_counts.fst')

write.fst(compute_time, "data/sim_cmp_time.fst", compress = 100)

for(f in seq_along(sim_results)){
  
  fname <- names(sim_results)[f]
  
  write.fst(sim_results[[f]], 
            glue('data/sim_rslts_{fname}.fst'), 
            compress = 100)
  
}




