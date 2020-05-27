
library(tidyverse)
library(tblStrings)

files <- list.files(
  path = 'results/ameshousing/_rslurm_ames_2020_05_18/',
  pattern = '^results',
  full.names = TRUE
)

data_clean <- map(files, ~read_rds(.x)[[1]]) %>% 
  map_dfr(select, impute, matches('^ext|^icv|^cvi'), .id = 'seed')

write_rds(data_clean, 'results/06-ames_clean.rds')

