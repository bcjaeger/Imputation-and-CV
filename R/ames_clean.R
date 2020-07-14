
library(tidyverse)
library(tblStrings)

files <- list.files(
  path = 'results_raw/ameshousing/_rslurm_ames_2020_05_27/',
  pattern = '^results',
  full.names = TRUE
)

data_clean <- map_dfr(files, ~read_rds(.x)[[1]])

data_clean %>% 
  write_rds("data/ames_rslts.rds", compress = 'gz')
