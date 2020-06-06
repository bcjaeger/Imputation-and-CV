
library(tidyverse)
library(tblStrings)

files <- list.files(
  path = 'results/ameshousing/_rslurm_ames_2020_05_27/',
  pattern = '^results',
  full.names = TRUE
)

data_clean <- map_dfr(files, ~read_rds(.x)[[1]])

write_rds(data_clean, 'results/06-ames_clean.rds')

