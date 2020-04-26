

library(tidyverse)
library(tblStrings)
library(yardstick)
library(gt)
library(knitr)
library(kableExtra)

sim_full <- read_rds('results/02-sim_clean.rds') 

# create mse data ---------------------------------------------------------

mse_values <- sim_full %>% 
  select(-compute_time) %>% 
  unnest(results) %>% 
  rename(neighbors = impute) 

mse_values %>% count(key)

mse_source_note <- 'All values are scaled by 100 for convenience'

# occasionally, there are NA values for mse
# reason: <50 neighbors were available in some small samples.
# solution: drop rows where any of cv_imp, imp_cv, or ex_dat are missing
# seems to be a very small number of rows.

mse_smry <- mse_values %>% 
  select(-rmse) %>% 
  pivot_wider(names_from = cv_strat, values_from = r2) %>%
  drop_na() %>% 
  group_by(key, nobs, ncov) %>% 
  mutate(abs_diff = abs(cv_imp - imp_cv)) %>% 
  summarise(
    nrows = n(),
    external    = 100 * pointErr(mean(ex_dat), sd(ex_dat), style = 'brac'),
    cv_diffs    = 100 * pointErr(mean(abs_diff), sd(abs_diff), style = 'brac'),
    cv_imp_rbs  = 100 * mean(ex_dat - cv_imp),
    imp_cv_rbs  = 100 * mean(ex_dat - imp_cv),
    cv_imp_sd   = 100 * sd(cv_imp),
    imp_cv_sd   = 100 * sd(imp_cv),
    cv_imp_rmse = 100 * rmse_vec(ex_dat, cv_imp),
    imp_cv_rmse = 100 * rmse_vec(ex_dat, imp_cv)
  ) %>% 
  ungroup()


# external R-squared table ------------------------------------------------

scenario_recode <- function(x)
  str_replace(x, pattern = '^S', replacement = 'Scenario ')

tbl_ex_rsq <- mse_smry %>% 
  select(key, nobs, ncov, external) %>% 
  pivot_wider(names_from = key, values_from = external) %>% 
  rename_at(vars(contains('_')), toupper) %>% 
  rename_at(vars(contains('_')), scenario_recode) %>% 
  gt(rowname_col = 'nobs', groupname_col = 'ncov') %>% 
  tab_spanner_delim(delim = '_') %>% 
  tab_source_note(mse_source_note)

tbl_ex_rsq

# absolute difference between cvi and icv table ---------------------------

tbl_abs_diff <- mse_smry %>% 
  select(key, nobs, ncov, cv_diffs) %>% 
  pivot_wider(
    names_from = key, 
    values_from = cv_diffs
  ) %>% 
  rename_at(vars(contains('_')), toupper) %>% 
  rename_at(vars(contains('_')), scenario_recode) %>% 
  gt(rowname_col = 'nobs', groupname_col = 'ncov') %>% 
  tab_spanner_delim(delim = '_') %>% 
  tab_source_note(mse_source_note)

tbl_abs_diff

# a function for tabulating with kable ------------------------------------

tabulate_errors <- function(data, type, format = 'html'){
  
  kbl_df <- data %>% 
    select(key, nobs, ncov, ends_with(type)) %>% 
    pivot_wider(
      names_from = key, 
      values_from = ends_with(type), 
      names_sep = '..'
    ) %>% 
    select(nobs, ncov, 
      ends_with('s1_mcar'), 
      ends_with('s2_mcar'),
      ends_with('s3_mcar'), 
      ends_with('s1_mar'),
      ends_with('s2_mar'), 
      ends_with('s3_mar')
    ) %>% 
    arrange(ncov, nobs)
  
  rowpack_index <- table(kbl_df$ncov)
  
  h1 <- c(" ", 
    "Scenario 1" = 2, 
    "Scenario 2" = 2, 
    "Scenario 3" = 2, 
    "Scenario 1" = 2, 
    "Scenario 2" = 2, 
    "Scenario 3" = 2
  )
  
  h2 <- c(" ",
    "Missing completely at random" = 6,
    "Missing at random" = 6
  )
  
  cnames <- c("N", rep(c("CV-I", "I-CV"), times = 6))
  
  
  align_L <- "l"
  align_C <- rep("c", ncol(kbl_df) - 2)
  
  align <- c(align_L, align_C)
  
  kbl_df %>%
    select(-ncov) %>% 
    mutate_at(vars(contains('..')), tbv_round) %>% 
    kable(format = format, col.names = cnames, align = align) %>% 
    kable_styling() %>% 
    pack_rows(index = rowpack_index) %>% 
    add_header_above(header = h1) %>% 
    add_header_above(header = h2) %>% 
    add_footnote(notation = 'none', label = mse_source_note)
  
}

# bias table --------------------------------------------------------------

tbl_bias <- tabulate_errors(mse_smry, type = 'rbs')


# variance table ----------------------------------------------------------

tbl_var <- tabulate_errors(mse_smry, type = 'sd')

# rmse table --------------------------------------------------------------

tbl_rmse <- tabulate_errors(mse_smry, type = 'rmse')


# create computational time data ------------------------------------------

cmp_times <- sim_full %>%
  select(-results) %>%
  unnest(compute_time)

cmp_times_smry <- cmp_times %>%
  mutate(time = time / 60) %>% 
  pivot_wider(values_from = time, names_from = cv_strat) %>% 
  mutate(
    # turn key into scenarios only - aggregates all md patterns into one
    key = str_sub(key, 1, 2),
    diff_raw = abs(imp_cv - cv_imp),
    diff_rel = cv_imp / imp_cv
  ) %>% 
  group_by(key, nobs, ncov, action) %>% 
  summarize_at(
    vars(starts_with('diff')),
    ~pointErr(mean(.x), sd(.x), style = 'brac')
  ) %>% 
  ungroup()

# computational overhead table --------------------------------------------

cmp_times_tbl <- cmp_times_smry %>%
  pivot_wider(names_from = action, values_from = c(diff_raw, diff_rel)) %>% 
  select(
    # dropping raw times due to space constraint
    # diffraw_make = diff_raw_make
    # diffraw_mdl = diff_raw_mdl, 
    key, nobs, ncov,
    diffmake = diff_rel_make, 
    diffmdl = diff_rel_mdl
  ) %>% 
  pivot_wider(names_from = key, values_from = c(diffmake, diffmdl)) %>% 
  select(nobs, ncov, ends_with('1'), ends_with('2'), ends_with('3')) %>% 
  gt(rowname_col = 'nobs', groupname_col = 'ncov') %>%
  tab_stubhead(label = html('Observations </br> in training set')) %>%
  tab_spanner(
    columns = vars(diffmake_s1, diffmdl_s1),
    label = "Scenario 1"
  ) %>%
  tab_spanner(
    columns = vars(diffmake_s2, diffmdl_s2),
    label = "Scenario 2"
  ) %>%
  tab_spanner(
    columns = vars(diffmake_s3, diffmdl_s3),
    label = "Scenario 3"
  ) %>%
  cols_label(
    diffmake_s1 = 'Impute',
    diffmdl_s1  = 'Fit',
    diffmake_s2 = 'Impute',
    diffmdl_s2  = 'Fit',
    diffmake_s3 = 'Impute',
    diffmdl_s3  = 'Fit'
  )

cmp_times_tbl


# combine and save outputs ------------------------------------------------

list(
  externa_rsq = tbl_ex_rsq,
  absolute_diffs = tbl_abs_diff,
  bias = tbl_bias,
  variance = tbl_var,
  mse = tbl_rmse,
  compute_times = cmp_times_tbl
) %>% 
  write_rds('results/03-sim_tabulate.rds')


rmarkdown::draft(
  "99-SIM_Article.Rmd", template = "sim_article",
  package = "rticles"
)
