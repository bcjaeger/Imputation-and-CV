

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
    external_mn = mean(ex_dat),
    external_sd = sd(ex_dat),
    abs_diff_mn = mean(abs_diff),
    abs_diff_sd = sd(abs_diff),
    cv_imp_rbs  = 100 * mean(ex_dat - cv_imp),
    imp_cv_rbs  = 100 * mean(ex_dat - imp_cv),
    cv_imp_std   = 100 * sd(cv_imp),
    imp_cv_std   = 100 * sd(imp_cv),
    cv_imp_rmse = 100 * rmse_vec(ex_dat, cv_imp),
    imp_cv_rmse = 100 * rmse_vec(ex_dat, imp_cv)
  ) %>% 
  ungroup()

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
  summarize(
    diff_raw_mn = mean(diff_raw),
    diff_raw_sd = sd(diff_raw),
    diff_rel_mn = mean(diff_rel),
    diff_rel_sd = sd(diff_rel)
  )

# a function for tabulating with kable ------------------------------------

tabulate_errors <- function(data, type, format = 'latex', caption){
  
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
  
  cnames <- c("N", rep(c("\\cvi", "\\icv"), times = 6))
  
  
  align_L <- "l"
  align_C <- rep("c", ncol(kbl_df) - 2)
  
  align <- c(align_L, align_C)
  
  kbl_df %>%
    select(-ncov) %>% 
    mutate_at(vars(contains('..')), tbv_round) %>% 
    kable(format = format, col.names = cnames, booktabs = TRUE,
      align = align, caption = caption, escape = FALSE) %>% 
    kable_styling() %>% 
    pack_rows(index = rowpack_index, 
      hline_before = TRUE, hline_after = TRUE) %>% 
    add_header_above(header = h1) %>% 
    add_header_above(header = h2) %>% 
    add_footnote(notation = 'none', label = mse_source_note)
  
}

# bias table --------------------------------------------------------------

tbl_bias <- tabulate_errors(mse_smry, type = 'rbs', 
  caption = "Bias of external $R^2$ estimates using \\icv and \\cvi")


# variance table ----------------------------------------------------------

tbl_var <- tabulate_errors(mse_smry, type = 'std', 
  caption = 'Variance of external $R^2$ estimates using \\icv and \\cvi')

# rmse table --------------------------------------------------------------

tbl_rmse <- tabulate_errors(mse_smry, type = 'rmse', 
  caption = 'Mean-squared error of external $R^2$ estimates using \\icv and \\cvi')


# combine and save outputs ------------------------------------------------

list(
  data_mse = mse_smry,
  data_cmp = cmp_times_smry,
  kbl_bias = tbl_bias,
  kbl_vrnc = tbl_var,
  kbl_rmse = tbl_rmse
) %>% 
  write_rds('results/03-sim_tabulate.rds')
