

library(tidyverse)
library(tblStrings)
library(yardstick)
library(gt)
library(knitr)
library(kableExtra)

sim_full <- read_rds('results/02-sim_clean.rds') 

# general footnote for tables
mse_source_note <- 'All values are scaled by 100 for convenience'

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
    cv_imp_std  = 100 * sd(cv_imp),
    imp_cv_std  = 100 * sd(imp_cv),
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
  ) %>% 
  ungroup()


# make some datasets to pass into kable function --------------------------

kbl_mse <- mse_smry %>% 
  transmute(
    key, nobs, ncov, 
    external = as.character(
      100 * pointErr(external_mn, external_sd, style = 'brac')
    )
  ) %>% 
  pivot_wider(names_from = key, values_from = external) %>% 
  arrange(ncov, nobs)

kbl_cv_diffs <- mse_smry %>% 
  transmute(
    key, nobs, ncov, 
    cv_diffs = as.character(
      100 * pointErr(abs_diff_mn, abs_diff_sd, style = 'brac')
    )
  ) %>% 
  pivot_wider(names_from = key, values_from = cv_diffs)  %>% 
  arrange(ncov, nobs)
  
kbl_df_cmp <- cmp_times_smry %>% 
  mutate(diff_rel = pointErr(diff_rel_mn, diff_rel_sd, style = 'brac')) %>% 
  select(key, nobs, ncov, action, diff_rel) %>%
  mutate_if(is_pointErr, as.character) %>% 
  pivot_wider(names_from = c(action, key), values_from = diff_rel)  %>% 
  arrange(ncov, nobs)

# a function for tabulating with kable ------------------------------------

tabulate_pointErrs <- function(data, format = 'latex', 
  h1_label, caption, label){
  
  rowpack_index <- table(data$ncov)
  
  h1 <- c(" ", 
    "Scenario 1" = 2, 
    "Scenario 2" = 2, 
    "Scenario 3" = 2
  )
  
  cnames <- c("N", rep(h1_label, times = 3))
  
  align_L <- "l"
  align_C <- rep("c", ncol(data) - 2)
  
  align <- c(align_L, align_C)
  
  data %>%
    select(-ncov) %>% 
    kable(format = format, col.names = cnames, booktabs = TRUE,
      align = align, caption = caption, label = label, escape = FALSE) %>% 
    kable_styling() %>% 
    pack_rows(
      index = rowpack_index, 
      hline_before = FALSE, 
      hline_after = TRUE,
      latex_gap_space = "0.75em"
    ) %>% 
    add_header_above(header = h1) %>% 
    add_footnote(notation = 'none', label = mse_source_note)
  
  
}

tabulate_errors <- function(data, type, format = 'latex', caption, label){
  
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
      align = align, caption = caption, label = label, escape = FALSE) %>% 
    kable_styling() %>% 
    pack_rows(
      index = rowpack_index, 
      hline_before = FALSE, 
      hline_after = TRUE,
      latex_gap_space = "0.75em"
    ) %>% 
    add_header_above(header = h1) %>% 
    add_header_above(header = h2) %>% 
    add_footnote(notation = 'none', label = mse_source_note)
  
}

# external r-squared table ------------------------------------------------

tbl_ext_rsq <- tabulate_pointErrs(
  data = kbl_mse, 
  h1_label = c("MAR", "MCAR"),
  caption = 'True external $R^2$ values for the modeling technique that is internally assessed using \\cvi\\space and \\icv.', 
  label = 'ext_rsq'
)

# absolute cv diffs table -------------------------------------------------

tbl_cv_diffs <- tabulate_pointErrs(
  data = kbl_cv_diffs, 
  h1_label = c("MAR", "MCAR"),
  caption = 'Mean absolute differences in estimates of external $R^2$ between \\cvi\\space and \\icv.', 
  label = 'cv_diffs'
)

# computation time table --------------------------------------------------

tbl_cmp_time <- tabulate_pointErrs(
  data = kbl_df_cmp, 
  h1_label = c("Make", "Model"),
  caption = 'The ratio (standard deviation) of computational time required to make imputed datasets and fit imputed models to each dataset using \\cvi\\space (numerator) and \\icv (denominator).', 
  label = 'cmp_time'
)


# bias table --------------------------------------------------------------

tbl_bias <- tabulate_errors(mse_smry, type = 'rbs', 
  caption = "Bias of external $R^2$ estimates using \\cvi\\space and \\icv",
  label = 'bias')


# variance table ----------------------------------------------------------

tbl_var <- tabulate_errors(mse_smry, type = 'std', 
  caption = 'Standard deviation of external $R^2$ estimates using \\cvi\\space and \\icv.',
  label = 'variance')

# rmse table --------------------------------------------------------------

tbl_rmse <- tabulate_errors(mse_smry, type = 'rmse', 
  caption = 'Root-mean-squared error of external $R^2$ estimates using \\icv and \\cvi', 
  label = 'rmse')


# combine and save outputs ------------------------------------------------

list(
  data_mse = mse_smry,
  data_cmp = cmp_times_smry,
  tbl_ext_rsq = tbl_ext_rsq,
  tbl_cv_diffs = tbl_cv_diffs,
  tbl_cmp_time = tbl_cmp_time,
  tbl_bias = tbl_bias,
  tbl_var = tbl_var,
  tbl_rmse = tbl_rmse
) %>% 
  write_rds('results/03-sim_tabulate.rds')
