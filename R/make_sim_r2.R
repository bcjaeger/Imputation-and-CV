##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_data
make_sim_r2 <- function(sim_data, 
                        null_string = '---') {

  # occasionally, there are NA values for rmse and r2
  # reason: <35 neighbors were available in some small samples.
  # So drop rows where any of cv_imp, imp_cv, or ex_dat are missing.
  # Seems to be a very small number of rows.
  
  r2_data <- sim_data %>% 
    select(-rmse) %>% 
    pivot_wider(names_from = cv_strat, values_from = r2) %>%
    drop_na() %>% 
    mutate(abs_diff = abs(cv_imp - imp_cv)) 
  
  .f <- function(.x){
    .x %>% 
      summarise(
        nrows = n(),
        external_mn = mean(ex_dat),
        external_sd = sd(ex_dat),
        abs_diff_mn = mean(abs_diff),
        abs_diff_sd = sd(abs_diff),
        cv_imp_rbs  = mean(ex_dat - cv_imp),
        imp_cv_rbs  = mean(ex_dat - imp_cv),
        cv_imp_std  = sd(cv_imp),
        imp_cv_std  = sd(imp_cv),
        cv_imp_rmse = rmse_vec(ex_dat, cv_imp),
        imp_cv_rmse = rmse_vec(ex_dat, imp_cv),
        .groups = 'drop'
      )
  }
  
  r2_values_key <- .f(group_by(r2_data, key)) %>% 
    mutate(ncov = 'Overall', nobs = null_string)
  
  r2_values_bygroups <- .f(group_by(r2_data, key, nobs, ncov)) %>% 
    mutate(nobs = as.character(nobs))
  
  r2_values_smry <- bind_rows(r2_values_key, r2_values_bygroups) %>% 
    relocate(nobs, ncov, .after = key)
  
  r2_tune <- r2_data %>% 
    group_by(seed, key, ncov, nobs) %>% 
    summarise(
      cv_imp_nbrs = impute[which.max(cv_imp)],
      imp_cv_nbrs = impute[which.max(imp_cv)],
      cv_imp_r2 = ex_dat[which.max(cv_imp)],
      imp_cv_r2 = ex_dat[which.max(imp_cv)],
      .groups = 'drop'
    )
  
  ..f <- function(.x){
    .x %>% 
      summarise(
        cv_imp_r2 = mean(cv_imp_r2),
        imp_cv_r2 = mean(imp_cv_r2),
        cv_imp_nbrs = round(median(cv_imp_nbrs)),
        imp_cv_nbrs = round(median(imp_cv_nbrs)),
        .groups = 'drop'
      )
  }
  
  r2_tune_key <- ..f(group_by(r2_tune, key)) %>% 
    mutate(ncov = 'Overall', nobs = null_string)
  
  r2_tune_bygroups <- ..f(group_by(r2_tune, key, nobs, ncov)) %>% 
    mutate(nobs = as.character(nobs))
  
  r2_tune_smry <- bind_rows(r2_tune_key, r2_tune_bygroups) %>% 
    relocate(nobs, ncov, .after = key)
  
  list(
    values_all = r2_values_smry,
    values_tuned = r2_tune_smry
  )

}
