##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_cmp_time
make_sim_cmp_time_smry <- function(sim_cmp_time, null_string) {

  cmp_times_diffs <- sim_cmp_time %>%
    mutate(time = time / 60) %>% 
    pivot_wider(values_from = time, names_from = cv_strat) %>% 
    # turn key into scenarios only - aggregates all md patterns into one
    mutate(key = str_sub(key, 1, 2),
           diff_raw = abs(imp_cv - cv_imp),
           diff_rel = cv_imp / imp_cv) 
  
  .f <- list(mn = mean, sd = sd)
  
  cmp_times_bygroups <- cmp_times_diffs %>% 
    group_by(key, nobs, ncov, action) %>% 
    summarise(across(starts_with('diff'), .f), .groups = 'drop') %>% 
    arrange(ncov, nobs) %>% 
    mutate(nobs = as.character(nobs))
  
  cmp_times_overall <- cmp_times_diffs %>% 
    group_by(key, action) %>% 
    summarise(across(starts_with('diff'), .f), .groups = 'drop') %>% 
    mutate(ncov = 'Overall', nobs = null_string)
  
  bind_rows(cmp_times_bygroups, cmp_times_overall)

}
