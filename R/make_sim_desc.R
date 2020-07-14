##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_sim_desc <- function(sim_cmp_time, sim_data) {

  sim_time_total <- sim_cmp_time %>% 
    summarize(seconds = sum(time)) %>% 
    pluck('seconds') %>% 
    magrittr::divide_by(60 * 60) # seconds to hours 
  
  sim_time_compare <- sim_cmp_time %>% 
    filter(action == 'make') %>% 
    group_by(cv_strat) %>% 
    summarize(time_minutes = mean(time) / 60, .groups = 'drop') %>% 
    mutate(
      time_ratio = max(time_minutes) / min(time_minutes),
      time_minutes = format(round(time_minutes, 2), nsmall = 2)
    )
  
  sim_time_mdl <- sim_cmp_time %>% 
    filter(action == 'mdl', cv_strat == 'imp_cv') %>% 
    summarise(time_minutes = mean(time) / 60) %>% 
    mutate(time_minutes = format(round(time_minutes, 2), nsmall = 2)) %>% 
    as.character()
  
  sim_counts <- read.fst('data/sim_counts.fst')
  
  sim_prop_converged_perc <- 
    with(sim_counts, observed / expected) %>% 
    multiply_by(100) %>% 
    tbl_val(decimals = c(3,2,1))
  
  # The total number of settings in this simulation is 72
  # - 2 missing data patterns
  # - 4 sample sizes
  # - 3 covariate structures
  # - 3 data generation scenarios
  
  
  list(
    validation_size = 10000,
    nfolds = 10,
    total_scenarios = 72,
    expected = format(sim_counts$expected, big.mark = ','),
    observed = format(sim_counts$observed, big.mark = ','),
    converged = sim_prop_converged_perc,
    total_hours = sim_time_total,
    time_compare = sim_time_compare,
    time_mdl_fit = sim_time_mdl
  )

}
