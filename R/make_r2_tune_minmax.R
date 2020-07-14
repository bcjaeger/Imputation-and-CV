##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_overall_r2_tune
make_r2_tune_minmax <- function(sim_overall_r2_tune, digits = 2) {

  sim_overall_r2_tune %>% 
    mutate(diff = abs(imp_cv_r2 - cv_imp_r2)) %>% 
    arrange(desc(diff)) %>% 
    slice(1, n()) %>% 
    select(scenario, miss_mech, diff, imp_cv_r2) %>% 
    mutate(scenario = recode(scenario, 
                             's1' = 'scenario 1', 
                             's2' = 'scenario 2', 
                             's3' = 'scenario 3'),
           perc_diff = format(round(100 * diff / imp_cv_r2, digits), 
                              nsmall = digits))
  

}
