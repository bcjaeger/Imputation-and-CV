##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_r2
##' @param diff
##' @param decimals
make_external_r2_diff <- function(sim_r2, diff, decimals) {

  sim_r2 %>%
    separate(key, into = c('scenario', 'miss_mech'), sep = '_') %>% 
    select(scenario, miss_mech, nobs, ncov, external_mn) %>% 
    pivot_wider(values_from = external_mn, names_from = scenario) %>% 
    summarise(value = max(abs(!!enquo(diff)))) %>% 
    mutate(value = tbl_val(value, decimals = decimals + 2)) %>% 
    pull(value)
  
}
