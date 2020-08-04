##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_cmp_time
make_sim_cmp_time_inline <- function(sim_cmp_time) {

  sim_cmp_time %>% 
    separate(key, into = c('scenario', 'miss_pattern')) %>% 
    filter(nobs == 5000) %>% 
    mutate(time = time / 60) %>% # minutes 
    group_by(action, cv_strat) %>% 
    summarize(
      time = quantile(time, probs = c(0.25, 0.50, 0.75)),
      probs = 100 * c(0.25, 0.50, 0.75),
      .groups = 'keep'
    ) %>%
    pivot_wider(
      names_from = probs, 
      values_from = time, 
      names_prefix = 'time_'
    ) %>% 
    transmute(time = tbl_string("{time_50} ({time_25} - {time_75})")) %>% 
    pivot_wider(names_from = cv_strat, values_from = time) %>% 
    ungroup() %>% 
    filter(action == 'make') %>% 
    select(-action) %>% 
    as.list()

}
