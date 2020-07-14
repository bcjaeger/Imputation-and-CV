##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param data 
##' @param .scenario 
##' @param .miss_mech 
##' @param .type 
##' @param digits 
##'
##' @title

puller <- function(data, .scenario, .miss_mech, .type, .measure, digits = 5){
  filter(data, scenario == .scenario, miss_mech == .miss_mech) %>% 
    select_at(vars(starts_with(.type))) %>%
    select_at(vars(ends_with(.measure))) %>% 
    as.numeric() %>% 
    round(digits) %>% 
    format(nsmall = digits)
}

pull_icv <- function(data, scenario, miss_mech, measure, digits = 5){
  puller(data, scenario, miss_mech, 'imp_cv', measure, digits)
}

pull_cvi <- function(data, scenario, miss_mech, measure, digits = 5){
  puller(data, scenario, miss_mech, 'cv_imp', measure, digits)
}
