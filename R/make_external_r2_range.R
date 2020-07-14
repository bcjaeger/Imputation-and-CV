##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_external_r2_range <- function(sim_r2, decimals) {

  sim_r2 %>%
    summarize(min = min(external_mn), max = max(external_mn)) %>% 
    mutate(across(everything(), tbl_val, decimals = decimals + 1))

}
