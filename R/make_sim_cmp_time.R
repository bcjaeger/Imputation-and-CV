##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_sim_cmp_time <- function(null_string = '---') {

  as_tibble(read.fst('data/sim_cmp_time.fst'))

}
