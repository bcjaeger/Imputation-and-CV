##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_sim_data <- function() {

  fnms <- list.files('data', pattern = '^sim_rslts', full.names = FALSE)
  fnms <- str_remove(fnms, '^sim_rslts_')
  fnms <- str_remove(fnms, '\\.fst$')
  
  fobj <- list.files('data', pattern = '^sim_rslts', full.names = TRUE)
  
  fobj %>% 
    map(read.fst) %>% 
    set_names(fnms) %>% 
    bind_rows(.id = 'key') %>% 
    as_tibble()

}
