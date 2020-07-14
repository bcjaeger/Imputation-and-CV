##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_r2
##' @param caption
##' @param label
##' @param decimals
##' @param scale_by
##' @param nobs_lvls
make_tbl_1header <-function(sim_r2,
                            col_mean,
                            col_sd,
                            caption,
                            label,
                            decimals,
                            scale_by,
                            nobs_lvls,
                            nobs_lbls) {
  
  data_tbl <- sim_r2$values_all %>% 
    rename(col_mean = !!enquo(col_mean),
           col_sd = !!enquo(col_sd)) %>% 
    transmute(
      key, 
      ncov, 
      nobs = factor(nobs, levels = nobs_lvls, labels = nobs_lbls), 
      tbv = tbl_string(
        "{scale_by*col_mean} ({scale_by*col_sd})",
        decimals = decimals
      )
    ) %>% 
    pivot_wider(names_from = key, values_from = tbv) %>% 
    arrange(ncov, nobs)
  
  h1_label <- c("MAR", "MCAR")
  
  col.names <- c("N", rep(h1_label, times = 3))
  
  header1 <- c(" ", 
               "Scenario 1" = 2, 
               "Scenario 2" = 2, 
               "Scenario 3" = 2)
  
  
  align_L <- "l"
  align_C <- rep("c", ncol(data_tbl) - 2)
  align <- c(align_L, align_C)
  
  make_sim_tbl(data = data_tbl,
               col.names = col.names,
               align = align,
               format = 'latex',
               scale_by = scale_by,
               caption = caption,
               header1 = header1,
               label = label)
  
}
