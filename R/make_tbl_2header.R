##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_tbl_2header <- function(sim_r2,
                             type,
                             caption,
                             label,
                             decimals,
                             scale_by,
                             nobs_lvls,
                             nobs_lbls) {
  
  data_tbl <- sim_r2 %>% 
    select(key, nobs, ncov, ends_with(type)) %>% 
    pivot_wider(
      names_from = key, 
      values_from = ends_with(type), 
      names_sep = '..'
    ) %>% 
    select(nobs, ncov, 
           ends_with('s1_mcar'), 
           ends_with('s2_mcar'),
           ends_with('s3_mcar'), 
           ends_with('s1_mar'),
           ends_with('s2_mar'), 
           ends_with('s3_mar')
    ) %>% 
    mutate(
      across(matches('s\\d?\\_mcar$|s\\d?\\_mar$'), 
             ~tbl_val(scale_by * .x, decimals = decimals)),
      nobs = factor(nobs, levels = nobs_lvls, labels = nobs_lbls)
    ) %>% 
    arrange(ncov, nobs)
  
  header1 <- c(" ", 
               "Scenario 1" = 2, 
               "Scenario 2" = 2, 
               "Scenario 3" = 2, 
               "Scenario 1" = 2, 
               "Scenario 2" = 2, 
               "Scenario 3" = 2)
  
  header2 <- c(" ",
               "Missing completely at random" = 6,
               "Missing at random" = 6)
  
  col.names <- c("N", rep(c("\\cvi", "\\icv"), times = 6))
  
  
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
               header2 = header2,
               label = label)
  
}
