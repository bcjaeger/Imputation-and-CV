##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ames_data
make_fig_ames_cmp_time <- function(ames_data) {

  .ames_data <- ames_data %>% 
    unnest(results)
  
  .ames_tune_rf <- .ames_data %>% 
    group_by(seed) %>% 
    summarize(rf_icv = ext_rf[which.max(icv_rf)],
              rf_cvi = ext_rf[which.max(cvi_rf)]) %>% 
    rename_with(.fn = ~str_remove_all(.x, 'rf_')) %>% 
    mutate(mdl = 'rf')
  
  .ames_tune_lm <- .ames_data %>% 
    group_by(seed) %>% 
    summarize(lm_icv = ext_lm[which.max(icv_lm)],
              lm_cvi = ext_lm[which.max(cvi_lm)]) %>% 
    rename_with(.fn = ~str_remove_all(.x, 'lm_')) %>% 
    mutate(mdl = 'lm')
  
  .ames_tune <- bind_rows(.ames_tune_lm, .ames_tune_rf) %>% 
    left_join(select(ames_data, seed, starts_with('time'))) %>% 
    rename(r2_icv = icv, r2_cvi = cvi) %>% 
    pivot_longer(cols = matches('cvi$|icv$'), 
                 names_to = c('cv', 'metric'),
                 names_sep = '_',
                 values_to = 'value') %>% 
    pivot_wider(names_from = cv, values_from = value) %>% 
    mutate(
      mdl = recode(
        mdl, 
        lm = 'Linear regression', 
        rf = 'Random forest'
      ),
      metric = recode(
        metric, 
        icv = 'Imputation before CV',
        cvi = 'Imputation during\neach replicate of CV'
      )
    ) 
  
  medians <- .ames_tune %>%
    select(-seed) %>% 
    group_by(metric) %>% 
    #group_by(mdl, metric) %>% 
    summarize(across(.cols = c(r2, time), .fns = median)) %>% 
    mutate(
      label = tbl_string("Median R-squared: {r2}\nMedian imputation time, {time}s",
                         decimals = c(3,1,0))
    )
  
  fig <- ggplot(.ames_tune) + 
    aes(x = time, y = r2, color = metric) +
    geom_hex(aes(fill=metric), 
             bins = 75, 
             alpha = 0.25,
             show.legend = FALSE) +
    geom_mark_circle(data = medians, 
                     mapping = aes(label = label), 
                     expand = 0.001, 
                     show.legend = FALSE,
                     label.fontsize = 7) +
    geom_point(data = medians,
               mapping = aes(x=time, y=r2, fill = metric), 
               shape = 21, 
               color = 'black', 
               size = 7, 
               show.legend = TRUE,
               inherit.aes = FALSE) +
    #facet_wrap(~mdl, nrow = 2) + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = 'top',
          text = element_text(face = 'bold', colour = 'grey20')) + 
    scale_color_manual(values = c('purple', 'orange')) +
    scale_fill_manual(values = c('purple', 'orange')) + 
    labs(x = 'Computation time required for imputation, seconds',
         y = 'External R-squared obtained by final model',
         fill = 'Imputation order') + 
    guides(color = FALSE)
  
  # it's just easier to save the image and render it with latex
  # than it is to save the figure object and render it with markdown
  ggsave(
    filename = 'fig_ames_cmp_time.png',
    path = 'doc/figs', 
    device = 'png',
    plot = fig, 
    dpi = 600,
    width = 6, 
    height = 6
  )
  
  return("SEE doc/figs/fig_ames_cmp_time.png")

}
