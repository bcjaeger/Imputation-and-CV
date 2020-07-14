##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param sim_data
make_fig_trends_by_nbrs <- function(sim_data) {

  ggdat <- sim_data %>% 
    separate(key, into = c('scenario', 'miss_mech')) %>% 
    mutate(scenario = recode(
      scenario,
      s1 = 'Scenario 1',
      s2 = 'Scenario 2',
      s3 = 'Scenario 3'
    ))
  
  ggdat_estimates <- filter(ggdat, cv_strat != 'ex_dat') %>% 
    mutate(
      cv_strat = recode(
        cv_strat, 
        cv_imp = 'imputation in each replicate',
        imp_cv = 'imputation before data splitting.'
      ),
      cv_strat = fct_relevel(factor(cv_strat),
                             'imputation before data splitting.')
    )
  
  ggdat_truth <- filter(ggdat, cv_strat == 'ex_dat')
  
  fig <- ggplot(ggdat_estimates) +
    aes(x = impute, y = r2, group = cv_strat) +
    geom_smooth(mapping = aes(color = cv_strat), linetype = 1,
                method = 'lm', formula = y~poly(x,2), se = FALSE) + 
    geom_smooth(data = ggdat_truth, 
                mapping = aes(fill = 'True R-squared'),
                linetype = 2, method = 'lm', color = 'black',
                formula = y~poly(x,2), se = FALSE) +
    facet_grid(nobs~scenario, scales = 'free') + 
    scale_color_brewer(palette = 'Dark2') + 
    labs(
      x = 'Number of neighbors used to impute missing values',
      y = 'R-squared', 
      fill = 'External validation',
      color = 'Cross-validation with'
    ) + 
    theme_bw() +
    theme(
      legend.position = 'top', 
      legend.direction = 'vertical',
      panel.grid = element_blank()
    ) + 
    scale_y_continuous(labels = scales::percent)
  
  # it's just easier to save the image and render it with latex
  # than it is to save the figure object and render it with markdown
  ggsave(
    filename = 'fig_trends_by_nbrs.png',
    path = 'doc/figs', 
    device = 'png',
    plot = fig, 
    dpi = 300,
    width = 6.5, 
    height = 6
  )
  
  return("SEE doc/figs/fig_trends_by_nbrs.png")

}
