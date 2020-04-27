

library(tidyverse)
library(tblStrings)
library(yardstick)
library(gt)
library(knitr)
library(kableExtra)

sim_full <- read_rds('results/02-sim_clean.rds') 

sim_mse <- sim_full %>% 
  select(key, seed, ncov, nobs, results) %>% 
  unnest(cols = results) %>% 
  separate(key, into = c('scenario', 'miss_mech')) %>% 
  mutate(scenario = recode(
    scenario,
    s1 = 'Scenario 1',
    s2 = 'Scenario 2',
    s3 = 'Scenario 3'
  ))

sim_mse_estimates <- filter(sim_mse, cv_strat != 'ex_dat') %>% 
  mutate(
    cv_strat = recode(
      cv_strat, 
      cv_imp = 'imputation in each replicate',
      imp_cv = 'imputation before data splitting.'
    ),
    cv_strat = fct_relevel(factor(cv_strat),
      'imputation before data splitting.')
  )

sim_mse_truth <- filter(sim_mse, cv_strat == 'ex_dat')

fig_r2 <- ggplot(sim_mse_estimates) +
  aes(x = impute, y = r2, group = cv_strat) +
  geom_smooth(mapping = aes(color = cv_strat), linetype = 1,
    method = 'lm', formula = y~poly(x,2), se = FALSE) + 
  geom_smooth(data = sim_mse_truth, 
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

ggsave(
  filename = 'sim_r2.png',
  path = 'SIM_Article/figs', 
  device = 'png',
  plot = fig_r2, 
  dpi = 300,
  width = 6.5, 
  height = 6
)

dev.off()
