

the_plan <- drake_plan(
  
  ## NOTE: Simulations are done in a separate project using
  ## high performance computing.
  
  # parameters ----
  
  scale_by = 100,
  decimals = c(2,2,1),
  null_string = '---',
  nobs_lvls = c('100', '500', '1000', '5000', null_string),
  nobs_lbls = c('100', '500', '1,000', '5,000', null_string),
  
  # data management ----
  
  sim_data = make_sim_data(),
  sim_cmp_time = make_sim_cmp_time(),
  sim_cmp_time_smry = make_sim_cmp_time_smry(sim_cmp_time, null_string),
  sim_desc = make_sim_desc(sim_cmp_time, sim_data),
  sim_r2 = make_sim_r2(sim_data),
  
  ames_data = make_ames_data(),
  ames_desc = make_ames_desc(ames_data, decimals = decimals + 2),
  
  # table captions ----
  
  caption_tbl_true_r2 = paste(
    'True external $R^2$ values for the modeling',
    'technique that is internally assessed using',
    '\\cvi\\space and \\icv.'
  ),
  
  caption_tbl_diffs_r2 = paste(
    'Mean absolute differences in estimates of',
    'external $R^2$ between \\cvi\\space and \\icv.'
  ),
  
  caption_tbl_rbs_r2 = paste(
    "Bias of external $R^2$ estimates using \\cvi\\space and \\icv."
  ),
  
  caption_tbl_std_r2 = paste(
    "Standard deviation of external $R^2$",
    "estimates using \\cvi\\space and \\icv."
  ),
  
  caption_tbl_rmse_r2 = paste(
    "Root-mean-squared error of external $R^2$",
    "estimates using \\cvi\\space and \\icv."
  ),
  
  caption_tbl_tune_r2 = paste(
    "Mean external $R^2$ when \\cvi\\space and \\icv\\space",
    "were applied to tune the number of neighbors used for imputation."
  ),
  
  # figure captions ----
  
  # table creation ----
  
  tbl_true_r2 = make_tbl_1header(sim_r2,
                                 col_mean = 'external_mn',
                                 col_sd = 'external_sd',
                                 caption = caption_tbl_true_r2,
                                 label = 'ext_rsq',
                                 decimals = decimals, 
                                 scale_by = scale_by,
                                 nobs_lvls = nobs_lvls,
                                 nobs_lbls = nobs_lbls),
  
  tbl_diffs_r2 = make_tbl_1header(sim_r2, 
                                  col_mean = 'abs_diff_mn',
                                  col_sd = 'abs_diff_sd',
                                  caption = caption_tbl_diffs_r2,
                                  label = 'cv_diffs',
                                  decimals = decimals, 
                                  scale_by = scale_by,
                                  nobs_lvls = nobs_lvls,
                                  nobs_lbls = nobs_lbls),
  
  tbl_rbs_r2 = make_tbl_2header(sim_r2$values_all,
                                type = 'rbs',
                                caption = caption_tbl_rbs_r2,
                                label = 'bias',
                                decimals = decimals,
                                scale_by = scale_by,
                                nobs_lvls = nobs_lvls,
                                nobs_lbls = nobs_lbls),
  
  tbl_std_r2 = make_tbl_2header(sim_r2$values_all,
                                type = 'std',
                                caption = caption_tbl_std_r2,
                                label = 'variance',
                                decimals = decimals,
                                scale_by = scale_by,
                                nobs_lvls = nobs_lvls,
                                nobs_lbls = nobs_lbls),
  
  tbl_rmse_r2 = make_tbl_2header(sim_r2$values_all,
                                 type = 'rmse',
                                 caption = caption_tbl_rmse_r2,
                                 label = 'rmse',
                                 decimals = decimals,
                                 scale_by = scale_by,
                                 nobs_lvls = nobs_lvls,
                                 nobs_lbls = nobs_lbls),
  
  tbl_tune_r2 = make_tbl_2header(sim_r2$values_tuned,
                                 type = 'r2',
                                 caption = caption_tbl_tune_r2,
                                 label = 'tune',
                                 decimals = decimals,
                                 scale_by = scale_by,
                                 nobs_lvls = nobs_lvls,
                                 nobs_lbls = nobs_lbls),
  
  
  # figure creation ----
  
  fig_trends_by_nbrs = make_fig_trends_by_nbrs(sim_data),
  
  # inline results ----
  
  r2_ext_range = make_external_r2_range(sim_r2$values_all, decimals),
  r2_ext_diff23 = make_external_r2_diff(sim_r2$values_all, s2-s3, decimals),

  sim_overall_r2_values = sim_r2$values_all %>% 
    filter(ncov == 'Overall') %>% 
    separate(key, into = c('scenario', 'miss_mech')),
  
  sim_overall_r2_tune = sim_r2$values_tuned %>%
    filter(ncov == 'Overall') %>% 
    separate(key, into = c('scenario', 'miss_mech')) %>% 
    mutate(diff = abs(imp_cv_r2 - cv_imp_r2)),
  
  r2_tune_head2head = as.list(summarize(
    .data = sim_r2$values_tuned,
    cvi_wins = sum(cv_imp_r2 > imp_cv_r2),
    total_comparisons = n(),
    cvi_wins_pct = tbl_string("{100*mean(cv_imp_r2 > imp_cv_r2)}%")
  )),
  
  r2_tune_minmax = make_r2_tune_minmax(sim_overall_r2_tune),
  
  # report compilation ----
  
  sim_article = target(
    command = {
      rmarkdown::render(knitr_in("doc/SIM_Article.Rmd"))
      file_out("doc/SIM_Article.pdf")
    }
  )
  
  
  
)
