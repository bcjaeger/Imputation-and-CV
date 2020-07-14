
library(rslurm)

main <- function(
  seed  = 731,                # random seed for reproducibility
  nx    = 10,                 # total number of x (signal) variables
  nz    = 40,                 # total number of z (noise) variables
  nobs  = 100,                # total number of obs in data
  ngrps = 1,                  # total number of groups in the data.
  # ------------------------- # each group has different mean values for x
  rmean = TRUE,               # if TRUE, then the specific means of groups
  # ------------------------- # are generated at random. If FALSE, means are
  # ------------------------- # generated from -1 to 1, systematically
  latent = FALSE,             # if FALSE, the groups are aligned with the
  # ------------------------- # folds. if TRUE, then they are not aligned.
  # ------------------------- # this is only relevant if ngrps > 1.
  n_val = 5000,               # total number of obs in external data
  nfold = 10,                 # number of folds for CV
  x_cor = 3/4,                # correlation of x variables
  alpha = 0.90,               # for glmnet models
  k_nbr = seq(35),            # nearest neighbors
  miss_prop = 0.90,           # proportion of missing data (by cases)
  miss_mech = 'mcar'          # missing data mechanism (mar/mcar/mnar)
){
  
  library(glue)       # used to handle characters
  library(rsample)    # used for cross-validation
  library(glmnet)     # used for penalized regression
  library(magrittr)   # loaded for %<>% and use_series
  library(tidyverse)  # used for handling data/lists
  library(mice)       # used to ampute simulated data
  library(ipa)        # used for multiple imputes with knn
  library(mvtnorm)    # to simulate data
  library(data.table) # prevent crashing?
  
  # run devtools::install_github('bcjaeger/ipa') if needed
  
  # some functions used to help make inputs for mice::ampute
  
  set_upper_tri_to_zero <- function(mtx){
    mtx[upper.tri(mtx)] <- 0
    mtx
  }
  
  set_n_to_zero <- function(mtx, n){
    for(i in 1:ncol(mtx)) mtx[sample(nrow(mtx), n), i] <- 0
    mtx
  }
  
  set_col_to_1 <- function(mtx, col_index){
    mtx[, col_index] <- 1
    mtx
  }
  
  drop_1s <- function(mtx, by = 'row'){
    
    margin = if (by == 'row') 1 else if (by == 'col') 2
    
    drop_indx <- apply(mtx, margin, function(x) all(x==1))
    
    if(any(drop_indx)){
      
      drop_indx <- which(drop_indx)
      
      if (by == 'row') mtx <- mtx[ -drop_indx, ]
      if (by == 'col') mtx <- mtx[ , -drop_indx]
      
    } 
    
    mtx
    
  }
  
  # Make all the data ----
  
  set.seed(seed)
  ncov <- nx + nz
  # Covariance of x and z values
  Sigma <- matrix(data = 0, nrow = ncov, ncol = ncov)
  
  # Impose an autoregressive covariance structure
  for (i in 1:ncov) {
    for (j in 1:ncov) {
      
      expo <- abs(i-j)
      
      Sigma[i, j] = x_cor^expo
      
    }
  }
  
  diag(Sigma) <- 1
  
  # group means
  
  if(ngrps > 1){
    
    # a vector that indicates group membership
    grp_vec <- rep(1:ngrps, each = ceiling(nobs / ngrps))
    # correct length if needed
    grp_vec <- grp_vec[1:nobs]
    # a vector to show how big each group is
    grp_sizes <- map_int(seq(ngrps), ~sum(grp_vec==.x))
    
    # the mean values of x and z for each group
    # generated at random if rmean is TRUE
    if(rmean){
      means <- rnorm(n = ngrps, sd = 1)
    } else {
      means <- seq(-1, 1, length.out = ngrps)
    }
    
    # initialize x and z
    z <- x <- vector(mode = 'list', length = ngrps)
    
    for(i in seq(ngrps)){
      
      .means <- rep(means[i], ncov)
      
      xz <- rmvnorm(n = grp_sizes[i], mean = .means, sigma = Sigma)
      
      x[[i]] <- xz[, 1:nx]
      z[[i]] <- xz[, -c(1:nx)]
      
    }
    
    x <- reduce(x, rbind)
    z <- reduce(z, rbind)
    
    # We're also going to make an external data set with missing data.
    # This will be used later, but it needs to be imputed first.
    # the external data are generated with a mean value outside 
    # of the internal data
    
    if(rmean){
      .means <- rep(rnorm(n=1), ncov)
    } else {
      .means <- rep(means[ngrps] + 1/4, ncov)
    }
    
    xz_val <- rmvnorm(n = n_val, mean = .means, sigma = Sigma)
    
    x_val <- xz_val[, 1:nx]
    z_val <- xz_val[, -c(1:nx)]
    
  } else {
    
    # just one group
    grp_vec <- rep(1, nobs)
    grp_sizes <- nobs
    
    
    xz <- rmvnorm(n = nobs, sigma = Sigma)
    # x matrix has signal,
    x <- xz[, 1:nx]
    # z matrix has noise
    z <- xz[, -c(1:nx)]
    
    # We're also going to make an external data set with missing data.
    # This will be used later, but it needs to be imputed first.
    
    xz_val <- rmvnorm(n = n_val, sigma = Sigma)
    # x matrix has signal,
    x_val <- xz_val[, 1:nx]
    # z matrix has noise
    z_val <- xz_val[, -c(1:nx)]
    
    
  }
  
  colnames(x) <- glue("x{1:nx}")
  colnames(z) <- glue("z{1:nz}")
  
  # regression coefficients
  beta <- seq(-1, 1, length.out = nx)
  
  # outcome value
  y <- x %*% beta + rnorm(n = nobs)
  
  # simulated data (no missing valus yet)
  sim <- bind_cols(y = as.numeric(y), as_tibble(x), as_tibble(z))
  
  colnames(x_val) <- glue("x{1:nx}")
  colnames(z_val) <- glue("z{1:nz}")
  
  y_val <- x_val %*% beta + rnorm(n = n_val)
  
  # simulated external data (no missing valus yet)
  val <- bind_cols(y = as.numeric(y_val), as_tibble(x_val), as_tibble(z_val))
  
  # sanity checks: 
  ## 1 ## plot of y as function of x variables
  
  # sim %>%
  #   select(y, starts_with('x')) %>%
  #   pivot_longer(cols = -y) %>%
  #   ggplot(aes(x=value, y=y, col = name)) +
  #   geom_smooth(method = 'lm', se=FALSE) +
  #   theme_bw() +
  #   coord_cartesian(ylim = c(-2,2))
  
  ## 2 ## plot y as a function of z variables
  
  # sim %>%
  #   select(y, starts_with('z')) %>%
  #   pivot_longer(cols = -y) %>%
  #   ggplot(aes(x=value, y=y, col = name)) +
  #   geom_smooth(method = 'lm', se=FALSE) +
  #   theme_bw() +
  #   coord_cartesian(ylim = c(-2,2))
  
  # Ampute the data ----
  
  
  # this may need to run a couple times before a good
  # random missing pattern is generated. 
  ampute_error = TRUE
  attempts <- 0
  
  # Make an empty pattern matrix to start the while loop.
  pattern = matrix(data = integer(), ncol = ncov)
  
  while(ampute_error){
    
    print(paste('attempting to ampute data, attempt', attempts))
    
    # Make an empty pattern matrix to start the while loop.
    pattern = matrix(data = integer(), ncol = ncov)
    
    while( nrow(pattern) < 1 ){
      
      pattern <- ampute.default.patterns(n = ncol(sim)) %>% 
        set_n_to_zero(n = round(ncov/2)) %>% 
        #set_upper_tri_to_zero() %>% 
        set_col_to_1(col_index = c(1, grep('z', names(sim)))) %>% 
        drop_1s(by = 'row')
      
    }
    
    
    # wts <- ampute.default.weights(pattern, miss_mech)
    # wts[, grep('z', names(sim))] <- 0
    
    dat <- try(expr = ampute(sim, 
      prop = miss_prop, 
      patterns = pattern,
      # weights = wts,
      std = FALSE, # prevents error if nrow(score) == 1
      freq = ampute.default.freq(pattern),
      mech = toupper(miss_mech),
      bycases = TRUE
    ) %>% 
        use_series('amp') %>% 
        as_tibble(),
      silent = TRUE
    )
    
    
    val <- try(expr = ampute(val, 
      prop = miss_prop, 
      patterns = pattern,
      # weights = wts,
      std = FALSE, 
      freq = ampute.default.freq(pattern),
      mech = toupper(miss_mech),
      bycases = TRUE
    ) %>% 
        use_series('amp') %>% 
        as_tibble(),
      silent = TRUE
    )
    
    if(class(val)[1] != 'try-error' & class(dat)[1] != 'try-error'){
      ampute_error = FALSE
    }
    
    attempts <- attempts + 1
    
  }
  
  # Make Cross-validation folds ----
  
  # if we have latent groups, it is because we are trying to gauge
  # how much of an advantage true CV may have when the validation
  # data comes from a different X population than the training data.
  # CV with imputation embedded has an advantage over imputation
  # done before CV in this scenario 
  if(ngrps > 1){
    
    nfold <- ngrps
    cv_imp <- vfold_cv(data = dat, v = nfold)
    
    if(!latent){
      
      foldid <- grp_vec
      
      for(i in 1:nfold){
        cv_imp$splits[[i]]$in_id <- which(foldid != i)
      }
      
    }
    
    if(latent){
      
      # initialize the vector
      foldid <- vector(mode='integer', length = nobs)
      
      # get fold information from cv_imp
      folds <- purrr::map(cv_imp$splits, complement)
      
      # folds is a list of nfold things, each thing has numbers
      # indicating rows of data that belong to this fold. 
      # set foldid = fold rows for each thing in folds
      for (i in seq_along(cv_imp$splits)) {foldid[folds[[i]]] <- i}
      
    }
    
  } else {
    
    cv_imp <- vfold_cv(data = dat, v = nfold)
    
    # notably, when we use CV to internally validate models, they need to
    # use the exact same folds as cv_imp has. We save those folds in the
    # foldid vector, which is passed into cv.glmnet when we internally
    # validate models.
    
    # initialize the vector
    foldid <- vector(mode='integer', length = nobs)
    
    # get fold information from cv_imp
    folds <- purrr::map(cv_imp$splits, complement)
    
    # folds is a list of nfold things, each thing has numbers
    # indicating rows of data that belong to this fold. 
    # set foldid = fold rows for each thing in folds
    for (i in seq_along(cv_imp$splits)) {foldid[folds[[i]]] <- i}
    
    # sanity check: all values of foldid should be 1 for 
    # the rows listed in folds[[1]]
    print(folds[[1]])
    print(foldid[folds[[1]]])
    
  }
  
  # object to keep data on how long things take to do...
  
  compute_times <- list()
  
  # Impute the data ----
  
  # this is the easy way - but possibly wrong (!): impute first, and 
  # cross-validate a model using the imputed data. This may be okay
  # because the ipa package uses unsupervised imputation - i.e., 
  # it does not use the outcome data to impute missing values. This
  # should theoretically prevent cross-validation from losing its
  # integrity.
  
  max_neighbors <- min(
    purrr::map_int(dat, ~sum(stats::complete.cases(.x)))
  )
  
  k_nbr_trunc <- k_nbr[k_nbr <= max_neighbors]
  
  start <- Sys.time()
  imp_cv <- dat %>%
    brew_nbrs(outcome = y) %>%
    verbose_on(level = 1) %>%
    spice(with = spicer_nbrs(k_neighbors = k_nbr_trunc)) %>%
    mash() %>% 
    stir()
  stop <- Sys.time()
  
  imp_cv <- imp_cv %>% 
    ferment(data_new = val) %>% 
    bottle(type = 'matrix')
  
  compute_times$make_imp_cv <- as.numeric(stop-start, units = 'secs')
  
  # the hope is that we can interpret the results from this procedure
  # in the same way that we would interpret them if we were to run
  # imputation during each replicate of cross-validation. That is, 
  # we interpret the cross-validated model errors to be a consistent
  # estimate of generalization error if we were to apply the 
  # given imputation + modeling strategy to the entire training set,
  # and then externally validate the resulting prediction model in 
  # a large validation set. 
  
  # this is the hard, but correct way: run cross-validation and 
  # apply imputation in each replicate to the training/testing
  # folds, separately. If there is a quicker way to do this without
  # sacrificing integrity of the model's internal error estimate, 
  # analyses would be able to look at more strategies to handle
  # missing data and ultimately pick better ones.
  
  
  start <- Sys.time()
  cv_imp <- cv_imp %>% 
    as_tibble() %>% 
    mutate(
      imputes = map(splits, .f = ~ {
        bounds <- ipa:::get_par_bounds(training(.x), flavor = 'kneighbors')
        k_nbr_tmp <- k_nbr[k_nbr <= bounds$neighbors$max]
        training(.x) %>% 
          brew_nbrs(outcome = y) %>% 
          verbose_on(level = 1) %>% 
          spice(with = spicer_nbrs(k_neighbors = k_nbr_tmp)) %>% 
          mash() %>% 
          stir() %>% 
          ferment(data_new = testing(.x)) %>% 
          bottle(type = 'matrix')
      })
    )
  stop <- Sys.time()
  
  compute_times$make_cv_imp <- as.numeric(stop-start, units = 'secs')
  
  print(cv_imp)
  
  # note how each impute looks a lot like imp_cv, but contains another
  # column called 'testing' which contains imputed test data.
  print(cv_imp$imputes[[1]])
  
  # Q1: Does cross-validation work with no missing data? ----
  
  # I admit this is really just a sanity check and isn't that
  # interesting of a question, but it still is worth checking.
  
  cmp <- cv.glmnet(
    x = as.matrix(select(sim, -y)),
    y = as.matrix(select(sim, y)),
    foldid = foldid,
    alpha = alpha,
    keep = TRUE
  )
  
  # find the index (ix) of the 'best' lambda value
  ix <- which(cmp$lambda == cmp$lambda.min)
  # get internal predictions at this index (i.e., predict using lambda.min)
  prd <- cmp$fit.preval[, ix]
  # see if those predictions give you the expected rmse of 1
  rmse <- sqrt(mean((prd - y)^2))
  # they basically do.
  print(rmse)
  
  # A1: Yes, cross-validation works when there is no missing data.
  # more importantly, it appears the simulation is good enough to 
  # show that things we know to be true are true. 
  
  # Q2: What is estimated error when we impute during cross validation? ----
  
  # need to be careful here, because we will be fitting models to each
  # of many different imputed sets, each one being based on a different
  # number of nearest neighbors. To make sure all models are fitted
  # in the same way, we make foldid_mini, which determines the nested
  # CV folds that all models will use. If we didn't do this, some 
  # values of k neighbors might appear to over or under perform 
  # because they got a particularly lucky or unlucky sample to 
  # run CV with.
  
  # foldid_mini should be the same size as one training fold
  size_mini <- ceiling((nfold-1) * nobs / nfold)
  # create the smaller foldid for nested CV
  foldid_mini <- sample(1:nfold, size = size_mini, replace = T)
  
  start <- Sys.time()
  cv_imp_error <- cv_imp %>% 
    mutate(imputes = map(imputes, 'wort')) %>% 
    unnest(col = imputes) %>% 
    mutate(
      mdl = map(training, .f = ~{
        cv.glmnet(
          x = .x$X,
          y = .x$Y,
          foldid = foldid_mini[1:nrow(.x$X)],
          alpha = alpha,
          keep = FALSE # don't need this b/c we validate on testing
        )
      })
    )
  stop <- Sys.time()
  
  compute_times$mdl_cv_imp <- as.numeric(stop-start, units = 'secs')
  
  cv_imp_error <- cv_imp_error %>% 
    mutate(
      prd = map2(mdl, testing, .f = ~ {
        as.numeric(predict(.x, newx = .y$X, s = 'lambda.min'))
      })
    )
  
  ref_rmse <- sqrt( mean((val$y - mean(val$y))^2) )
  
  cv_imp_error_vals <- cv_imp_error %>% 
    mutate(
      errors = map2(prd, testing, .f = ~ {
        as.numeric(.x - .y$Y)
      })
    ) %>% 
    unnest(errors) %>% 
    group_by(impute) %>%
    summarise(
      rmse = sqrt(mean(errors^2)),
      r2 = 1 - rmse / ref_rmse
    )
  
  # this plot shows a horizontal line at the error value
  # we got when we didn't have any missing data
  # ggplot(cv_imp_error_vals, aes(x=impute, y=rmse)) +
  #   geom_point() +
  #   geom_line() +
  #   geom_hline(yintercept = rmse, linetype = 2) +
  #   theme_classic()
  
  # A2: a well chosen value of K nearest neighbors gets you pretty close.
  # print out the best cross-validated error minus the error we got with
  # no missing data (this would be better if we scaled to R-squared vals)
  print(min(cv_imp_error_vals$rmse) - rmse)
  
  # Q3: How about when we take the shortcut? ----
  
  # all we need to do here is fit one cv.glmnet model over 
  # the original folds and keep its pre-validated predictions.
  # (this is done for each imputed dataset to match Q2)
  
  start <- Sys.time()
  imp_cv_error <- imp_cv$wort %>% 
    mutate(
      mdl = map(training, .f = ~{
        cv.glmnet(
          x = .x$X,
          y = .x$Y,
          foldid = foldid, # use the original folds
          alpha = alpha,
          keep = TRUE # we DO need this to use the same folds as cv_imp
        )
      }),
      prd = map(mdl, .f = ~ {
        ix <- which(.x$lambda == .x$lambda.min)
        as.numeric(.x$fit.preval[, ix])
      })
    )
  stop <- Sys.time()
  
  compute_times$mdl_imp_cv <- as.numeric(stop-start, units = 'secs')
  
  imp_cv_error_vals <- imp_cv_error %>% 
    unnest(prd) %>% 
    group_by(impute) %>% 
    mutate(errors = prd - y) %>% 
    summarise(
      rmse = sqrt(mean(errors^2)),
      r2 = 1 - rmse / ref_rmse
    )
  
  # Q4: How accurate are these errors for external testing data? ----
  # Usually we don't have a gigantic validation set to test our 
  # prediction equations, so it is ideal to have an accurate 
  # internal estimate. However, doing imputation first and CV
  # second might bias the internal error estimates by ignoring
  # the uncertainty of missing values in the testing data. Put 
  # another way, if we have missingness in training and testing data,
  # then imputing all of the data at the same time and then fitting
  # a model to some of the imputed data and testing it in the rest
  # of the imputed data is very different from the reality of
  # imputing all our training data, fitting a model to it, and then
  # imputing the external testing data and testing our model.
  
  ex_dat_error_vals <- imp_cv_error %>% 
    mutate(
      errors = map2(mdl, testing, .f = ~{
        as.numeric(.y$Y - predict(.x, newx = .y$X, s = 'lambda.min'))
      })
    ) %>% 
    select(impute, errors) %>% 
    unnest(cols = errors) %>% 
    group_by(impute) %>% 
    summarise(
      rmse = sqrt(mean((errors)^2)),
      r2 = 1 - rmse / ref_rmse
    )
  
  cp_data <- enframe(compute_times, name = 'action', value = 'time') %>% 
    mutate(time = map_dbl(time, as.numeric)) %>% 
    separate(action, into = c('action', 'first', 'second')) %>% 
    unite(col = 'cv_strat', first, second)
  
  results <- bind_rows(
    cv_imp = cv_imp_error_vals,
    imp_cv = imp_cv_error_vals,
    ex_dat = ex_dat_error_vals,
    .id = 'cv_strat'
  ) 
  
  # ggplot(results) +
  #   aes(x=impute, y=r2, col = cv_strat) +
  #   geom_line()
  
  # Final output preparation ----
  
  output <- tibble(
    compute_time = list(cp_data), 
    results      = list(results),
    seed         = seed, 
    nx           = nx,
    nz           = nz,
    nobs         = nobs,
    ngrps        = ngrps,
    rmean        = rmean,
    n_val        = n_val,
    nfold        = nfold,
    x_cor        = x_cor,
    alpha        = alpha,
    miss_prop    = miss_prop,
    miss_mech    = miss_mech
  )
  
  output
  
  
}

library(tidyverse)

start_iter <- 251
stop_iter <- 500
mem_per_cpu <- 30000

today <- Sys.time() %>% 
  as.Date() %>% 
  str_replace_all(pattern = '\\-', replacement = '_')

#grp <- 'noGroup'
grp <- 'wGroups'

latent <- FALSE

if(grp == 'noGroups') ngrps = 1 else if(grp == 'wGroups') ngrps = 10

if(latent & ngrps > 1) grp <- paste(grp, 'latent', sep = '_')

if(!latent & ngrps > 1) grp <- paste(grp, 'observed', sep = '_')

jobname <- as.character(glue::glue(
  "sim_x_{today}_x_{grp}_x_{start_iter}_{stop_iter}"
))

pars <- 
  expand.grid(
    seed   = seq(start_iter, stop_iter, by = 1),
    nobs   = c(100, 500, 1000, 5000),
    ncov   = c("10_10", "10_40", "10_490"),
    ngrps  = ngrps, 
    latent = latent,
    rmean  = TRUE,
    nfold  = 10,
    n_val  = 10000,
    x_cor  = 3/4, 
    alpha  = 0.9,
    miss_prop = 0.9,
    miss_mech = c('mcar', 'mar'),
    stringsAsFactors = FALSE
  ) %>% 
  separate(ncov, into = c('nx', 'nz'), sep = '_', convert = TRUE)

sopt <- 
  list(
    partition='short',
    time='11:59:00',
    share=TRUE,
    'mem-per-cpu'= mem_per_cpu
  )

slurm_apply(
  main,
  pars,
  jobname = jobname,
  slurm_options = sopt,
  nodes = nrow(pars),
  cpus_per_node = 1,
  submit = TRUE
)




