## Load your packages, e.g. library(drake).
source("./packages.R")

## Load R files
rfiles_all <- list.files("./R", full.names = TRUE)

dont_source <- 'sim_slurm\\.R$|sim_clean\\.R$|ames_slurm\\.R|ames_clean\\.R$'

rfiles_to_source <- rfiles_all[-grep(dont_source, x = rfiles_all)]

lapply(rfiles_to_source, source)

## _drake.R must end with a call to drake_config().
## The arguments to drake_config() are basically the same as those to make().
## lock_envir allows functions that alter the random seed to be used. The biggest
## culprits of this seem to be interactive graphics e.g. plotly and mapdeck.
drake_config(the_plan,
             lock_envir = FALSE,
             seed = 329,
             prework = options(scipen = 9999999))
