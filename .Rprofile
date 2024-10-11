# Circumvent srcref issue https://github.com/rstudio/renv/issues/1713
options("install.opts" = "--without-keep.source")

source("renv/activate.R")
local({
  config_profile = Sys.getenv('R_CONFIG_ACTIVE', 'default')
  cli::cli_alert_info("Loading config {.val {config_profile}}")
})

conf <- config::get()

# Quick validation of some essentials
checkmate::assert_number(conf$task_dim_max, lower = 1)
checkmate::assert_subset(conf$tuning$tuner, choices = c("random_search", "mbo"))
num_threads <- checkmate::assert_integerish(conf$learner_threads, lower = 1)

options(
  mlr3oml.cache = TRUE,
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE
)

# Make parallelization behave
Sys.setenv(OMP_NUM_THREADS = num_threads)
Sys.setenv(OPENBLAS_NUM_THREADS = num_threads)
Sys.setenv(OMP_THREAD_LIMIT = num_threads)
Sys.setenv(MKL_NUM_THREADS = num_threads)
rm(num_threads)
