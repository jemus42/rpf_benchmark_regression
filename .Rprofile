# Circumvent srcref issue https://github.com/rstudio/renv/issues/1713
options("install.opts" = "--without-keep.source")

source("renv/activate.R")
local({
  config_profile = Sys.getenv('R_CONFIG_ACTIVE', 'default')
  cli::cli_alert_info("Loading config {.val {config_profile}}")
})

conf <- config::get()

options(
  mlr3oml.cache = TRUE,
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE
)

# Make parallelization behave
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)
Sys.setenv(MKL_NUM_THREADS = 1)
