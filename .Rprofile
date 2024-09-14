source("renv/activate.R")
config_profile = Sys.getenv('R_CONFIG_ACTIVE', 'default')
cli::cli_alert_info("Loading config {.val {config_profile}}")
conf <- config::get()

options(
  mlr3oml.cache = TRUE,
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE
)
