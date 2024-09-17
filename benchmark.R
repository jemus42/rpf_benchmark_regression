library(mlr3verse)
library(mlr3extralearners)
library(batchtools)

source("learners.R")
source("tasks.R")

design <- benchmark_grid(
  tasks = tasks,
  resamplings = resamplings,
  learners = learners,
  paired = TRUE # To use previously instantiated resamplings
)

if (!fs::dir_exists(fs::path_dir(conf$reg_dir))) {
  fs::dir_create(fs::path_dir(conf$reg_dir), recurse = TRUE)
}

if (fs::dir_exists(conf$reg_dir)) {
  if (config::is_active("production")) {
    cli::cli_abort("Refusing to delete existing registry {.file {fs::path_rel(conf$reg_dir)}} in production mode")
  } else {
    cli::cli_alert_warning("Deleting registry at {.file {fs::path_rel(conf$reg_dir)}}")
    fs::dir_delete(conf$reg_dir)
  }
}

reg <- makeExperimentRegistry(
  file.dir = conf$reg_dir,
  work.dir = here::here(),
  seed = conf$seed
)

# store_models = TRUE required to retain tuning instances in AutoTuners
mlr3batchmark::batchmark(design, store_models = TRUE)
tab <- ljoin(unwrap(getJobTable()), task_meta, by = "task_id")
data.table::setkey(tab, job.id)
