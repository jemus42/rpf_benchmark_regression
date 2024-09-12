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
  fs::dir_create(fs::path_dir(conf$reg_dir))
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

mlr3batchmark::batchmark(design, store_models = FALSE)
tab <- merge(unwrap(getJobTable()), task_meta, by = "task_id")

sample_ids = tab[dim_rank <= 10, .SD[sample(nrow(.SD), 1)], by = c("task_id", "learner_id")]
sample_ids

submitJobs(sample_ids)

