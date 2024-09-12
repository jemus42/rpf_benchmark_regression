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
tab <- ljoin(unwrap(getJobTable()), task_meta, by = "task_id")
data.table::setkey(tab, job.id)

sample_ids = tab[dim_rank <= 5, .SD[sample(nrow(.SD), 1)], by = c("task_id", "learner_id")]
# sample_ids

submitJobs(sample_ids)
waitForJobs()

cli::cli_alert_info("Reducing results")
bmr <- mlr3batchmark::reduceResultsBatchmark(findDone(), store_backends = FALSE)

measures <- list(
  msr("regr.rmse", id = "rmse"),
  msr("regr.mae", id = "mae"),
  msr("regr.rmsle", id = "rmsle")
)

cli::cli_alert_info("Scoring results")
scores <- bmr$score(measures, conditions = TRUE)
cli::cli_alert_info("Aggregating results")
aggr <- bmr$aggregate(measures, conditions = TRUE)


if (!fs::dir_exists(fs::path_dir(conf$result_path))) {
  fs::dir_create(fs::path_dir(conf$result_path))
}

if (!fs::dir_exists(conf$result_path)) {
  fs::dir_create(conf$result_path)
}

cli::cli_alert_info("Saving results")
saveRDS(bmr, fs::path(conf$result_path, "bmr", ext = "rds"))
saveRDS(scores, fs::path(conf$result_path, "scores", ext = "rds"))
saveRDS(aggr, fs::path(conf$result_path, "aggr", ext = "rds"))
