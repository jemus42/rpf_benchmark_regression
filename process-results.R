library(batchtools)
library(mlr3)
library(mlr3tuning)

if (!fs::dir_exists(conf$result_path)) {
  fs::dir_create(conf$result_path, recurse = TRUE)
}

path_bmr_reduced <- fs::path(conf$result_path, "bmr_reduced", ext = "rds")
path_archives <- fs::path(conf$result_path, "tuning_archives", ext = "rds")
path_results <- fs::path(conf$result_path, "tuning_results", ext = "rds")

if (!fs::file_exists(path_bmr_reduced)) {
  reg <- loadRegistry(conf$reg_dir, writeable = FALSE)
  tab <- ljoin(unwrap(getJobTable()), readRDS("task_meta.rds"), by = "task_id")
  data.table::setkey(tab, job.id)

  cli::cli_h2("Processing registry")
  getStatus()
  ids <- findDone()

  tictoc::tic(msg = "Reducing results")
  # Disabling the progress bar for speedup with many jobs
  options(batchtools.progress = FALSE)
  bmr <- mlr3batchmark::reduceResultsBatchmark(ids, store_backends = FALSE)
  options(batchtools.progress = TRUE)
  tictoc::toc()

} else {
  cli::cli_alert_info("File {.file {fs::path_rel(path_bmr_reduced)}} already exists!")
}

# Score results -------------------------------------------------------------------------------
cli::cli_h2("Scoring and aggregating")

measures <- list(
  msr("regr.rmse", id = "rmse"),
  msr("regr.mae", id = "mae"),
  msr("regr.rmsle", id = "rmsle")
)

tictoc::tic(msg = "Scoring results")
scores <- bmr$score(measures, conditions = TRUE)
tictoc::toc()

tictoc::tic(msg = "Aggregating results")
aggr <- bmr$aggregate(measures, conditions = TRUE)
tictoc::toc()

tictoc::tic(msg = "Saving to disk: scores, aggr")
saveRDS(scores, fs::path(conf$result_path, "scores", ext = "rds"))
saveRDS(aggr, fs::path(conf$result_path, "aggr", ext = "rds"))
tictoc::toc()

# Extract tuning archives ---------------------------------------------------------------------
cli::cli_h2("Extracting tuning archives and results")

if (!fs::file_exists(path_bmr_reduced)) {

  tictoc::tic("Extracting and saving tuning archives")
  tuning_archives <- extract_inner_tuning_archives(bmr, unnest = NULL)
  saveRDS(archives, path_archives)
  tictoc::toc()

  tictoc::tic("Extracting and saving tuning results")
  tuning_results <- extract_inner_tuning_results(bmr)
  saveRDS(archives, path_results)
  tictoc::toc()

  cli::cli_alert_info("Discarding models from bmr")
  bmr$discard(models = TRUE)

  tictoc::tic("Saving reduced bmr")
  saveRDS(bmr, path_bmr_reduced)
  tictoc::toc()

} else {
  cli::cli_alert_info("Reduced bmr already exists! Reading from disk.")
  bmr <- readRDS(path_bmr_reduced)
}

# tictoc::tic(msg = "Saving full bmr")
# saveRDS(bmr, fs::path(conf$result_path, "bmr", ext = "rds"))
# tictoc::toc()

cli::cli_alert_success("Done!")
