library(batchtools)
library(mlr3)
library(mlr3tuning)
library(data.table)

if (!fs::dir_exists(conf$result_path)) {
  fs::dir_create(conf$result_path, recurse = TRUE)
}

save_obj <- function(obj, name, postfix = "") {
  path <- fs::path(conf$result_path, glue::glue("{name}_{postfix}"), ext = "rds")
  cli::cli_alert_info("Saving {.val {deparse(substitute(obj))}} to {.file {fs::path_rel(path)}}")
  saveRDS(obj, path)
}

reg <- loadRegistry(conf$reg_dir, writeable = FALSE)
task_meta <- readRDS("task_meta.rds")
tab <- ljoin(unwrap(getJobTable()), task_meta, by = "task_id")
data.table::setkey(tab, job.id)
save_obj(tab, name = "jobs")

cli::cli_h2("Processing registry")
print(getStatus())
ids <- findDone()
# ids = tab[, .SD[sample(nrow(.SD), 5)], by = c("problem", "algorithm", "tags")]

tictoc::tic(msg = "Reducing results")
# Disabling the progress bar for speedup with many jobs
options(batchtools.progress = FALSE)
bmr <- mlr3batchmark::reduceResultsBatchmark(ids, store_backends = FALSE)
options(batchtools.progress = TRUE)
tictoc::toc()

# Score results -------------------------------------------------------------------------------
cli::cli_h2("Scoring and aggregating")

measures <- list(
  msr("regr.rmse", id = "rmse"),
  msr("regr.mae", id = "mae"),
  # https://mlr3measures.mlr-org.com/reference/rrse.html
  msr("regr.rrse", id = "rrse")
)

tictoc::tic(msg = "Scoring results")
scores <- bmr$score(measures, conditions = TRUE)
scores <- as.data.table(scores)
scores[, task := NULL]
scores[, learner := NULL]
scores[, resampling := NULL]
scores[, resampling_id := NULL]
scores[, uhash := NULL]

score_errors <- sapply(scores$errors, \(x) length(x) > 0, simplify = TRUE) |> sum()
score_warnings <- sapply(scores$warnings, \(x) length(x) > 0, simplify = TRUE) |> sum()

if (score_errors == 0) scores[, errors := NULL]
if (score_warnings == 0) scores[, warnings := NULL]
tictoc::toc()

tictoc::tic(msg = "Aggregating results")
aggr <- bmr$aggregate(measures, conditions = TRUE)
aggr <- as.data.table(aggr)
aggr[, resample_result := NULL]
aggr[, resampling_id := NULL]

if (sum(aggr[, errors]) == 0) aggr[, errors := NULL]
if (sum(aggr[, warnings]) == 0) aggr[, warnings := NULL]
tictoc::toc()

tictoc::tic(msg = "Saving to disk: scores, aggr")
save_obj(scores, name = "scores")
save_obj(aggr, name = "aggr")
tictoc::toc()

# Extract tuning archives ---------------------------------------------------------------------
cli::cli_h2("Extracting tuning archives and results")

tictoc::tic("Extracting and saving tuning archives")
tuning_archives <- extract_inner_tuning_archives(bmr, unnest = NULL)

archives_rpf <- tuning_archives[
  startsWith(learner_id, "rpf"),
 .(learner_id, experiment, task_id, iteration, splits, split_try, t_try, max_interaction_ratio, regr.mse,
   runtime_learners, timestamp, warnings, errors, batch_nr)]

archives_rpf <- merge(archives_rpf, task_meta, by = "task_id")
archives_rpf[, rmse := sqrt(regr.mse)]
archives_rpf[, max_interaction := fifelse(learner_id == "rpf", pmax(ceiling(max_interaction_ratio * pmin(p, 20)), 1), 2)]

archives_xgb <- tuning_archives[
  startsWith(learner_id, "xgb"),
  .(learner_id, experiment, task_id, iteration, regr.xgboost.max_depth, regr.xgboost.subsample,
    regr.xgboost.colsample_bytree, regr.xgboost.eta, regr.xgboost.nrounds, regr.mse,
    runtime_learners, timestamp, warnings, errors, batch_nr)]
archives_xgb[, regr.xgboost.eta := exp(regr.xgboost.eta)] # due to tuning in logscale

names_to_trim <- stringr::str_subset(names(archives_xgb), "xgb")
data.table::setnames(archives_xgb,
                     old = names_to_trim,
                     new = stringr::str_remove(names_to_trim, "^regr\\.xgboost\\."))

archives_ranger <- tuning_archives[
  learner_id == "ranger",
  .(learner_id, experiment, task_id, iteration, mtry.ratio, min.node.size, sample.fraction,
    regr.mse, runtime_learners, timestamp, warnings, errors, batch_nr)]

save_obj(archives_rpf,    name = "archives", postfix = "rpf")
save_obj(archives_xgb,    name = "archives", postfix = "xgb")
save_obj(archives_ranger, name = "archives", postfix = "ranger")
tictoc::toc()

tictoc::tic("Extracting and saving tuning results")
tuning_results <- extract_inner_tuning_results(bmr)

results_rpf <- tuning_results[
  startsWith(learner_id, "rpf"),
  .(learner_id, experiment, task_id, iteration, splits, split_try, t_try, max_interaction_ratio, regr.mse)]

results_rpf <- merge(results_rpf, task_meta, by = "task_id")
results_rpf[, rmse := sqrt(regr.mse)]
results_rpf[, max_interaction := fifelse(learner_id == "rpf", pmax(ceiling(max_interaction_ratio * pmin(p, 20)), 1), 2)]

results_xgb <- tuning_results[
  startsWith(learner_id, "xgb"),
  .(learner_id, experiment, task_id, iteration, regr.xgboost.max_depth, regr.xgboost.subsample,
    regr.xgboost.colsample_bytree, regr.xgboost.eta, regr.xgboost.nrounds, regr.mse)]
results_xgb[, regr.xgboost.eta := exp(regr.xgboost.eta)] # due to tuning in logscale

names_to_trim <- stringr::str_subset(names(results_xgb), "xgb")
data.table::setnames(
  results_xgb,
  old = names_to_trim,
  new = stringr::str_remove(names_to_trim, "^regr\\.xgboost\\.")
)

results_ranger <- tuning_results[
  learner_id == "ranger",
  .(learner_id, experiment, task_id, iteration, mtry.ratio, min.node.size, sample.fraction, regr.mse)]

save_obj(archives_rpf,    name = "results", postfix = "rpf")
save_obj(archives_xgb,    name = "results", postfix = "xgb")
save_obj(archives_ranger, name = "results", postfix = "ranger")
tictoc::toc()

cli::cli_alert_success("Done!")
