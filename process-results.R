library(batchtools)
library(mlr3)
library(mlr3tuning)
library(data.table)

if (!fs::dir_exists(conf$result_path)) {
  fs::dir_create(conf$result_path, recurse = TRUE)
}

save_obj <- function(obj, name, postfix = NULL) {
  if (!is.null(postfix)) name <- paste0(name, "_", postfix)
  path <- fs::path(conf$result_path, name, ext = "rds")
  cli::cli_alert_info("Saving {.val {deparse(substitute(obj))}} to {.file {fs::path_rel(path)}}")
  saveRDS(obj, path)
}

measures <- list(
  msr("regr.rmse", id = "rmse"),
  msr("regr.mae", id = "mae"),
  # https://mlr3measures.mlr-org.com/reference/rrse.html
  msr("regr.rrse", id = "rrse")
)

tictoc::tic("Full result processing")
reg <- loadRegistry(conf$reg_dir, writeable = FALSE)
task_meta <- readRDS("task_meta.rds")
tab <- ljoin(unwrap(getJobTable()), task_meta, by = "task_id")
data.table::setkey(tab, job.id)
save_obj(tab, name = "jobs")

cli::cli_h1("Processing registry")
print(getStatus())


learners = unique(tab$learner_id)



for (learner in learners) {
  cli::cli_h2("Processing results for {.val {learner}}")

  if (learner == "featureless") {
    result_types = c("scores", "aggr")
  } else {
    result_types = c("scores", "aggr", "archives", "results")
  }

  learner_output_files = fs::path(
    conf$result_path,
    paste0(result_types, "_", learner),
    ext = "rds"
  )

  if (all(fs::file_exists(learner_output_files))) {
    cli::cli_alert_info("Output files for {.val {learner}} already exist, skipping")
    next
  }

  ids_all = tab[learner_id == learner]
  ids = ijoin(findDone(), ids_all)

  cli::cli_inform("Found {.val {nrow(ids)}} / {.val {nrow(ids_all)}} completed jobs")

  tictoc::tic(msg = "Reducing results")
  # Disabling the progress bar for speedup with many jobs
  options(batchtools.progress = FALSE)
  bmr <- mlr3batchmark::reduceResultsBatchmark(ids, store_backends = FALSE)
  options(batchtools.progress = TRUE)
  tictoc::toc()


  cli::cli_h3("Scoring and aggregating")


  tictoc::tic(msg = "Scoring results")
  scores <- bmr$score(measures, conditions = TRUE)
  scores <- as.data.table(scores)
  scores[, task := NULL]
  scores[, learner := NULL]
  scores[, resampling := NULL]
  scores[, resampling_id := NULL]
  scores[, uhash := NULL]

  scores[, errors := sapply(errors, \(x) length(x), simplify = TRUE)]
  scores[, warnings := sapply(warnings, \(x) length(x) > 0, simplify = TRUE)]

  tictoc::toc()

  tictoc::tic(msg = "Aggregating results")
  aggr <- bmr$aggregate(measures, conditions = TRUE)
  aggr <- as.data.table(aggr)
  aggr[, resample_result := NULL]
  aggr[, resampling_id := NULL]
  tictoc::toc()

  tictoc::tic(msg = "Saving to disk: scores, aggr")
  save_obj(scores, name = "scores", postfix = learner)
  save_obj(aggr, name = "aggr", postfix = learner)
  tictoc::toc()

  if (learner == "featureless") next

  cli::cli_h3("Extracting tuning archives and results")

  tictoc::tic("Extracting and saving tuning archives")
  tuning_archives <- extract_inner_tuning_archives(bmr, unnest = NULL)

  if (startsWith(learner, "rpf")) {

    archives <- tuning_archives |>
      dplyr::filter(startsWith(learner_id, "rpf")) |>
      dplyr::select(tidyselect::any_of(
        c("learner_id", "experiment", "task_id", "iteration", "splits", "split_try", "t_try", "max_interaction_ratio",
          "regr.mse", "runtime_learners", "timestamp", "warnings", "errors", "batch_nr")
      ))

    archives <- merge(archives, task_meta, by = "task_id")
    archives[, rmse := sqrt(regr.mse)]

    if (learner == "rpf_fixdepth") {
      archives[, max_interaction := 2]
    } else {
      archives[, max_interaction := pmax(ceiling(max_interaction_ratio * pmin(p, 20)), 1)]
      archives[, max_interaction_ratio := NULL]
    }

  } else if (startsWith(learner, "xgb")) {

    archives <- tuning_archives |>
      dplyr::filter(startsWith(learner_id, "xgb")) |>
      dplyr::select(tidyselect::any_of(
        c("learner_id", "experiment", "task_id", "iteration", "regr.xgboost.max_depth", "regr.xgboost.subsample",
          "regr.xgboost.colsample_bytree", "regr.xgboost.eta", "regr.xgboost.nrounds", "regr.mse",
          "runtime_learners", "timestamp", "warnings", "errors", "batch_nr")
      ))

    names_to_trim <- stringr::str_subset(names(archives), "xgb")
    data.table::setnames(archives,
                         old = names_to_trim,
                         new = stringr::str_remove(names_to_trim, "^regr\\.xgboost\\."))

    archives <- merge(archives, task_meta, by = "task_id")
    archives[, rmse := sqrt(regr.mse)]

    if (learner == "xgb_fixdepth") {
      archives[, max_depth := 2]
    }

  } else if (learner == "ranger") {
    archives <- tuning_archives[
      learner_id == "ranger",
      .(learner_id, experiment, task_id, iteration, mtry.ratio, min.node.size, sample.fraction,
        regr.mse, runtime_learners, timestamp, warnings, errors, batch_nr)]

    archives <- merge(archives, task_meta, by = "task_id")
    archives[, rmse := sqrt(regr.mse)]
    archives[, mtry := ceiling(mtry.ratio * p)]
    archives[, mtry.ratio := NULL]
  }

  save_obj(archives, name = "archives", postfix = learner)
  tictoc::toc()

  tictoc::tic("Extracting and saving tuning results")
  tuning_results <- extract_inner_tuning_results(bmr)

  if (startsWith(learner, "rpf")) {
    results <- tuning_results |>
      dplyr::filter(startsWith(learner_id, "rpf")) |>
      dplyr::select(tidyselect::any_of(
        c("learner_id", "experiment", "task_id", "iteration", "splits", "split_try", "t_try",
          "max_interaction_ratio", "regr.mse")
      ))

    results <- merge(results, task_meta, by = "task_id")
    results[, rmse := sqrt(regr.mse)]
    if (learner == "rpf_fixdepth") {
      results[, max_interaction := 2]
    } else {
      results[, max_interaction := pmax(ceiling(max_interaction_ratio * pmin(p, 20)), 1)]
      results[, max_interaction_ratio := NULL]
    }

  } else if (startsWith(learner, "xgb")) {
    results <- tuning_results |>
      dplyr::filter(startsWith(learner_id, "xgb")) |>
      dplyr::select(tidyselect::any_of(
        c("learner_id", "experiment", "task_id", "iteration", "regr.xgboost.max_depth", "regr.xgboost.subsample",
          "regr.xgboost.colsample_bytree", "regr.xgboost.eta", "regr.xgboost.nrounds", "regr.mse")
      ))

    results[, regr.xgboost.eta := exp(regr.xgboost.eta)] # due to tuning in logscale

    names_to_trim <- stringr::str_subset(names(results), "xgb")
    data.table::setnames(
      results,
      old = names_to_trim,
      new = stringr::str_remove(names_to_trim, "^regr\\.xgboost\\.")
    )

    results <- merge(results, task_meta, by = "task_id")
    results[, rmse := sqrt(regr.mse)]
    if (learner == "xgb_fixdepth") results[, max_depth := 2]

  } else if (learner == "ranger") {
    results <- tuning_results[
      learner_id == "ranger",
      .(learner_id, experiment, task_id, iteration, mtry.ratio, min.node.size, sample.fraction, regr.mse)]
    results <- merge(results, task_meta, by = "task_id")
    results[, rmse := sqrt(regr.mse)]
    results[, mtry := ceiling(mtry.ratio * p)]
    results[, mtry.ratio := NULL]
  }

  save_obj(results, name = "results", postfix = learner)
  tictoc::toc()

  rm(bmr)

  tictoc::tic("Collecting garbage just in case")
  gc(full = TRUE)
  tictoc::toc()
}

cli::cli_h2("Combining results")

tictoc::tic("Scores")
fs::dir_ls(conf$result_path, glob = "*/scores_*.rds") |>
  lapply(readRDS) |>
  data.table::rbindlist() |>
  save_obj(name = "scores")
tictoc::toc()

tictoc::tic("Aggrs")
fs::dir_ls(conf$result_path, glob = "*/aggr_*.rds") |>
  lapply(readRDS) |>
  data.table::rbindlist() |>
  save_obj(name = "aggr")
tictoc::toc()

cli::cli_alert_success("Done!")
tictoc::toc()
