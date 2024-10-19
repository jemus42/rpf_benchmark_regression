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
  tuning_archives <- try(extract_inner_tuning_archives(bmr, unnest = NULL))
  if (inherits(tuning_archives, "try-error")) {
    cli::cli_alert_danger("Failed to extract tuning archives for {.val {learner}}, skipping")
    next
  }
  tuning_archives <- merge(tuning_archives, task_meta, by = "task_id")
  tuning_archives[, rmse := sqrt(regr.mse)]
  tuning_archives[, x_domain := NULL]
  tuning_archives[, resampling_id := NULL]

  if (learner == "rpf") {
    tuning_archives[, max_interaction := pmax(ceiling(max_interaction_ratio * pmin(p, 20)), 1)]
    tuning_archives[, max_interaction_ratio := NULL]
  }

  if (learner == "rpf_fixdepth") {
    tuning_archives[, max_interaction := 2]
  }

  if (startsWith(learner, "xgb")) {
    tuning_archives[, regr.xgboost.eta := exp(regr.xgboost.eta)]

    names_to_trim <- stringr::str_subset(names(tuning_archives), "xgb")
    data.table::setnames(tuning_archives,
                         old = names_to_trim,
                         new = stringr::str_remove(names_to_trim, "^regr\\.xgboost\\."))
  }

  if (learner == "xgb_fixdepth") tuning_archives[, max_depth := 2]

  if (learner == "ranger") {
    tuning_archives[, mtry := ceiling(mtry.ratio * p)]
    tuning_archives[, mtry.ratio := NULL]
  }

  save_obj(tuning_archives, name = "archives", postfix = learner)
  tictoc::toc()

  tictoc::tic("Extracting and saving tuning results")
  tuning_results <- try(extract_inner_tuning_results(bmr))
  if (inherits(tuning_results, "try-error")) {
    cli::cli_alert_danger("Failed to extract tuning results for {.val {learner}}, skipping")
    next
  }

  tuning_results <- merge(tuning_results, task_meta, by = "task_id")
  tuning_results[, rmse := sqrt(regr.mse)]
  tuning_results[, x_domain := NULL]
  tuning_results[, resampling_id := NULL]

  if (learner == "rpf") {
    tuning_results[, max_interaction := pmax(ceiling(max_interaction_ratio * pmin(p, 20)), 1)]
    tuning_results[, max_interaction_ratio := NULL]
  }

  if (learner == "rpf_fixdepth") {
    tuning_results[, max_interaction := 2]
  }

  if (startsWith(learner, "xgb")) {
    tuning_results[, regr.xgboost.eta := exp(regr.xgboost.eta)]

    names_to_trim <- stringr::str_subset(names(tuning_results), "regr.xgboost")
    data.table::setnames(
      tuning_results,
      old = names_to_trim,
      new = stringr::str_remove(names_to_trim, "^regr\\.xgboost\\.")
    )
  }
  if (learner == "xgb_fixdepth") tuning_results[, max_depth := 2]

  if (learner == "ranger") {
    tuning_results[, mtry := ceiling(mtry.ratio * p)]
    tuning_results[, mtry.ratio := NULL]
  }

  save_obj(tuning_results, name = "results", postfix = learner)
  tictoc::toc()

  tictoc::tic("Collecting garbage just in case")
  gc(reset = TRUE)
  rm(bmr, scores, aggr, tuning_archives, tuning_results)
  memory.mult = c(if (.Machine$sizeof.pointer == 4L) 28L else 56L, 8L)
  mem.used = sum(gc()[, 1L] * memory.mult)
  cli::cli_alert_info("Used {.val {prettyunits::pretty_bytes(mem.used)}}")
  tictoc::toc()
}

cli::cli_h2("Combining results")

tictoc::tic("Scores")
scores = fs::dir_ls(conf$result_path, glob = "*/scores_*.rds") |>
  lapply(readRDS) |>
  data.table::rbindlist()

save_obj(scores, name = "scores")
tictoc::toc()

tictoc::tic("Aggrs")
aggr = fs::dir_ls(conf$result_path, glob = "*/aggr_*.rds") |>
  lapply(readRDS) |>
  data.table::rbindlist()

save_obj(aggr, name = "aggr")
tictoc::toc()

cli::cli_alert_success("Done!")
tictoc::toc()
