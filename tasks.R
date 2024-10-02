library(mlr3)
library(mlr3oml)
requireNamespace("qs") # for caching

cli::cli_h2("Creating tasks")
# Gets the OpenML-CTR23 collection
# https://www.openml.org/search?type=study&study_type=task&sort=tasks_included&id=353
# https://openreview.net/pdf?id=HebAOoMm94
collection = ocl(353)
# Get the OML tasks as intermediate objects
omltasks = lapply(collection$task_ids, \(id) otsk(id))
# Derive mlr3 tasks and resamplings
tasks = lapply(omltasks, \(learner) as_task(learner))
names(tasks) = mlr3misc::ids(tasks)

# all_feature_types <- sapply(tasks, \(task) {
#   unique(task$feature_types$type)
# }) |>
#   unlist() |>
#   unique() |>
#   sort()

task_meta <- data.table::rbindlist(lapply(tasks, \(task) {

  x <- task$data(cols = task$feature_names)

  data.table::data.table(
    task_id = task$id,
    n = task$nrow,
    p = task$n_features,
    dim = task$nrow * task$n_features,
    dim_rank = 0,
    has_missings = any(task$missings() > 0),
    n_numeric   = sum(vapply(x, \(col) is.numeric(col),   logical(1))),
    n_integer   = sum(vapply(x, \(col) is.integer(col),   logical(1))),
    n_factor    = sum(vapply(x, \(col) is.factor(col),    logical(1))),
    n_character = sum(vapply(x, \(col) is.character(col), logical(1))),
    n_logical   = sum(vapply(x, \(col) is.logical(col),   logical(1)))
  )
}))


# Discard tasks with missing values etc.
tasks_exclude <- task_meta[
  (has_missings) | (n_logical > 0) | (n_character > 0) | dim > conf$task_dim_max
  , task_id]

cli::cli_alert_warning("Excluding the following tasks due to missings or unsupported features:")
cli::cli_ul(tasks_exclude)

tasks_keep_ids <- which(!names(tasks) %in% tasks_exclude)
tasks <- tasks[tasks_keep_ids]
task_meta <- task_meta[tasks_keep_ids]
task_meta[, dim_rank := rank(dim)]

task_meta[, task_label := sprintf("%s (%i x %i)", task_id, n, p)]
task_meta[, task_label := factor(task_label, levels = task_label, ordered = TRUE)]
task_meta[, task_id := factor(task_id, levels = task_id, ordered = TRUE)]

data.table::setorder(task_meta, dim_rank)

cli::cli_alert_info("{.val {length(tasks)}} tasks remaining")

# Resampling ----------------------------------------------------------------------------------
cli::cli_h2("Creating resamplings")

if (conf$resampling$outer$strategy == "OML") {
  cli::cli_alert_info("Using OML resamplings")
  resamplings = lapply(omltasks, \(task) as_resampling(task))
  # Discard resamplings for tasks with missing values
  resamplings <- resamplings[tasks_keep_ids]

} else {
  set.seed(conf$seed)

  cli::cli_alert_info("Creating {.val {conf$resampling$outer$strategy}} resamplings")

  resamplings = lapply(tasks, \(task) {
    # cli::cli_alert_info("Making resampling for {task$id}")
    resampling <- switch(
      conf$resampling$outer$strategy,
      "cv" = rsmp("cv", folds = conf$resampling$outer$folds),
      "repeated_cv" = rsmp("repeated_cv",
                           folds = conf$resampling$outer$folds,
                           repeats = conf$resampling$outer$repeats)
    )
    resampling$instantiate(task)

    resampling_dir <- here::here("resamplings", conf$resampling$outer$strategy)
    if (!fs::dir_exists(resampling_dir)) {
      fs::dir_create(resampling_dir, recurse = TRUE)
    }

    data.table::fwrite(
      x = as.data.table(resampling),
      file = fs::path(resampling_dir, task$id, ext = "csv")
    )

    resampling
  })
}

stopifnot(length(tasks) == length(resamplings))

saveRDS(task_meta, "task_meta.rds")
