library(mlr3oml)
requireNamespace("qs")
options(mlr3oml.cache = TRUE)


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

  task_meta <- data.table::data.table(
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

  task_meta
}))


# Discard tasks with missing values etc.
tasks_exclude <- task_meta[
  (has_missings) | (n_logical > 0) | (n_character > 0)
  , task_id]

cli::cli_alert_warning("Excluding the following tasks due to missings or unsupported features:")
cli::cli_ul(tasks_exclude)

tasks_keep_ids <- which(!names(tasks) %in% tasks_exclude)
tasks <- tasks[tasks_keep_ids]

cli::cli_alert_info("{.val {length(tasks)}} tasks remaining")

task_meta[, dim_rank := rank(dim)]


# Resampling ----------------------------------------------------------------------------------

if (conf$resampling$outer$strategy == "OML") {
  resamplings = lapply(omltasks, \(task) as_resampling(task))
  # Discard resamplings for tasks with missing values
  resamplings <- resamplings[tasks_keep_ids]

} else {
  set.seed(conf$seed)

  resamplings = lapply(tasks, \(task) {
    cli::cli_alert_info("Making resampling for {task$id}")
    switch(
      conf$resampling$outer$strategy,
      "cv" = rsmp("cv", folds = conf$resampling$outer$folds),
      "repeated_cv" = rsmp("repeated_cv",
                           folds = conf$resampling$outer$folds,
                           repeats = conf$resampling$outer$repeats),
    )
  })
}

stopifnot(length(tasks) == length(resamplings))
